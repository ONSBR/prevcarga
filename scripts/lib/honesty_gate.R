## honesty_gate.R -- gate de honestidade falsificavel, IN-PROCESS (M3.2, reusavel)
##
## Responsabilidade: trocar a garantia de honestidade que o ISOLAMENTO DE PROCESSO dava na referencia
## (G5: arvore-sombra + system2) por invariante de MODULO falsificavel. Sem subprocesso, disco, env ou
## relogio [TBD: ADR-0029 -- escopo forecast_origin==origin e reach-control por saida].
##
## Molde: prevcarga_honesty_gate(run_fn, panel, cut, horizon, tol = 1e-5, perturb)
##   - panel: painel in-memory do leitor (interval_start, interval_end, y_true + colunas livres, nunca
##     perturbadas). So o canal y_true e perturbado.
##   - cut: instante no fuso da carga, recebido PRONTO; o gate nunca o calcula (nenhuma aritmetica de
##     data de negocio aqui). INFERENCIA: cut = available_until derivado do dado. TREINO: cut = fim de
##     last_data_day = O-1-data_lag_days do descriptor (cuts$last_data_day de prevcarga_resolve_cuts), igual
##     para origin_agnostic e origin_scoped (ADR-0023: nada do dia O em diante no treino).
##   - Bracos sobre copias profundas: CLEAN, CLEAN2 (determinismo), FORBIDDEN (y_true com
##     interval_end em (cut, cut + horizon dias], horizonte CHEIO, com controle-positivo de alcance
##     reach_ok) e CONTROL (y_true com interval_end em (cut - 1 dia, cut]).
##   - Delta = max|dy_pred| apos casar pelas chaves presentes; diferenca estrutural = Inf.
##   - Veredito em ordem fixa: CLEAN2>tol INDETERMINADO; FORBIDDEN>tol VAZA; !reach_ok INDETERMINADO;
##     CONTROL<=tol HARNESS QUEBRADO; senao HONESTO.
## O gate NAO aborta em VAZA: quem roda em runtime converte VAZA em honesty.leak (exit 15).
## Helpers internos .prevcarga_hg_* sao reusaveis pelo canal de clima (M3.5).
## Reusado por: combination (M4), training/inference (M5), cache (M7).

.prevcarga_hg_check <- function(run_fn, panel, cut, horizon, tol) {
  bad <- function(where) prevcarga_abort("contract.honesty_gate_args", where = where)
  if (!is.function(run_fn)) bad("run_fn")
  if (!is.data.frame(panel) ||
      !all(c("interval_start", "interval_end", "y_true") %in% names(panel))) bad("panel")
  if (!lubridate::is.POSIXct(cut) || length(cut) != 1L || is.na(cut)) bad("cut")
  if (!lubridate::is.POSIXct(panel$interval_end)) bad("panel")
  if (!identical(lubridate::tz(cut), lubridate::tz(panel$interval_end))) bad("cut (tzone)")
  if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) ||
      horizon < 1 || horizon != round(horizon)) bad("horizon")
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol <= 0) bad("tol")
  invisible(TRUE)
}

.prevcarga_hg_regions <- function(interval_end, cut, horizon) {
  list(forbidden = interval_end > cut & interval_end <= cut + lubridate::days(horizon),
       control   = interval_end > cut - lubridate::days(1) & interval_end <= cut)
}

.prevcarga_hg_copy <- function(p) {
  if (data.table::is.data.table(p)) return(data.table::copy(p))
  as.data.frame(lapply(p, identity), stringsAsFactors = FALSE, check.names = FALSE)
}

.prevcarga_hg_arm <- function(tbl, mask, perturb, cols = "y_true") {
  p <- .prevcarga_hg_copy(tbl)
  changed <- FALSE
  for (cl in cols) {
    old <- tbl[[cl]][mask]
    v <- p[[cl]]
    v[mask] <- perturb(old)
    p[[cl]] <- v
    changed <- changed || any(v[mask] != old, na.rm = TRUE)
  }
  list(panel = p, n_perturbed = sum(mask), changed = any(mask) && changed)
}

.prevcarga_hg_verdict <- function(clean2, forbidden, control, reach_ok, tol) {
  if (clean2 > tol)    return("INDETERMINADO")
  if (forbidden > tol) return("VAZA")
  if (!reach_ok)       return("INDETERMINADO")
  if (control <= tol)  return("HARNESS QUEBRADO")
  "HONESTO"
}

.prevcarga_hg_delta <- function(a, b) {
  if (!is.data.frame(a) || !is.data.frame(b) || !("y_pred" %in% names(a)) || !("y_pred" %in% names(b)))
    prevcarga_abort("contract.honesty_gate_args", where = "run_fn output")
  keys <- intersect(c("area", "forecast_origin", "target_datetime", "lead"), intersect(names(a), names(b)))
  if (nrow(a) != nrow(b)) return(Inf)
  if (!nrow(a)) return(0)
  keyvec <- function(d) {
    if (!length(keys)) return(as.character(seq_len(nrow(d))))
    cols <- lapply(keys, function(k) {
      v <- d[[k]]
      if (lubridate::is.POSIXct(v)) format(as.numeric(v), digits = 17) else as.character(v)
    })
    do.call(paste, c(cols, sep = "\r"))
  }
  ka <- keyvec(a); kb <- keyvec(b)
  oa <- order(ka, method = "radix"); ob <- order(kb, method = "radix")
  if (!identical(ka[oa], kb[ob])) return(Inf)
  ya <- as.numeric(a$y_pred)[oa]; yb <- as.numeric(b$y_pred)[ob]
  if (!identical(is.na(ya), is.na(yb))) return(Inf)
  max(c(0, abs(ya - yb)), na.rm = TRUE)
}

prevcarga_honesty_gate <- function(run_fn, panel, cut, horizon, tol = 1e-5,
                                   perturb = function(y) y * 1.30 + 1) {
  .prevcarga_hg_check(run_fn, panel, cut, horizon, tol)
  horizon <- as.integer(horizon)
  reg <- .prevcarga_hg_regions(panel$interval_end, cut, horizon)
  arm_f <- .prevcarga_hg_arm(panel, reg$forbidden, perturb, "y_true")
  arm_c <- .prevcarga_hg_arm(panel, reg$control, perturb, "y_true")
  base <- run_fn(.prevcarga_hg_copy(panel))
  c2   <- run_fn(.prevcarga_hg_copy(panel))
  fz   <- run_fn(arm_f$panel)
  ct   <- run_fn(arm_c$panel)
  clean2_delta    <- .prevcarga_hg_delta(base, c2)
  forbidden_delta <- .prevcarga_hg_delta(base, fz)
  control_delta   <- .prevcarga_hg_delta(base, ct)
  reach_ok <- isTRUE(arm_f$changed)
  list(
    verdict         = .prevcarga_hg_verdict(clean2_delta, forbidden_delta, control_delta, reach_ok, tol),
    clean2_delta    = clean2_delta,
    forbidden_delta = forbidden_delta,
    control_delta   = control_delta,
    reach_ok        = reach_ok,
    arms = list(
      forbidden = list(n_perturbed = as.integer(arm_f$n_perturbed),
                       region = sprintf("(cut, cut + %dd]", horizon)),
      control   = list(n_perturbed = as.integer(arm_c$n_perturbed), region = "(cut - 1d, cut]"),
      channel = "y_true", tol = tol, horizon = horizon))
}
