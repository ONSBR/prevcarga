## helper-combination-cli.R -- fixtures do verbo `combine` e do prevcarga_combina_gate (M4.4). Auto-carregado por
## test_dir; nao e codigo de produto. Reusa pc_cmb_panel/pc_cmb_truth (helper-combination.R), pc_cmb_aligned_long
## (helper-combination-align.R), pc_cmb_write_run_manifest (helper-combination-io.R), pc_deck_sentinel (helper-deck.R).
## Tudo em tempdir; sem RNG (formulas fechadas); sem subprocesso.

## painel de qualidade (gate): A = y + e, B = y - e (erros complementares: 1/2 A + 1/2 B = y), C/D = y * (1 + 0.04 j),
## o membro curto S so ate O+1 (d_plus 0..1) e o melhor membro la (vies 0,1%). A combina vence em WMAPE por construcao.
pc_cmb_quality_panel <- function(origins = pc_cmb_lead_origins()) {
  p <- pc_cmb_panel(origins, d_plus = 0:3, members = c("A", "B", "C", "D", "S"), cuts = "0900")
  y <- p$y_true
  e <- 0.03 * sin(2 * pi * seq_along(y) / 7)
  p$f_A <- y + e
  p$f_B <- y - e
  p$f_C <- y * (1 + 0.04 * 3)
  p$f_D <- y * (1 + 0.04 * 4)
  p$f_S <- ifelse(p$d_plus >= 2L, NA_real_, y * 1.001)
  data.table::as.data.table(p)
}

## CONTROL: pesos otimos no treino (Jan: y = f_A) mas a relacao muda no holdout (y = media dos membros) => a combina
## perde para o EQW em WMAPE; a propriedade continua valendo (qualidade so reportada).
pc_cmb_loses_panel <- function(origins = pc_cmb_lead_origins()) {
  p <- pc_cmb_panel(origins, d_plus = 0:3, members = c("A", "B", "C"), cuts = "0900")
  y0 <- p$y_true
  k <- seq_along(y0)
  p$f_A <- y0 + 0.30 * sin(2 * pi * k / 7)
  p$f_B <- y0 + 0.30 * sin(2 * pi * k / 11 + 1)
  p$f_C <- y0 + 0.30 * sin(2 * pi * k / 13 + 2)
  p$y_true <- ifelse(p$ym == "2024-01", p$f_A, (p$f_A + p$f_B + p$f_C) / 3)
  data.table::as.data.table(p)
}

## pesos degenerados: nas celulas FITADAS, w = 1 no pior membro (presente na celula) e a = 0
pc_cmb_degenerate_weights <- function(weights, worst) {
  target <- paste0("f_", sub("^f_", "", worst))
  w <- data.table::copy(data.table::as.data.table(weights))
  ww <- w$w
  for (i in which(is.na(w$fallback))) {
    nm <- names(ww[[i]])
    ww[[i]] <- stats::setNames(as.numeric(nm == target), nm)
  }
  data.table::set(w, j = "w", value = list(ww))
  fit <- is.na(w$fallback)
  data.table::set(w, i = which(fit), j = "a", value = 0)
  data.table::set(w, i = which(fit), j = "nonstationary", value = FALSE)
  w[]
}

## root de runs `_infer_c0900` dos 8 membros alinhados (um run por origem; o membro curto ate O+1, os demais ate O+8),
## deck sentinela de rotulo `start` e `<tmp>/state`. Origens: janeiro inteiro, fim de fevereiro e 9-10/mar (3 meses).
pc_cmb_cli_origins <- function() c(format(seq(as.Date("2024-01-01"), as.Date("2024-01-31"), by = 1)),
                                   format(seq(as.Date("2024-02-25"), as.Date("2024-02-29"), by = 1)),
                                   "2024-03-09", "2024-03-10")

pc_cmb_cli_fixture <- function(origins = pc_cmb_cli_origins(), area = "SECO", cut = "0900",
                               dir = withr::local_tempdir(.local_envir = parent.frame())) {
  members <- pc_source_lib(c("errors", "topology"))$prevcarga_members(root = pc_root())
  root <- file.path(dir, "root")
  for (o in origins) for (j in seq_len(nrow(members))) {
    sg <- members$sigla[[j]]
    dd <- if (j == 1L) 2L else 9L
    long <- pc_cmb_aligned_long(sg, area, o, cut, dd, members)
    k <- as.integer(substr(gsub("-", "", o), 5L, 8L)) + seq_len(nrow(long))
    long$y_pred <- long$y_pred + 0.02 * j * sin(2 * pi * k / (5 + j) + j)
    exo <- if (is.na(members$exogena[[j]])) "" else paste0("_", members$exogena[[j]])
    run <- file.path(root, members$mdir[[j]], area, sprintf("%s_%s_infer%s_c%s", o, o, exo, cut))
    dir.create(run, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(long, file.path(run, "long.parquet"))
    au <- lubridate::ymd_hm(sprintf("%s %s:%s", o, substr(cut, 1L, 2L), substr(cut, 3L, 4L)), tz = pc_cmb_tz)
    pc_cmb_write_run_manifest(run, o, au, load_label = "start", delivery_days = dd)
  }
  deck <- pc_deck_sentinel("start", days = lubridate::ymd("2023-12-31") + 0:81, areas = area,
                           dir = file.path(dir, "deck"))
  state <- file.path(dir, "state")
  dir.create(state)
  list(root = root, data = deck, state = state, dir = dir, members = members, area = area, origins = origins)
}

## modulos de dominio que os handlers (verbo sourced com pai globalenv) esperam no globalenv: errors/runtime/config (a shim os carrega assim)
pc_cmb_global_base <- function() {
  if (!exists("prevcarga_abort", envir = globalenv(), inherits = FALSE))
    for (m in c("errors", "runtime", "config")) sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
  invisible(NULL)
}

## roda a fachada IN-PROCESS (sem subprocesso): prevcarga_main com o runtime real; captura stdout/stderr/exit
pc_cmb_run_cli <- function(argv) {
  pc_cmb_global_base()
  E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- E$prevcarga_main(argv, root = pc_root(), runtime = get("prevcarga_runtime_enforce", envir = globalenv()))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}

## os modulos do verbo no globalenv (como o dispatch da fachada faz); para chamar prevcarga_build_combine direto
pc_cmb_global_all <- function() {
  pc_cmb_global_base()
  for (m in c("topology", "schema_long", "provenance", "deck_reader", "dates", "evaluation", "features", "registry",
              "combination"))
    sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
  invisible(NULL)
}

## ambiente do verbo com `prevcarga_combina_load` substituido por um espiao (pai = env do espiao, avo = globalenv)
pc_cmb_spy_verb <- function() {
  pc_cmb_global_all()
  spy <- new.env(parent = globalenv())
  spy$calls <- 0L
  spy$prevcarga_combina_load <- function(...) {
    spy$calls <- spy$calls + 1L
    stop("espiao: prevcarga_combina_load acionado")
  }
  v <- new.env(parent = spy)
  sys.source(file.path(pc_root(), "scripts", "lib", "cli", "combine.R"), envir = v)
  list(env = v, spy = spy)
}
