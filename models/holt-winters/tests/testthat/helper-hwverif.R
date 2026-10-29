## helper-hwverif.R -- utilidades dos gates [PLUGINS] de honestidade do HW (HW.6). Auto-carregado por test_dir.
## Nao e codigo de produto. Tudo pelo leitor, pelo despachante da fundacao (P.0c) e pelo molde honesty_gate.R (M3.2/M3.5)
## sobre o deck sintetico (P.0e); o oraculo nunca e tocado. Sem variaveis de ambiente; datas por lubridate com tz explicito.

for (m in c("provenance", "honesty_gate"))
  if (!exists(paste0(if (m == "provenance") "prevcarga_freeze" else "prevcarga_honesty_gate"), envir = hw_lib, inherits = FALSE))
    sys.source(file.path(hw_root(), "scripts", "lib", paste0(m, ".R")), envir = hw_lib)

HV_O <- lubridate::ymd("2024-05-15")
HV_TOL <- 1e-5
HV_HIST <- 60L

hv_au <- function(hhmm) lubridate::ymd_hm(paste("2024-05-15", hhmm), tz = HW_TZ)
hv_ascii <- function(x) all(utf8ToInt(paste(x, collapse = "")) < 128L)

## deck com a carga do dia O ate `hhmm` (o available_until que o leitor deriva fica igual a ele)
hv_store <- function(hhmm = "09:00") hw_fx_store(load_until = hv_au(hhmm))

## store apontando para uma copia do deck (mutadores em copia do P.0e; a topologia nao e do deck)
hv_with_dir <- function(st, dir) {
  file.copy(file.path(st$dir, "topology.yaml"), file.path(dir, "topology.yaml"), overwrite = TRUE)
  modifyList(st, list(dir = dir))
}

hv_cuts <- function(st, rule = "available_until") suppressWarnings(hw_fx_cuts(st, rule = rule))
hv_panel <- function(st, cuts, mode = c("train", "infer")) hw_fx_panel(st, cuts, match.arg(mode), history_days = HV_HIST)

## fundacao no global so durante `code` (o plugin carregado pelo loader a procura la; interino como engine_abort)
hv_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_plugin_scan_pure", "prevcarga_delivery_grid")
  nm <- nm[!vapply(nm, exists, NA, envir = globalenv(), inherits = FALSE)]   # aninhado: so o mais externo instala/remove
  for (n in nm) assign(n, hw_lib[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  force(code)
}

hv_plugin <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- hv_with_cli(hw_lib$prevcarga_plugin_load("holt-winters", file.path(hw_root(), "models")))
    cache
  }
})

## spec do run.yaml com a janela de treino do fixture (60 dias => 59 linhas na CargaTempVer)
hv_spec <- function(plugin = hv_plugin(), train_days = 59L) { s <- plugin$spec; s$history_days$train <- train_days; s }

hv_ctx <- function(cuts) list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)

hv_train <- function(panel, cuts, spec = hv_spec(), plugin = hv_plugin())
  hv_with_cli(suppressWarnings(suppressMessages(hw_lib$prevcarga_plugin_train(plugin, panel, cuts, spec, hv_ctx(cuts)))))

hv_infer <- function(art, panel, cuts, plugin = hv_plugin())
  hv_with_cli(suppressWarnings(hw_lib$prevcarga_plugin_infer(plugin, art, panel, cuts, hv_ctx(cuts))))

hv_forecast <- function(panel, cuts, spec = hv_spec(), plugin = hv_plugin())
  hv_with_cli(suppressWarnings(suppressMessages(hw_lib$prevcarga_plugin_forecast(plugin, panel, cuts, spec, hv_ctx(cuts)))))

## erro classificado do despachante (NULL se nao houve); avisos do plugin ficam mudos
hv_err <- function(expr) hw_catch(suppressWarnings(suppressMessages(expr)))

## sha256 do artefato congelado (bytes de prevcarga_plugin_freeze_bytes)
hv_sha <- function(art) {
  f <- tempfile("hv-sha-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))))
  hw_lib$prevcarga_plugin_freeze_bytes(art, f)
}

expect_hv_error <- function(e, class, exit, id = NULL) {
  testthat::expect_s3_class(e, "prevcarga_error")
  testthat::expect_identical(e$class_name, class)
  testthat::expect_identical(e$exit_code, exit)
  testthat::expect_match(conditionMessage(e), sprintf("^\\[prevcarga/%s\\]", class))
  testthat::expect_true(hv_ascii(conditionMessage(e)))
  if (!is.null(id)) testthat::expect_identical(e$id, id)
  invisible(e)
}

## inicio (00:00 local) do dia `d` no fuso do deck
hv_day_start <- function(d) lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(d)), HW_TZ)

## Ponte molde M3.2 -> store (como em test-plugin-dispatch-honesty.R): o molde perturba o painel EM MEMORIA (y*1.3+1);
## a mesma regiao e perturbada no store (copia) com a mesma funcao.
hv_store_for_gate <- function(st, base, p) {
  ch <- which(abs(p$y_true - base$y_true) > 0)
  if (!length(ch)) return(st)
  s0 <- p$interval_start[ch]
  out <- hw_lib$prevcarga_fixture_perturb(st$dir, "load", after = min(s0) - lubridate::minutes(30L), at_or_before = max(s0))
  hv_with_dir(st, out)
}

## painel y_true do molde: a carga que o leitor entrega na janela de 10 dias ate O
hv_gate_panel <- function(panel) {
  ld <- panel$load[lubridate::as_date(interval_start, tz = HW_TZ) >= HV_O - 10L]
  data.frame(area_code = ld$area_code, interval_start = ld$interval_start, interval_end = ld$interval_end,
             y_true = ld$load_mwh, stringsAsFactors = FALSE)
}

## plugin no env de teste (estagios direto) e forecast acoplado do plugin (treina e infere no mesmo painel, dia O ate available_until)
hv_env <- local({ cache <- NULL; function() { if (is.null(cache)) cache <<- hw_env(); cache } })
hv_fc <- function(panel, cuts, spec = hv_spec()) suppressWarnings(suppressMessages(hv_env()$plugin_forecast(panel, cuts, spec)))

## Deck ragged de 2 areas: ZZ2 perde 09:00-24:00 do dia O => available_until global = 09:00 e ZZ1 tem carga alem do corte no store.
hv_ragged <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      dir <- tempfile("hv-rag-")
      hw_lib$prevcarga_fixture_deck(dir, 20241015L, c(HW_AREA, "ZZ2"), HV_O - 59L:0L, load_label = "start", tz = HW_TZ,
                                    horizon_days = 10L)
      d2 <- hw_lib$prevcarga_fixture_drop_slots(dir, "ZZ2", HV_O, "09:00", 30L)
      writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                   sprintf("  X: {areas: [%s, ZZ2], loss: PX}", HW_AREA), "order: [X]", "members: []"),
                 file.path(d2, "topology.yaml"))
      cache <<- list(dir = d2, O = HV_O, days = 60L, load_until = NULL)
    }
    cache
  }
})

## painel do HW lido pelo leitor nas 2 areas (o corte e o minimo das areas) e restrito a ZZ1
hv_ragged_panel <- function(st, cuts, mode = c("train", "infer")) {
  mode <- match.arg(mode)
  ctx <- hw_fx_ctx(st); O <- lubridate::as_date(cuts$origin_day); H <- cuts$horizon_days
  both <- c(HW_AREA, "ZZ2")
  p <- suppressWarnings(hw_lib$prevcarga_read_panel(ctx, O, both, horizon_days = H, history_days = HV_HIST))
  fc <- hw_lib$prevcarga_read_weather(ctx, "forecast", both, O, O + H)
  one <- function(x) x[x$area_code == HW_AREA]
  p$load <- one(p$load); p$weather_observed <- one(p$weather_observed); p$holidays <- one(p$holidays)
  p$weather_forecast <- one(fc[issue_time <= cuts$weather_issue_limit])
  data.table::setnames(p$weather_observed, "timestamp", "valid_time")
  if (mode == "train") {
    p$load <- p$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
    p$weather_observed <- p$weather_observed[lubridate::as_date(valid_time, tz = cuts$tz) <= cuts$last_data_day]
  }
  p
}

## Delta do molde (max|dy_pred| casado por posicao; diferenca estrutural = Inf)
hv_delta <- function(a, b) hw_lib$.prevcarga_hg_delta(a, b)

HV_HOL <- lubridate::ymd("2024-05-17")                          # sexta O+2: feriado util (tipo 12 como o O-5 do deck, que da historico ao SemiFer; simplified_code 1 => FER 1)

hv_holiday_store <- function() {
  st <- hv_store("09:00")
  d <- hw_lib$prevcarga_fixture_set_holiday(st$dir, HW_AREA, HV_HOL,
                                            c(holiday_type_id = 12, prevcarga_code = 12, simplified_code = 1))
  hv_with_dir(st, d)
}

hv_drop_temp <- function(v) rep(NA_real_, length(v))

