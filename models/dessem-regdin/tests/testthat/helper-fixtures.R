## helper-fixtures.R -- fixtures compartilhadas dos testes de verificacao do dessem-regdin (RD.4; reusadas pelo RD.5).
## Auto-carregado por test_dir. Nao e codigo de produto. Camada fina sobre os helpers do RD.1-RD.3 (painel sintetico de 14
## meses sem RNG, loader real, motor real, despachante real da fundacao): os treinos custam ~1,5 min e ficam em cache de
## sessao. Os paineis entregues ao despachante sao SEMPRE crus (o despachante reamostra e desloca o frame de modelagem).
## Sem variaveis de ambiente; datas por lubridate com tz explicito.

## cuts do despachante: origin_day/train_origin = O (origin_scoped), last_data_day = O-1, available_until no fuso do
## painel (00:00, 09:00, 09:30, 12:00 de O) e a regra de emissao de clima derivada dele.
rd_fx_cuts <- function(O = RD2_O, available_until = rd3_au(9L), rule = "available_until") {
  cu <- rd_cuts(origin = format(O), available_until = available_until)
  cu$weather_issue_cutoff <- rule
  cu
}

## painel de exibicao cru: historico ate O-1 (+ carga de O ate `until`); clima previsto so ate o limite de emissao
rd_fx_panel <- function(until = NULL, days = RD2_D) {
  pan <- rd3_panel(until = until, days = days)
  if (!is.null(until)) pan$weather_forecast <- pan$weather_forecast[issue_time <= until]
  pan
}

## o mesmo painel com `days` dias a menos de historia (o fim fica em O-1)
rd_fx_shorten <- function(until = NULL, days = 30L) rd_fx_panel(until = until, days = RD2_D - as.integer(days))

## spec do despachante: o run.yaml real do plugin (rd2_spec) com sobrescritas por modifyList (NULL remove o campo)
rd_fx_spec <- function(exogena = "MED", overrides = list()) utils::modifyList(rd2_spec(exogena), overrides)

## raiz de modelos descartavel (plugin + motor) com o run.yaml real mutado, para o loader real
rd_fx_root <- function(edit = identity) rd_fixture_root(edit)

rd_fx_plugin <- function() rd2_plugin()

rd_train <- function(pan = rd_fx_panel(), cuts = rd_fx_cuts(), spec = rd_fx_spec())
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_train(rd_fx_plugin(), pan, cuts, spec, ctx = rd2_ctx())))

rd_infer <- function(art, pan = NULL, cuts = rd_fx_cuts()) {
  if (is.null(pan)) pan <- rd_fx_panel(until = cuts$available_until)
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_infer(rd_fx_plugin(), art, pan, cuts)))
}

## forecast ACOPLADO (treina ate O-1 e infere numa passada, em memoria) sobre o MESMO painel do infer
rd_forecast <- function(pan = NULL, cuts = rd_fx_cuts(), spec = rd_fx_spec()) {
  if (is.null(pan)) pan <- rd_fx_panel(until = cuts$available_until)
  m <- rd_model_panel(pan, cuts)
  rd_with_cli(suppressMessages(rd_fx_plugin()$env$plugin_forecast(m$panel, m$cuts, spec)))
}

## espiao de contagem em uma funcao do ENV DO PLUGIN (o que o estagio resolve por nome); restaurado em withr::defer do
## chamador (ou por $restore())
rd_spy <- function(fn_name, env = rd_fx_plugin()$env, frame = parent.frame()) {
  real <- get(fn_name, envir = env, inherits = FALSE)
  n <- 0L
  assign(fn_name, function(...) { n <<- n + 1L; real(...) }, envir = env)
  restore <- function() assign(fn_name, real, envir = env)
  withr::defer(restore(), envir = frame)
  list(count = function() n, restore = restore)
}

## max|Delta y_pred| posicional (mesma grade de entrega) entre dois envelopes do despachante
rd_fx_ydelta <- function(a, b) {
  stopifnot(nrow(a$delivery) == nrow(b$delivery))
  max(abs(a$delivery$y_pred - b$delivery$y_pred))
}

## recebe a funcao do gate do molde (pc_gate_tier) so quando o runner a carregou; fora do runner o braco de paridade pula
## com o rotulo [tier] (nunca roda sem golden)
rd_gate_tier <- function(min) {
  if (exists("pc_gate_tier", mode = "function")) return(pc_gate_tier(min))
  testthat::skip(sprintf("[tier] gate so roda no tier %s (sem runner de gates)", min))
}

## mensagem de terminal so ASCII (sem acento)
rd_ascii <- function(x) !any(grepl("[^\\x01-\\x7F]", x, perl = TRUE, useBytes = TRUE))
