## helper-rd3.R -- fixtures do RD.3 (infer com guarda de janela, refit do ARIMA com fingerprint, cabeca mm, forecast acoplado).
## Reusa o painel sintetico do RD.2 (14 meses, sem RNG) e acrescenta um feriado DENTRO do horizonte (O+2) para exercitar o
## RL_perfilFeriado recomputado. Cada treino custa ~1 min: os artefatos ficam em cache de sessao.

rd3_panel <- function(until = NULL, days = RD2_D) {
  pan <- rd_panel(origin = format(RD2_O), days = days, until = until)
  base <- rd2_panel(until = until)
  keep <- base$load$interval_start >= min(pan$load$interval_start)
  for (nm in c("load", "weather_observed")) {
    tc <- if (nm == "load") "interval_start" else "valid_time"
    base[[nm]] <- base[[nm]][get(tc) >= min(pan[[nm]][[tc]])]
  }
  base$holidays <- rbind(base$holidays,
    data.table::data.table(area_code = "SE", date = RD2_O + 2L, holiday_type_id = 1L, prevcarga_code = 3L, simplified_code = 3L))
  base
}

rd3_au <- function(hour, minute = 0L) rd2_au(hour) + lubridate::minutes(minute)

rd3_cuts <- function(au) {
  cu <- rd_cuts(available_until = au)
  cu$weather_issue_cutoff <- "available_until"
  cu
}

rd3_train <- function(p, exogena = "MED", pan = rd3_panel()) {
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_train(p, pan, rd3_cuts(rd3_au(9L)),
                                                            rd2_spec(exogena), ctx = rd2_ctx())))
}

## infer pelo despachante: painel de exibicao com a carga de O ate `au`
rd3_infer <- function(p, art, au, pan = NULL, perturb = NULL) {
  if (is.null(pan)) pan <- rd3_panel(until = au)
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]            # nenhuma emissao depois do limite do corte
  if (!is.null(perturb)) pan$load[perturb(interval_start), load_mwh := load_mwh + 500]
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_infer(p, art, pan, rd3_cuts(au))))
}

## forecast ACOPLADO do plugin (treina ate O-1 e infere numa passada, em memoria) sobre o MESMO painel do infer
rd3_forecast <- function(p, au, exogena = "MED", pan = NULL) {
  if (is.null(pan)) pan <- rd3_panel(until = au)
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  cu <- rd3_cuts(au)
  m <- rd_model_panel(pan, cu)
  rd_with_cli(suppressMessages(p$env$plugin_forecast(m$panel, m$cuts, rd2_spec(exogena))))
}

## estagios do plugin sobre um painel (para chamar plugin_infer direto, com o argumento de teste)
rd3_stage_inf <- function(p, art, pan, au) {
  cu <- rd3_cuts(au)
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  m <- rd_model_panel(pan, cu)
  frame <- rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  list(inf = rd_with_cli(suppressMessages(p$descriptor$stages$inference_features$fn(frame, art$payload))), cuts = m$cuts)
}

rd3_state <- local({
  s <- NULL
  function() {
    if (!is.null(s)) return(s)
    p <- rd2_plugin()
    s <<- list(p = p, eng = parent.env(p$env), med = rd3_train(p, "MED"), max = rd3_train(p, "MAX"))
    s
  }
})

## max|Delta y_pred| nos interval_end comuns (o corte 00:00 entrega mais intervalos que o das 09:00)
rd3_ymax <- function(a, b) {
  k <- intersect(as.numeric(a$delivery$interval_end), as.numeric(b$delivery$interval_end))
  ia <- as.numeric(a$delivery$interval_end) %in% k; ib <- as.numeric(b$delivery$interval_end) %in% k
  max(abs(a$delivery$y_pred[ia] - b$delivery$y_pred[ib]))
}

## posicional: o forecast acoplado e o plugin_infer direto entregam no frame de modelagem (sem o to_display_frame do despachante)
rd3_ypos <- function(a, b) { stopifnot(nrow(a$delivery) == nrow(b$delivery)); max(abs(a$delivery$y_pred - b$delivery$y_pred)) }
