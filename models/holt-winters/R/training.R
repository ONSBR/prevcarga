## training.R -- estagio de treino do plugin Holt-Winters (HW.3). SO definicoes. Reescreve train_hw_model do oraculo
## prevcargadessem3/models/holt-winters/R/training/hw_training.R (somente leitura) como plugin_train(features, spec).
## Classe origin_scoped (ADR-0023): 1 treino por origem O com a serie ate O-1; o dia O nunca entra. 1 HW multiplicativo da
## media diaria + 48 HW por intervalo (frequency 7), h_fit = horizon_days + 1 FIXO (passo 1 = dia O, 2..9 = O+1..O+8; o
## ajuste do forecast::hw nao depende de h). Nenhum objeto forecast/ets sobrevive: so os `$mean` (dado puro). Sem hora, sem
## relogio, sem ambiente, sem caminho, sem saveRDS (persistencia e do M5). Datas so por lubridate. Saida: ASCII puro.

## Confere (nunca corta/preenche) as pre-condicoes do treino origin_scoped. Devolve os metadados da serie.
## Substitui o align_first_idx data-driven do oraculo (hw_training.R:365-377): o validate-deck ja reprova buraco, logo a
## cauda curta e erro e align_first_idx fica carimbado em 2L.
.hw_check_series <- function(features, spec) {
  cuts <- features$meta; at <- "holt-winters/training"
  if (NROW(features$load_day_o) > 0L)
    hw_abort("hw.train_day_o", sprintf("%d intervalo(s) do dia O no treino", NROW(features$load_day_o)),
             list(n = NROW(features$load_day_o)), where = at)
  if (!identical(cuts$train_origin, cuts$origin_day))
    hw_abort("hw.bad_cut", "train_origin diferente de origin_day (origin_scoped: train_origin = O, sem shift)",
             list(field = "train_origin"), where = at)
  ctv <- features$CargaTempVer
  fim <- max(ctv$Data)
  if (!identical(fim, cuts$last_data_day))
    hw_abort("hw.last_day_incomplete", sprintf("datafimver %s != last_data_day %s", format(fim), format(cuts$last_data_day)),
             list(datafimver = format(fim), last_data_day = format(cuts$last_data_day)), where = at)
  need <- spec$history_days$train
  if (!is.null(need) && nrow(ctv) < need)
    hw_abort("hw.history_short", sprintf("%d dias de historico < history_days.train %d", nrow(ctv), as.integer(need)),
             list(have = nrow(ctv), need = as.integer(need)), where = at)
  list(datainiver = min(ctv$Data), datafimver = fim, align_first_idx = 2L, h_fit = spec$h_fit)
}

## Inicio do ts() diario: (ano, dia do ano) de datainiver, so por lubridate (sem format()).
.hw_ts_start <- function(d) c(as.integer(lubridate::year(d)), as.integer(lubridate::yday(d)))
.hw_ts <- function(x, datainiver)
  stats::ts(x, start = .hw_ts_start(datainiver), frequency = plugin_constants()$ts_frequency)

## Unico ponto que chama forecast::hw (costura dos espioes dos testes).
.hw_hw <- function(y, h) forecast::hw(y, h = h, seasonal = "multiplicative")

## Media diaria (CargaTempMed[,4]; PrevisaoDiaria.R:710-714): $mean (h_fit) e model_var; o objeto forecast e descartado.
.hw_fit_daily <- function(ctm, datainiver, h_fit) {
  fit <- .hw_hw(.hw_ts(ctm[, 4L], datainiver), h_fit)
  list(mean = as.numeric(fit$mean), model_var = stats::sd(fit$x - fit$fitted))
}

## 48 ajustes independentes (colunas 4..51 = H01..H48; hw_training.R:445-465); matriz h_fit x 48.
.hw_fit_interval <- function(ctv, datainiver, h_fit) {
  m <- matrix(NA_real_, nrow = h_fit, ncol = 48L, dimnames = list(NULL, hw_slot_cols()))
  for (j in seq_len(48L)) {
    fit <- .hw_hw(.hw_ts(ctv[, j + 3L], datainiver), h_fit)
    m[, j] <- as.numeric(fit$mean)
  }
  m
}

## Snapshots que o infer le (SemiFer e efeitoTemp): arqCarga do SemiFer = CargaTempVerFer com Data >= piso (2019-01-01).
.hw_semifer_snapshots <- function(features) {
  fer <- features$CargaTempVerFer
  list(carga_temp_ver_fer = fer[fer$Data >= plugin_constants()$semifer_floor, , drop = FALSE],
       carga_temp_med1 = features$CargaTempMed1, carga_temp_ver = features$CargaTempVer)
}

## `spec` = list(params, seed, horizon_days, history_days) (modelo spec do run.yaml) OU, na forma do despachante P.0c,
## so `params` com `seed` no 3o argumento (horizon_days cai em delivery_days - 1 do descriptor; a janela de treino ja foi
## aplicada pelo leitor, logo a guarda history_days.train so age quando o spec completo e dado).
.hw_train_spec <- function(spec, seed) {
  full <- is.list(spec[["params"]])
  params <- if (full) spec[["params"]] else spec
  sd <- if (full) spec[["seed"]] else seed
  if (length(sd) != 1L || is.na(sd) || !is.numeric(sd))
    hw_abort("hw.bad_param", "seed ausente (nenhum default)", list(param = "seed"), where = "holt-winters/training")
  hz <- if (full && !is.null(spec[["horizon_days"]])) as.integer(spec[["horizon_days"]])
        else plugin_descriptor()$delivery_days - 1L
  list(params = params, seed = as.integer(sd), horizon_days = hz, h_fit = hz + 1L,
       history_days = if (full) spec[["history_days"]] else NULL)
}

## features (plugin_build_features), spec -> modelo em memoria. O FE nao roda de novo aqui; o despachante fixa/restaura o RNG.
plugin_train <- function(features, spec, seed = NULL) {
  sp <- .hw_train_spec(spec, seed)
  s  <- .hw_check_series(features, sp)
  day <- .hw_fit_daily(features$CargaTempMed, s$datainiver, s$h_fit)
  mi  <- .hw_fit_interval(features$CargaTempVer, s$datainiver, s$h_fit)
  if (anyNA(mi) || any(!is.finite(mi)) || anyNA(day$mean) || any(!is.finite(day$mean)))
    hw_abort("hw.non_finite", "previsao nao finita no ajuste", list(field = "mean_interval"), where = "holt-winters/training")
  c(list(model_id = "holt-winters", origin_day = features$meta$origin_day, train_origin = features$meta$train_origin,
         mean_interval = mi, mean_daily = day$mean, model_var = day$model_var,
         datainiver = s$datainiver, datafimver = s$datafimver, align_first_idx = s$align_first_idx,
         h_fit = s$h_fit, n_steps_fit = s$h_fit - 1L, m_mes = as.integer(features$m_mes),
         params = sp$params, seed = sp$seed),
    .hw_semifer_snapshots(features))
}
