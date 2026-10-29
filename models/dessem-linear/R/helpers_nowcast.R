## helpers_nowcast.R -- dessem-linear (DL.5): cabeca de nowcast `mm` do dia O (harmonizacao DESSEM 16b/16d).
## Dono: este plugin (o motor nao tem nowcast). So definicoes, sem estado, sem `<<-`, sem substituir funcao do motor.

## mm[d] = media das horas COMPLETAS (hourly_load e horario, hora incompleta ja descartada pelo to_hourly) cujo rotulo
## `interval_end` esta no frame de modelagem (o do despachante). A MESMA regra vale nos dias historicos e em O: a hora
## entra se `interval_end <= floor_hour(available_until)` no horario do dia (corte = hora do dia de available_until).
## Meia hora isolada nao forma hora completa, logo available_until 09:00 e 09:30 dao o mesmo mm.
## `origin_day` (opcional): dia O; se available_until cair no dia anterior a O no frame de modelagem, o corte vira
## negativo (nenhuma hora entra). Sem ele assume-se que available_until esta no dia de O.
dl_morning_means <- function(hourly_load, available_until, origin_day = NULL) {
  floor_cut <- lubridate::floor_date(available_until, "hour")
  d_cut <- lubridate::as_date(floor_cut)
  shift_days <- if (is.null(origin_day)) 0L else as.integer(d_cut - origin_day)
  cut_tod <- lubridate::hour(floor_cut) + 24L * shift_days
  h <- data.table::as.data.table(hourly_load)
  h <- h[, .(Data = lubridate::as_date(interval_end), tod = lubridate::hour(interval_end), Carga)]
  out <- h[tod <= cut_tod, .(mm = mean(Carga)), by = Data]
  data.table::setorder(out, Data)
  out[]
}

## mm normalizada pela constante de DM do motor (so quando a normalizacao de DM esta ligada).
dl_normalize_mm <- function(mm, config_mdl, constnorm) {
  out <- data.table::copy(data.table::as.data.table(mm))
  on <- any(config_mdl$normalizacao$dm$carga$flag, config_mdl$normalizacao$dm$exogena$flag)
  if (isTRUE(on) && !is.null(constnorm)) out[, mm := mm / constnorm]
  out[]
}

## Entrada do ajuste da DM(O): obterEntradaFormulaDM do motor + coluna `mm` + `+ mm` na formula (so a iteracao
## dataPrev == origin_day; as demais DM seguem sem mm, por isso esta funcao so existe para o dia O).
dl_dm_head_input <- function(engine, serieDM, config_mdl, mm, origin_day, require_origin = TRUE, constnorm = NULL) {
  if (!nrow(mm) || (isTRUE(require_origin) && !(origin_day %in% mm$Data)))
    dl_abort("plugin.dessem_linear_mm_missing_origin", where = "dessem-linear/dl_dm_head",
             reason = sprintf("mm sem a linha do dia %s", format(origin_day)),
             details = list(dia = format(origin_day), require_origin = require_origin))
  mmn <- dl_normalize_mm(mm, config_mdl, constnorm)
  inp <- engine$obterEntradaFormulaDM(format(origin_day), serieDM, config_mdl)
  inp$dadosEntrada$serieDiariaModelo <- merge(inp$dadosEntrada$serieDiariaModelo, mmn, by = "Data", all.x = TRUE)
  inp$formulaMDL <- stats::update(inp$formulaMDL, . ~ . + mm)
  ## a formula nao carrega o frame desta chamada (env efemero): duas chamadas iguais dao `identical()` e o modelo
  ## fica livre de ambiente (o freeze de DL.6 reconstroi `terms` com baseenv() de qualquer forma)
  environment(inp$formulaMDL) <- baseenv()
  inp
}

## Ajusta o SVM da DM(O) com os mesmos argumentos de SVM_DemandaMedia (tune_wsvm do ENV DO MOTOR, com as guardas).
## `seed` (opcional) fixa o RNG do CV antes do ajuste; NULL preserva a posicao do RNG do chamador (DL.6/DL.7).
## Devolve o best.model.
dl_dm_head <- function(engine, serieDM, config_mdl, mm, origin_day, require_origin = TRUE, seed = NULL,
                       constnorm = NULL) {
  inp <- dl_dm_head_input(engine, serieDM, config_mdl, mm, origin_day, require_origin, constnorm)
  tr <- engine$filtraDadosTreinamento(inp$dadosEntrada)
  if (!is.null(seed)) set.seed(as.integer(seed))
  engine$tune_wsvm(inp$formulaMDL, iter.max = 500, weight = engine$getSVMWeights(config_mdl$alfa, nrow(tr)),
                   data = tr, ranges = config_mdl$ranges, kernel = config_mdl$kernel)$best.model
}
