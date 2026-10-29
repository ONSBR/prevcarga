## Manifest do motor DESSEM unico (contrato lido por prevcarga_plugin_load via descriptor$engine).
## O loader avalia este arquivo primeiro, le o manifest e avalia `files` na ordem de dependencia.
## So define funcoes; nenhum estado.
engine_manifest <- function() list(
  engine_id = "_dessem-engine",
  contract_version = 1L,
  files = c("funcoes_apoio.R", "dados_entrada.R", "entrada_demanda_media.R",
            "entrada_perfil.R", "modelos_demanda_media.R", "modelos_perfil.R",
            "exec_modelos.R", "engine.R", "robustez.R", "feriados.R", "exogena.R",
            "svm_serial.R"),
  required_functions = c("TransformaDeck", "processaModelosDMPerfil",
                         "processaModelosDMTSPerfil", "execDemandaMedia", "execDemandaMediaTS",
                         "obterEntradaFormulaDM", "SVM_DemandaMedia", "SVMhorario", "RL_perfilFeriado",
                         "RD_DemandaMediaHyndman", "desagregaModeloSH", "getConfig",
                         "engine_svm_freeze", "engine_svm_thaw", "normalize_special_day_codes",
                         "fill_forecast_tail", "dessem_engine_guard"),
  ## o loader exige um vetor chr em `packages` (checado com requireNamespace); os pacotes de series
  ## temporais (so o regdin os usa) ficam em `packages_ts`, que o loader ignora.
  packages = c("WeightSVM", "e1071", "data.table", "lubridate", "jsonlite", "rlist"),
  packages_ts = c("fable", "fabletools", "feasts", "urca", "tsibble"),
  config_file = "model_parameters.json",
  file_requires = list(
    "entrada_demanda_media.R" = "aggregaSerie",
    "entrada_perfil.R" = "obtemLagsHora",
    "modelos_demanda_media.R" = "entradaDemandaMedia",
    "modelos_perfil.R" = "entradaPerfil",
    "exec_modelos.R" = "SVM_DemandaMedia"))
