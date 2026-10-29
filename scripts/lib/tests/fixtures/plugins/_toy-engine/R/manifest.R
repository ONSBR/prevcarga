## Manifest do motor-brinquedo: ordem de carga, funcoes exigidas e dependencia entre arquivos.
engine_manifest <- function() list(
  contract_version = 1L,
  files = c("a.R", "b.R"),
  required_functions = c("engine_fit", "engine_predict"),
  packages = character(),
  file_requires = list(b.R = "engine_base"))
