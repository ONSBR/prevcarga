## svm_serial.R -- serializador de SVM do WeightSVM como dado puro (contrato de freeze da fundacao:
## prevcarga_plugin_freeze_bytes/prevcarga_plugin_scan_pure em scripts/lib/registry.R). Sem environment,
## closure, externalptr nem `call`; `terms` guardado como texto. Sem relogio nem host: duas congelacoes do
## mesmo modelo geram os mesmos bytes. Consumido pelos plugins dessem (dessem-linear, dessem-regdin).
.engine_svm_classes <- function() c("wsvm.formula", "wsvm", "svm")
.engine_svm_required <- function() c("SV", "coefs", "rho", "x.scale", "kernel", "terms_text")

.engine_svm_is_model <- function(x) inherits(x, "wsvm") || inherits(x, "svm")
.engine_svm_is_frozen <- function(x) is.list(x) && !is.null(x[["svm_class"]]) && !is.null(x[["terms_text"]])

## model (wsvm.formula, ou objeto `tune` => best.model, ou lista de modelos, p.ex. 24 perfis) -> dado puro.
engine_svm_freeze <- function(model) {
  if (inherits(model, "tune") && !is.null(model$best.model)) return(engine_svm_freeze(model$best.model))
  if (!.engine_svm_is_model(model)) {
    if (is.list(model) && !is.data.frame(model)) {
      out <- lapply(model, engine_svm_freeze)
      attributes(out) <- attributes(model)
      return(out)
    }
    return(model)
  }
  tt <- model$terms
  x <- unclass(model)
  x$call <- NULL; x$fitted <- NULL; x$residuals <- NULL; x$decision.values <- NULL
  x$terms <- NULL
  x$terms_text <- paste(deparse(stats::formula(tt), width.cutoff = 500L), collapse = "")
  x$term_labels <- attr(tt, "term.labels")
  x$data_classes <- attr(tt, "dataClasses")
  x$terms_intercept <- attr(tt, "intercept")        # wsvm zera o intercepto do terms; predict usa model.matrix
  x$svm_class <- class(model)
  x
}

## dado puro -> modelo com predict identico. Campo faltando ou classe desconhecida aborta [prevcarga/integrity].
engine_svm_thaw <- function(x) {
  if (!.engine_svm_is_frozen(x)) {
    if (is.list(x) && !is.data.frame(x)) {
      out <- lapply(x, engine_svm_thaw)
      attributes(out) <- attributes(x)
      return(out)
    }
    return(x)
  }
  fail <- function(why, ...) engine_abort("engine.dessem_svm_thaw_failed", where = "_dessem-engine/engine_svm_thaw",
                                          details = list(code = "engine.dessem.svm_thaw_failed", reason = why, ...))
  miss <- setdiff(.engine_svm_required(), names(x))
  if (length(miss)) fail(paste0("campo ausente: ", paste(miss, collapse = ",")), missing = miss)
  if (!is.character(x$svm_class) || !all(x$svm_class %in% .engine_svm_classes()))
    fail("classe desconhecida", svm_class = as.character(x$svm_class))
  tt <- tryCatch(stats::terms(stats::as.formula(x$terms_text, env = baseenv())), error = function(e) NULL)
  if (is.null(tt)) fail("terms_text invalido")
  if (!is.null(x$data_classes)) attr(tt, "dataClasses") <- x$data_classes
  if (!is.null(x$terms_intercept)) attr(tt, "intercept") <- x$terms_intercept
  x$terms <- tt
  keep <- setdiff(names(x), c("terms_text", "term_labels", "data_classes", "terms_intercept", "svm_class"))
  structure(x[keep], class = x$svm_class)
}
