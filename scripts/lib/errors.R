## errors.R -- contrato unico de erros da CLI (ADR-0040, M1.4b)
##
## Unico criador deste arquivo e o ticket M1.4b; extensoes aditivas: M2.5 e M3.4 (argumento opcional `result` em prevcarga_condition/prevcarga_abort, repassado ao envelope de falha). Epicos seguintes declaram seus
## error.id criando scripts/lib/errors/<nome>.tsv (barrel por descoberta) e apenas LEEM este arquivo.
## Saida de terminal/catalogo: ASCII puro, sem acento. Sem variaveis de ambiente.

## --- Tabela unica classe -> exit (ADR-0040). So cresce no fim, nunca renumera. ----------------
prevcarga_exit_codes <- c(ok = 0L, generic = 1L, contract = 2L, completude = 3L, topology = 4L,
  window = 5L, bundle = 6L, runtime = 7L, config = 8L, integrity = 9L, network = 10L,
  coherence = 11L, repro = 12L, state = 13L, partial = 14L, honesty = 15L)

prevcarga_exit_precedence <- function(classes) {
  classes <- unique(as.character(classes))
  if (!length(classes) || !all(classes %in% names(prevcarga_exit_codes)))
    stop("errors.R: classe fora da tabela de exit-codes (bug do pacote): ",
         paste(setdiff(classes, names(prevcarga_exit_codes)), collapse = ", "), call. = FALSE)
  head_order <- c("runtime", "config", "contract", "completude", "topology", "window")
  hit <- intersect(head_order, classes)
  if (length(hit)) return(hit[[1L]])
  rest <- setdiff(classes, head_order)
  rest[order(prevcarga_exit_codes[rest])][[1L]]
}

## --- Localizacao do diretorio lib (capturada no source) ----------------------------------------
.prevcarga_errors_state <- new.env(parent = emptyenv())
.prevcarga_errors_state$catalog <- NULL
.prevcarga_errors_state$lib <- local({
  of <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.character(of) && length(of) == 1L && file.exists(of))
    dirname(normalizePath(of, winslash = "/"))
  else NULL
})

prevcarga_errors_dir <- function() {
  lib <- getOption("prevcarga.lib", .prevcarga_errors_state$lib)
  if (is.null(lib)) lib <- file.path("scripts", "lib")
  file.path(lib, "errors")
}

## --- Tabela interna minima (autoprotecao do loader: aborta antes de o catalogo existir) ---------
.prevcarga_builtin_errors <- data.frame(
  id = c("contract.error_catalog_dup", "contract.error_catalog_schema", "generic.unknown_error_id"),
  class = c("contract", "contract", "generic"),
  message = c("error.id declarado em mais de um arquivo do catalogo",
              "arquivo do catalogo de erros invalido",
              "error.id inexistente no catalogo (bug do pacote)"),
  hint = c("declare cada error.id em um unico scripts/lib/errors/<nome>.tsv",
           "corrija scripts/lib/errors/*.tsv: colunas id/class/message/hint, classe da tabela, ASCII puro",
           "declare o id em scripts/lib/errors/<nome>.tsv"),
  stringsAsFactors = FALSE)

## --- Loader do catalogo distribuido ---------------------------------------------------------------
prevcarga_catalog_assert_ascii <- function(cat, where = NULL) {
  for (col in intersect(c("id", "message", "hint"), names(cat))) {
    bad <- vapply(cat[[col]], function(s) any(as.integer(charToRaw(enc2utf8(s))) > 127L), NA)
    if (any(bad))
      prevcarga_abort("contract.error_catalog_schema", where = where,
        details = list(reason = "nao-ascii", column = col, ids = unname(cat$id[bad])))
  }
  invisible(cat)
}

prevcarga_errors_load <- function(dir = prevcarga_errors_dir(), refresh = FALSE) {
  files <- list.files(dir, pattern = "\\.tsv$", full.names = TRUE)
  files <- files[order(basename(files), method = "radix")]
  want <- c("id", "class", "message", "hint")
  parts <- lapply(files, function(f) {
    df <- tryCatch(
      utils::read.delim(f, sep = "\t", quote = "", comment.char = "", encoding = "UTF-8",
                        colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL)
    if (is.null(df) || !all(want %in% names(df)))
      prevcarga_abort("contract.error_catalog_schema", where = basename(f),
        details = list(reason = "colunas", expected = want))
    df <- df[, want, drop = FALSE]
    df[is.na(df)] <- ""
    if (nrow(df)) df$file <- basename(f) else df$file <- character()
    df
  })
  all <- if (length(parts)) do.call(rbind, parts) else
    data.frame(id = character(), class = character(), message = character(), hint = character(),
               file = character(), stringsAsFactors = FALSE)
  rownames(all) <- NULL
  prevcarga_catalog_assert_ascii(all)
  badid <- !grepl("^[a-z]+\\.[a-z_]+$", all$id)
  if (any(badid))
    prevcarga_abort("contract.error_catalog_schema", where = paste(unique(all$file[badid]), collapse = ","),
      details = list(reason = "id", ids = all$id[badid]))
  badcl <- !(all$class %in% names(prevcarga_exit_codes)[-1L])
  if (any(badcl))
    prevcarga_abort("contract.error_catalog_schema", where = paste(unique(all$file[badcl]), collapse = ","),
      details = list(reason = "classe", ids = all$id[badcl], classes = all$class[badcl]))
  dup <- unique(all$id[duplicated(all$id)])
  if (length(dup)) {
    id <- dup[[1L]]
    fs <- all$file[all$id == id]
    prevcarga_abort("contract.error_catalog_dup", where = paste(fs, collapse = ","),
      details = list(id = id, files = fs))
  }
  out <- all[, want, drop = FALSE]
  out <- out[order(out$id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

prevcarga_error_catalog <- function(refresh = FALSE) {
  if (isTRUE(refresh) || is.null(.prevcarga_errors_state$catalog))
    .prevcarga_errors_state$catalog <- prevcarga_errors_load()
  .prevcarga_errors_state$catalog
}

prevcarga_error_lookup <- function(id) {
  b <- .prevcarga_builtin_errors
  if (id %in% b$id) return(as.list(b[b$id == id, , drop = FALSE][1L, ]))
  cat <- prevcarga_error_catalog()
  row <- cat[cat$id == id, , drop = FALSE]
  if (!nrow(row))
    prevcarga_abort("generic.unknown_error_id", details = list(requested = id))
  as.list(row[1L, ])
}

## --- Condicoes classificadas -----------------------------------------------------------------------
prevcarga_condition <- function(id, where = NULL, details = NULL, type = "prevcarga_error", result = NULL) {
  row <- prevcarga_error_lookup(id)
  structure(
    class = c(type, if (type == "prevcarga_error") "error" else "warning", "condition"),
    list(message = sprintf("[prevcarga/%s] %s", row$class, row$message),
         call = NULL, id = row$id, class_name = row$class,
         exit_code = unname(prevcarga_exit_codes[[row$class]]),
         where = where, hint = row$hint, details = details, result = result))
}

## `result` (opcional) viaja na condicao e sai em `result` do envelope de falha (ex.: findings do check-coherence).
prevcarga_abort <- function(id, where = NULL, details = NULL, result = NULL) {
  stop(prevcarga_condition(id, where = where, details = details, result = result))
}

prevcarga_warn <- function(id, where = NULL, details = NULL) {
  warning(prevcarga_condition(id, where = where, details = details, type = "prevcarga_warning"))
}

prevcarga_collect_errors <- function() {
  conds <- list()
  list(
    add = function(id, where = NULL, details = NULL) {
      conds[[length(conds) + 1L]] <<- prevcarga_condition(id, where = where, details = details)
      invisible(NULL)
    },
    has = function() length(conds) > 0L,
    finish = function() {
      if (!length(conds)) return(invisible(NULL))
      classes <- vapply(conds, function(c) c$class_name, "")
      win <- prevcarga_exit_precedence(classes)
      i <- which(classes == win)[[1L]]
      others <- lapply(conds[-i], function(c) list(id = c$id, class = c$class_name, where = c$where))
      d <- c(conds[[i]]$details, list(other_errors = others))
      prevcarga_abort(conds[[i]]$id, where = conds[[i]]$where, details = d)
    })
}

## --- Render ----------------------------------------------------------------------------------------
prevcarga_cond_warnings <- function(cond) {
  oth <- if (!is.null(cond)) cond$details$other_errors else NULL
  lapply(oth, function(o) list(id = o$id, message = prevcarga_error_lookup(o$id)$message))
}

prevcarga_envelope <- function(verb = NULL, result = NULL, cond = NULL, warnings = list(),
                               run_id = NULL, version = NULL) {
  err <- if (is.null(cond)) NULL else
    list(class = cond$class_name, id = cond$id,
         message = sub("^\\[prevcarga/[a-z]+\\] ", "", conditionMessage(cond)),
         where = cond$where, hint = cond$hint, details = cond$details)
  list(ok = is.null(cond), verb = verb,
       exit_code = if (is.null(cond)) 0L else as.integer(cond$exit_code),
       error = err, warnings = warnings, result = result, run_id = run_id, version = version)
}

prevcarga_render_error_text <- function(cond) {
  c(sprintf("prevcarga: erro [%s] (exit %d)", cond$class_name, cond$exit_code),
    paste0("o que: ", sub("^\\[prevcarga/[a-z]+\\] ", "", conditionMessage(cond))),
    paste0("onde: ", if (is.null(cond$where)) "-" else cond$where),
    paste0("como corrigir: ", cond$hint))
}

prevcarga_render_warning_text <- function(w) {
  sprintf("prevcarga: aviso [%s] %s", w$id, w$message)
}

prevcarga_render_error <- function(cond, format = c("text", "json"), verb = NULL, warnings = NULL,
                                   run_id = NULL, version = NULL) {
  format <- match.arg(format)
  if (is.null(warnings)) warnings <- prevcarga_cond_warnings(cond)
  if (format == "text")
    return(c(prevcarga_render_error_text(cond),
             vapply(warnings, prevcarga_render_warning_text, "")))
  env <- prevcarga_envelope(verb = verb, result = cond$result, cond = cond, warnings = warnings, run_id = run_id,
                            version = version)
  as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA))
}

## --- Handler de topo reutilizavel (quem sai e a fachada) ----------------------------------------------
prevcarga_format_calls <- function(calls) {
  vapply(calls, function(x) paste(deparse(x, nlines = 1L), collapse = ""), "")
}

prevcarga_with_errors <- function(expr, debug = FALSE) {
  calls <- NULL
  out <- tryCatch(
    withCallingHandlers(list(exit_code = 0L, cond = NULL, value = force(expr)),
      error = function(e) calls <<- sys.calls()),
    prevcarga_error = function(e) list(exit_code = e$exit_code, cond = e, value = NULL),
    error = function(e) {
      cond <- tryCatch(prevcarga_abort("generic.unexpected",
        details = list(original = conditionMessage(e))), prevcarga_error = identity)
      list(exit_code = 1L, cond = cond, value = NULL)
    })
  if (isTRUE(debug) && !is.null(out$cond) && length(calls))
    writeLines(prevcarga_format_calls(calls), con = stderr())
  out
}

## --- Catalogo consolidado para os utilitarios .sh/.ps1 (gerado, nunca versionado) ----------------------
prevcarga_export_error_catalog <- function(path, dir = prevcarga_errors_dir()) {
  cat <- prevcarga_errors_load(dir = dir)
  cat$exit_code <- unname(prevcarga_exit_codes[cat$class])
  cat <- cat[order(cat$id, method = "radix"), c("id", "class", "exit_code", "message", "hint")]
  lines <- c(paste(names(cat), collapse = "\t"),
             if (nrow(cat)) do.call(paste, c(unname(as.list(cat)), sep = "\t")))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), path)
  invisible(path)
}
