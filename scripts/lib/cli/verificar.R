## scripts/lib/cli/verificar.R - verbo auto-registrado `verificar` (M8.7; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Passthrough fino para prevcarga_verify_repro (verify_repro.R): so valida flag, resolve a referencia (--referencia ou --oficial)
## e compoe o result. `--tol` NAO existe mais: fica declarado so para o docopt aceitar o token e o handler emitir
## config.verify_tol_removed (exit 8) em vez de um config.usage generico; `--corte` e flag de escopo do backtest/plan
## (cli.R .prevcarga_check_scoped => config.usage). Sem env vars, sem relogio; erros e exits vem do catalogo (ADR-0040).
## Aviso (runtime_id diferente) sai por prevcarga_warn, coletado pela fachada no envelope. Texto devolve linhas ASCII.
## Requer errors.R, config.R (da fachada) e schema_long.R, provenance.R, evaluation.R, exports.R, verify_repro.R.

.prevcarga_vf_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.render_verificar_text <- function(r) {
  a <- r$arquivos
  c(sprintf("verificar  resultado=%s  trilho=%s  tol=%s (%s)", r$resultado, r$trilho, format(r$tol, scientific = FALSE), r$origem_tol),
    sprintf("saida:      %s", r$saida), sprintf("referencia: %s", r$referencia),
    if (nrow(a)) sprintf("arquivo %-32s pior_delta=%s n_fora=%d", a$path, format(a$pior_delta, digits = 6), a$n_fora),
    vapply(as.character(unlist(r$avisos)), function(w) paste("aviso:", w), ""),
    r$resultado)
}

cmd_verificar <- function(ctx, args) {
  if (!is.null(.prevcarga_vf_opt(args, "--tol")))
    prevcarga_abort("config.verify_tol_removed", where = "--tol")
  saida <- .prevcarga_vf_opt(args, "--saida")
  if (is.null(saida)) prevcarga_abort("config.usage", where = "--saida")
  if (!dir.exists(saida)) prevcarga_abort("state.artifact_missing", where = saida, details = list(reason = "--saida inexistente"))
  oficial <- isTRUE(args[["--oficial"]])
  ref <- .prevcarga_vf_opt(args, "--referencia")
  if (oficial == !is.null(ref))
    prevcarga_abort("config.usage", where = "--referencia", details = list(hint = "use --referencia=<dir> ou --oficial, nao ambos"))
  if (oficial) ref <- pc_vr_resolve_oficial(saida, ctx)
  else if (!dir.exists(ref)) prevcarga_abort("state.artifact_missing", where = ref, details = list(reason = "--referencia inexistente"))
  res <- prevcarga_verify_repro(saida, ref, trilho = .prevcarga_vf_opt(args, "--trilho"), alvo = .prevcarga_vf_opt(args, "--alvo"),
                                limiares = .prevcarga_vf_opt(args, "--limiares"), sem_membros = isTRUE(args[["--sem-membros"]]),
                                ctx = ctx)
  for (w in res$avisos) prevcarga_warn(w, where = saida)
  res$saida <- saida
  res$referencia <- ref
  res$oficial <- oficial
  if (identical(ctx$format, "json")) res else .render_verificar_text(res)
}

prevcarga_verb_verificar <- list(
  name    = "verificar",
  usage   = paste("prevcarga verificar --saida=<dir> (--referencia=<dir> | --oficial) [--trilho=<t>]",
                  "[--alvo=<nome> --limiares=<arq>] [--sem-membros]"),
  options = c(
    "--saida=<dir>           Entrega local do forecast (manifest da entrega, run_manifest e parquets)",
    "--referencia=<dir>      Entrega de referencia (mesmo layout)",
    "--oficial               Referencia = inferencia oficial sincronizada da mesma chave (origem, available_until, versao)",
    "--trilho=<t>            mesma-plataforma (1e-5 absoluto) ou entre-plataformas (0,1% relativo); default pela plataforma",
    "--alvo=<nome>           Alvo cujo limiar_mw (em --limiares) substitui a tolerancia do trilho",
    "--limiares=<arq>        YAML de limiares por alvo, assinado (sha256)",
    "--sem-membros           Ignora membros/ no pareamento",
    "--tol=<x>               REMOVIDO: sempre config.verify_tol_removed"),
  scoped  = c("--saida", "--referencia", "--oficial", "--trilho", "--alvo", "--limiares", "--sem-membros", "--tol"),
  handler = function(ctx, args) cmd_verificar(ctx, args),
  owner   = "M8",
  exempt  = FALSE,
  modules = c("schema_long.R", "provenance.R", "evaluation.R", "exports.R", "verify_repro.R")
)
