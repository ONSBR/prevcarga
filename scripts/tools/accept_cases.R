#!/usr/bin/env Rscript
# =============================================================================
# accept_cases.R -- configuracao e expansao de unidades do aceite em Docker local (spec RegDin Linear Refactor, E8-T2).
# Dev-only, roda no R do HOST, fora do bundle. Nao toca o pacote, nao chama docker, nao le variavel de ambiente.
#
# Uso:
#   Rscript --vanilla accept_cases.R --config accept_docker.yaml --stage aceite|medicao --out units.tsv
#   Rscript --vanilla accept_cases.R --config accept_docker.yaml --settings
#   Rscript --vanilla accept_cases.R --overlay topologia|janela --in <yaml extraido da imagem> --out <yaml>
#
# YAML (schema_version 1; unicas chaves aceitas): ref_rev, jobs (12-14), memory, cpus, min_free_gb_docker, min_free_gb_out,
#   aceite {origem, subsistema, areas_feriado, area_tempo, membro_tempo, [origem_tempo]}, medicao {origens, areas, mdirs}. origem_tempo
#   (opcional, default = origem) e a origem das unidades de tempo (train-models, infer-model, deck-inference).
#   Precedencia dos ajustes: flag do accept_docker.sh > YAML > default embutido. Origem e AAAA-MM-DD (lubridate, UTC);
#   cada area e folha ou subsistema de config/topology.yaml do repo.
#
# units.tsv (TAB, cabecalho, ordem radix, bytes identicos entre execucoes), uma linha por container de unidade:
#   unit           id estavel: <stage>-<versao>-<verbo>-<area>-<origem>[-<mdir>]; infer-model termina em -train|-inference
#   stage          aceite | medicao
#   wave           1..3 (a fila so abre a onda k+1 depois da onda k)
#   versao         ref (v0.1.5, ref_rev) | new (HEAD)
#   origem         AAAA-MM-DD;  area  folha ou subsistema;  mdir  '-' = todos os membros ('<mdir>/<EXOGENA>' no infer-model)
#   verbo          backtest | train-models | infer-model | deck-inference
#   topologia      full | sem_lin_uni (overlay de config/topology.yaml sem o membro LIN_UNI, so na nova)
#   janela         '-' | referencia (overlay de params.janela_dessem nos run.yaml dos DESSEM, so na nova)
#   data_flag      --data (ref) | --data-train | --data-inference (nova)
#   artifacts_from unit cujo state/artifacts e montado :ro em /artifacts, ou '-'
# --settings imprime k=v (jobs, memory, cpus, min_free_gb_docker, min_free_gb_out, ref_rev, overlay_mdirs) para o bash.
#
# Exits (ids em scripts/lib/errors/accept.tsv): 0 ok; 2 contract.accept_bad_config (YAML invalido, chave desconhecida, origem
# fora de AAAA-MM-DD ou area fora da topologia; nenhum arquivo de saida e gravado) | contract.accept_bad_args (uso). Em erro,
# stdout traz o envelope JSON (error.id, error_id) e stderr a mensagem em ASCII.
# =============================================================================

pc_accept_unit_cols <- c("unit", "stage", "wave", "versao", "origem", "area", "mdir", "verbo",
                         "topologia", "janela", "data_flag", "artifacts_from")

## defaults embutidos (ultimo da precedencia flag > YAML > default)
pc_accept_defaults <- list(ref_rev = "de24021", jobs = 13L, memory = "8g", cpus = 1, min_free_gb_docker = 50L, min_free_gb_out = 20L)

.pc_acc_bad <- function(where, reason) prevcarga_abort("contract.accept_bad_config", where = where, details = list(reason = reason))
.pc_acc_chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.pc_acc_chrv <- function(x) is.character(x) && length(x) >= 1L && !anyNA(x) && all(nzchar(x))
.pc_acc_int1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)

.pc_acc_date <- function(x, where) {
  ok <- .pc_acc_chr1(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  d <- if (ok) suppressWarnings(lubridate::ymd(x, tz = "UTC", quiet = TRUE)) else NA
  if (!ok || is.na(d) || !identical(format(d, "%Y-%m-%d"), x)) .pc_acc_bad(where, "origem fora de AAAA-MM-DD")
  x
}

.pc_acc_block <- function(b, keys, where, optional = character()) {
  if (!is.list(b) || is.null(names(b))) .pc_acc_bad(where, "bloco ausente ou invalido")
  for (k in setdiff(names(b), c(keys, optional))) .pc_acc_bad(paste0(where, ".", k), "chave desconhecida")
  for (k in setdiff(keys, names(b))) .pc_acc_bad(paste0(where, ".", k), "chave obrigatoria ausente")
  b
}

## le e valida o YAML do aceite. `root` = raiz do repo (config/topology.yaml e models/). Devolve a lista validada.
pc_accept_config_load <- function(path, root) {
  if (!.pc_acc_chr1(path) || !file.exists(path)) .pc_acc_bad("config", "arquivo ausente")
  raw <- tryCatch(yaml::read_yaml(path), error = function(e) .pc_acc_bad(path, "YAML ilegivel"))
  if (!is.list(raw) || is.null(names(raw))) .pc_acc_bad(path, "raiz nao e um mapa")
  top <- c("schema_version", "ref_rev", "jobs", "memory", "cpus", "min_free_gb_docker", "min_free_gb_out", "aceite", "medicao")
  for (k in setdiff(names(raw), top)) .pc_acc_bad(k, "chave desconhecida")
  for (k in c("schema_version", "aceite", "medicao")) if (is.null(raw[[k]])) .pc_acc_bad(k, "chave obrigatoria ausente")
  if (!.pc_acc_int1(raw$schema_version) || raw$schema_version != 1) .pc_acc_bad("schema_version", "deve ser 1")
  cfg <- pc_accept_defaults
  if (!is.null(raw$ref_rev)) {
    if (!.pc_acc_chr1(raw$ref_rev) || !grepl("^[0-9a-f]{7,40}$", raw$ref_rev)) .pc_acc_bad("ref_rev", "hash git curto ou longo")
    cfg$ref_rev <- raw$ref_rev
  }
  if (!is.null(raw$jobs)) {
    if (!.pc_acc_int1(raw$jobs) || raw$jobs < 12 || raw$jobs > 14) .pc_acc_bad("jobs", "inteiro entre 12 e 14")
    cfg$jobs <- as.integer(raw$jobs)
  }
  if (!is.null(raw$memory)) {
    if (!.pc_acc_chr1(raw$memory) || !grepl("^[0-9]+[mg]$", raw$memory)) .pc_acc_bad("memory", "formato <n>g ou <n>m")
    cfg$memory <- raw$memory
  }
  if (!is.null(raw$cpus)) {
    if (!is.numeric(raw$cpus) || length(raw$cpus) != 1L || !is.finite(raw$cpus) || raw$cpus <= 0) .pc_acc_bad("cpus", "numero positivo")
    cfg$cpus <- raw$cpus
  }
  for (k in c("min_free_gb_docker", "min_free_gb_out")) if (!is.null(raw[[k]])) {
    if (!.pc_acc_int1(raw[[k]]) || raw[[k]] < 0) .pc_acc_bad(k, "inteiro >= 0")
    cfg[[k]] <- as.integer(raw[[k]])
  }
  topo <- prevcarga_topology(root = root)
  members <- prevcarga_members(root = root)
  areas_ok <- c(topo$leaves, names(topo$subsystems))
  chk_area <- function(a, where) if (!.pc_acc_chr1(a) || !(a %in% areas_ok)) .pc_acc_bad(where, "area fora de config/topology.yaml")
  chk_areas <- function(v, where) {
    if (!.pc_acc_chrv(v)) .pc_acc_bad(where, "lista de areas (texto) esperada; atencao: N sem aspas vira FALSE no YAML 1.1")
    for (a in v) chk_area(a, where)
    v
  }
  ac <- .pc_acc_block(raw$aceite, c("origem", "subsistema", "areas_feriado", "area_tempo", "membro_tempo"), "aceite", optional = "origem_tempo")
  .pc_acc_date(ac$origem, "aceite.origem")
  if (!is.null(ac$origem_tempo)) .pc_acc_date(ac$origem_tempo, "aceite.origem_tempo")
  if (!.pc_acc_chr1(ac$subsistema) || !(ac$subsistema %in% names(topo$subsystems))) .pc_acc_bad("aceite.subsistema", "subsistema fora de config/topology.yaml")
  chk_areas(ac$areas_feriado, "aceite.areas_feriado")
  chk_area(ac$area_tempo, "aceite.area_tempo")
  mt <- strsplit(if (.pc_acc_chr1(ac$membro_tempo)) ac$membro_tempo else "", "/", fixed = TRUE)[[1L]]
  if (length(mt) != 2L || !(mt[[1L]] %in% members$mdir)) .pc_acc_bad("aceite.membro_tempo", "formato <mdir>/<EXOGENA> com mdir da topologia")
  me <- .pc_acc_block(raw$medicao, c("origens", "areas", "mdirs"), "medicao")
  if (!.pc_acc_chrv(me$origens)) .pc_acc_bad("medicao.origens", "lista de datas AAAA-MM-DD esperada")
  for (o in me$origens) .pc_acc_date(o, "medicao.origens")
  if (anyDuplicated(me$origens)) .pc_acc_bad("medicao.origens", "origem repetida")
  chk_areas(me$areas, "medicao.areas")
  if (anyDuplicated(me$areas)) .pc_acc_bad("medicao.areas", "area repetida")
  if (!.pc_acc_chrv(me$mdirs) || !all(me$mdirs %in% members$mdir)) .pc_acc_bad("medicao.mdirs", "mdir fora dos membros da topologia")
  cfg$aceite <- list(origem = ac$origem, origem_tempo = if (is.null(ac$origem_tempo)) ac$origem else ac$origem_tempo, subsistema = ac$subsistema,
                     areas_feriado = ac$areas_feriado, area_tempo = ac$area_tempo, membro_tempo = ac$membro_tempo)
  cfg$medicao <- list(origens = me$origens, areas = me$areas, mdirs = me$mdirs)
  cfg
}

.pc_acc_flag <- function(versao) if (versao == "ref") "--data" else "--data-train"

.pc_acc_rows <- function(...) data.table::rbindlist(list(...), use.names = TRUE)

pc_accept_units_medicao <- function(cfg) {
  g <- data.table::CJ(versao = c("ref", "new"), origem = cfg$medicao$origens, area = cfg$medicao$areas,
                      mdir = cfg$medicao$mdirs, sorted = TRUE)
  g[, `:=`(stage = "medicao", wave = 1L, verbo = "backtest", topologia = "full", janela = "-",
           data_flag = data.table::fifelse(versao == "ref", "--data", "--data-train"), artifacts_from = "-")]
  g[, unit := sprintf("medicao-%s-backtest-%s-%s-%s", versao, area, origem, mdir)]
  data.table::setorderv(g, c("stage", "wave", "versao", "origem", "area", "mdir", "unit"))
  stopifnot(nrow(g) == length(cfg$medicao$origens) * length(cfg$medicao$areas) * length(cfg$medicao$mdirs) * 2L)
  g[, pc_accept_unit_cols, with = FALSE]
}

pc_accept_units_aceite <- function(cfg) {
  a <- cfg$aceite; o <- a$origem; ot <- a$origem_tempo; mt <- strsplit(a$membro_tempo, "/", fixed = TRUE)[[1L]]
  ## `origem` = a da celula; as unidades de TEMPO (train-models, infer-model e deck-inference) usam origem_tempo (default = origem): a ref nao
  ## treina o dessem-linear numa origem com feriado de amostra pequena no horizonte (generic.unexpected herdado), e o tempo precisa dos dois lados
  row <- function(versao, verbo, area, wave, mdir = "-", suffix = mdir, topologia = "full", janela = "-", data_flag = NULL,
                  artifacts_from = "-", origem = o) {
    data.table::data.table(
      unit = paste0(c("aceite", versao, verbo, area, origem, if (!identical(suffix, "-")) suffix), collapse = "-"),
      stage = "aceite", wave = as.integer(wave), versao = versao, origem = origem, area = area, mdir = mdir, verbo = verbo,
      topologia = topologia, janela = janela, data_flag = if (is.null(data_flag)) .pc_acc_flag(versao) else data_flag,
      artifacts_from = artifacts_from)
  }
  tr_ref <- row("ref", "train-models", a$area_tempo, 1L, mt[[1L]], origem = ot)$unit
  tr_new <- row("new", "train-models", a$area_tempo, 1L, mt[[1L]], origem = ot)$unit
  u <- .pc_acc_rows(
    row("ref", "backtest", a$subsistema, 1L),
    row("new", "backtest", a$subsistema, 1L, topologia = "sem_lin_uni", janela = "referencia"),
    do.call(rbind, lapply(a$areas_feriado, function(ar) row("new", "backtest", ar, 1L, mdir = mt[[1L]]))),
    row("ref", "train-models", a$area_tempo, 1L, mt[[1L]], origem = ot),
    row("new", "train-models", a$area_tempo, 1L, mt[[1L]], origem = ot),
    row("ref", "infer-model", a$area_tempo, 2L, a$membro_tempo, suffix = "train", artifacts_from = tr_ref, origem = ot),
    row("new", "infer-model", a$area_tempo, 2L, a$membro_tempo, suffix = "train", artifacts_from = tr_new, origem = ot),
    row("new", "deck-inference", a$area_tempo, 2L, artifacts_from = tr_new, origem = ot),
    row("new", "infer-model", a$area_tempo, 3L, a$membro_tempo, suffix = "inference", data_flag = "--data-inference",
        artifacts_from = tr_new, origem = ot))
  data.table::setorderv(u, c("stage", "wave", "versao", "origem", "area", "mdir", "verbo", "unit"))
  u[, pc_accept_unit_cols, with = FALSE]
}

pc_accept_units <- function(cfg, stage) {
  if (identical(stage, "aceite")) pc_accept_units_aceite(cfg)
  else if (identical(stage, "medicao")) pc_accept_units_medicao(cfg)
  else prevcarga_abort("contract.accept_bad_args", where = "--stage", details = list(reason = "aceite ou medicao"))
}

pc_accept_settings_lines <- function(cfg) {
  c(sprintf("jobs=%d", cfg$jobs), sprintf("memory=%s", cfg$memory), sprintf("cpus=%s", format(cfg$cpus)),
    sprintf("min_free_gb_docker=%d", cfg$min_free_gb_docker), sprintf("min_free_gb_out=%d", cfg$min_free_gb_out),
    sprintf("ref_rev=%s", cfg$ref_rev), sprintf("overlay_mdirs=%s", paste(cfg$medicao$mdirs, collapse = ",")))
}

## escrita atomica e deterministica (TAB, LF, sem aspas)
pc_accept_write_tsv <- function(dt, path) {
  tmp <- paste0(path, ".tmp")
  data.table::fwrite(dt, tmp, sep = "\t", quote = FALSE, eol = "\n", na = "-")
  if (!isTRUE(file.rename(tmp, path))) { unlink(tmp); prevcarga_abort("contract.accept_bad_args", where = path) }
  invisible(path)
}

.pc_acc_write_yaml <- function(x, path) {
  txt <- yaml::as.yaml(x, handlers = list(logical = yaml::verbatim_logical))
  if (any(utf8ToInt(txt) > 127L)) prevcarga_abort("contract.accept_bad_args", where = path, details = list(reason = "nao-ASCII"))
  writeLines(txt, path)
  invisible(path)
}

## overlay somente leitura, a partir do arquivo extraido da imagem: kind "topologia" tira o membro LIN_UNI; "janela" fixa
## params.janela_dessem = referencia. Nada alem disso muda.
pc_accept_overlay <- function(kind, input, output) {
  if (!file.exists(input)) prevcarga_abort("contract.accept_bad_args", where = input, details = list(reason = "--in ausente"))
  x <- yaml::read_yaml(input)
  if (!is.list(x)) prevcarga_abort("contract.accept_bad_args", where = input, details = list(reason = "YAML ilegivel"))
  if (identical(kind, "topologia")) {
    keep <- vapply(x$members, function(m) !identical(m$sigla, "LIN_UNI"), NA)
    x$members <- x$members[keep]
  } else if (identical(kind, "janela")) {
    if (is.null(x$params)) x$params <- list()
    x$params$janela_dessem <- "referencia"
  } else prevcarga_abort("contract.accept_bad_args", where = "--overlay", details = list(reason = "topologia ou janela"))
  .pc_acc_write_yaml(x, output)
}

.pc_acc_opt <- function(args, key) { i <- which(args == key); if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else NULL }

pc_accept_cases_main <- function(args, root) {
  need <- function(key) { v <- .pc_acc_opt(args, key); if (is.null(v)) prevcarga_abort("contract.accept_bad_args", where = key); v }
  if (!is.null(.pc_acc_opt(args, "--overlay"))) {
    pc_accept_overlay(need("--overlay"), need("--in"), need("--out"))
    return(list(overlay = need("--overlay"), out = need("--out")))
  }
  cfg <- pc_accept_config_load(need("--config"), root)
  if ("--settings" %in% args) {
    writeLines(pc_accept_settings_lines(cfg), stdout())
    return(list(settings = TRUE))
  }
  stage <- need("--stage"); out <- need("--out")
  u <- pc_accept_units(cfg, stage)
  pc_accept_write_tsv(u, out)
  list(stage = stage, units = nrow(u), out = out)
}

if (sys.nframe() == 0L) {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  lib <- file.path(dirname(dirname(normalizePath(fa[[1L]]))), "lib")
  options(prevcarga.lib = lib)
  if (file.exists(file.path(lib, "runtime.R"))) source(file.path(lib, "runtime.R"))
  if (exists("prevcarga_runtime_fix", mode = "function")) prevcarga_runtime_fix()
  source(file.path(lib, "errors.R")); source(file.path(lib, "topology.R"))
  out <- prevcarga_with_errors(pc_accept_cases_main(commandArgs(trailingOnly = TRUE), dirname(dirname(lib))))
  if (is.null(out$cond)) quit(save = "no", status = 0L)
  env <- prevcarga_envelope(verb = "accept_cases", cond = out$cond)
  env$error_id <- out$cond$id
  writeLines(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA)), stdout())
  writeLines(prevcarga_render_error_text(out$cond), stderr())
  quit(save = "no", status = as.integer(out$cond$exit_code))
}
