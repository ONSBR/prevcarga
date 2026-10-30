## helper-porte-closure.R -- fixtures do fechamento do porte (M5.11; AC38..AC41). Auto-carregado por testthat::test_dir.
## Nao e codigo de produto. Importa o helper-plugins.R da fundacao (pc_grep_code & cia.), nunca o edita.
## Sem variaveis de ambiente (a topologia-fixture vai por um config.yaml em tempdir; o filho recebe o caminho como
## argumento); tudo em tempdir; datas por lubridate com tz explicito. Linux e Windows (sem symlink, sem fork).
##
## Os cenarios pesados (treino/inferencia pelas fachadas) rodam num `Rscript --vanilla` FILHO e limpo
## (`pcc_run_child`): as fachadas e o interino do loader (`globalenv()`) exigem a lib no ambiente global, que o
## processo do testthat nao deve sujar. As funcoes `pcc_child_*` abaixo sao as que o filho executa; nao tocam testthat.

PCC_TZ <- "Etc/GMT+3"
PCC_O <- lubridate::ymd("2024-10-15")
PCC_SEED <- 20241015L
PCC_DECK_DAYS <- 460L
PCC_CUTOFF <- "origin_midnight_utc"           # a emissao 00Z de O cobre o dia O inteiro (o do available_until nao cobre 00-03h)
PCC_FOUNDATION <- c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                    "registry", "features", "training", "inference")

## ---- caminhos e escopos -----------------------------------------------------------------------------------------
pcc_models_dir <- function(root = pc_root()) file.path(root, "models")

## Os plugins vem da topologia (nunca lista fixa): `mdir` distintos dos membros (o motor `_dessem-engine` nao e plugin).
pcc_all_plugins <- function(root = pc_root(), topology_path = file.path(root, "config", "topology.yaml"))
  unique(.pcc_members(root, topology_path)$mdir)

.pcc_members <- function(root, topology_path) {
  e <- new.env(parent = globalenv())
  for (m in c("errors", "config", "topology")) sys.source(file.path(root, "scripts", "lib", paste0(m, ".R")), envir = e)
  e$prevcarga_members(path = topology_path, root = root)
}

## Copia de config/ em tempdir com UMA edicao na topologia; devolve o caminho do config.yaml copiado (o que se passa por
## --config; o topology.yaml mora ao lado). `edit`: "add_member" (RGD_MXMN: dessem-regdin + MXMN) | "bad_mdir" (RF -> nao-existe).
pcc_topology_fixture <- function(edit = c("none", "add_member", "bad_mdir"), root = pc_root(),
                                 .local_envir = parent.frame()) {
  edit <- match.arg(edit)
  d <- withr::local_tempdir(pattern = "pcc-config-", .local_envir = .local_envir)
  file.copy(Sys.glob(file.path(root, "config", "*")), d, recursive = TRUE)
  tp <- file.path(d, "topology.yaml")
  txt <- readLines(tp, warn = FALSE)
  if (edit == "add_member")
    txt <- c(txt, "  - {sigla: RGD_MXMN, mdir: dessem-regdin, exogena: MXMN}")
  if (edit == "bad_mdir")
    txt <- sub("mdir: random-forest", "mdir: nao-existe", txt, fixed = TRUE)
  writeLines(txt, tp)
  file.path(d, "config.yaml")
}

## Arquivos varridos por escopo (R ordenados por radix; `run_yaml` = models/*/run.yaml). Nunca *.md.
pcc_scope_files <- function(models_dir, scope = c("plugins+engine", "engine", "run_yaml")) {
  scope <- match.arg(scope)
  f <- switch(scope,
    "plugins+engine" = Sys.glob(file.path(models_dir, "*", "R", "*.R")),
    engine = Sys.glob(file.path(models_dir, "_dessem-engine", "R", "*.R")),
    run_yaml = Sys.glob(file.path(models_dir, "*", "run.yaml")))
  sort(f, method = "radix")
}

## Copia de models/ so com o que a higiene varre (R/, run.yaml, README.md); sem models/*/models/ (artefatos) nem tests/.
pcc_models_copy <- function(root = pc_root(), .local_envir = parent.frame()) {
  d <- withr::local_tempdir(pattern = "pcc-models-", .local_envir = .local_envir)
  src <- pcc_models_dir(root)
  for (m in list.dirs(src, full.names = FALSE, recursive = FALSE)) {
    dir.create(file.path(d, m), recursive = TRUE)
    if (dir.exists(file.path(src, m, "R"))) file.copy(file.path(src, m, "R"), file.path(d, m), recursive = TRUE)
    one <- file.path(src, m, c("run.yaml", "README.md"))
    one <- one[file.exists(one)]
    if (length(one)) file.copy(one, file.path(d, m))
  }
  d
}

## Acrescenta 1 linha ao fim de `rel` (relativo a `copy`); devolve a linha plantada (numero) e o caminho.
pcc_plant <- function(copy, rel, line) {
  f <- file.path(copy, rel)
  n <- length(readLines(f, warn = FALSE))
  con <- file(f, "ab")
  writeLines(enc2utf8(line), con, useBytes = TRUE)
  close(con)
  list(file = f, line = n + 1L)
}

## ---- cenarios do filho (rodam em Rscript --vanilla; nunca no processo do testthat) ---------------------------------
## Lib da fundacao no ambiente global (como a CLI): o interino dos plugins le `prevcarga_abort` & cia. do globalenv.
pcc_child_lib <- function(root) {
  options(prevcarga.lib = file.path(root, "scripts", "lib"))
  g <- globalenv()
  for (m in PCC_FOUNDATION) sys.source(file.path(root, "scripts", "lib", paste0(m, ".R")), envir = g)
  sys.source(file.path(root, "scripts", "tools", "fixture_store.R"), envir = g)
  prevcarga_runtime_enforce()
  invisible(TRUE)
}

## Overrides rapidos de TESTE por plugin (chaves declaradas do spec efetivo: history_days.*, params.*). A janela e a menor
## em que cada plugin treina sobre o deck sintetico; o DESSEM exige history_days.infer == train (constraint do run.yaml).
## dessem-regdin: abaixo de ~400 dias o ARIMA com dummies de mes e lag de 7 dias nao ajusta no fixture (verossimilhanca nao-finita);
## o deck tem 460 dias para cobri-lo.
pcc_fast_params <- function(mdir) {
  switch(mdir,
    "random-forest" = list("history_days.train" = 40L, "params.ntree" = 5L),
    lgbm = list("history_days.train" = 115L),
    "holt-winters" = list("history_days.train" = 40L),
    "dessem-linear" = list("history_days.train" = 40L, "history_days.infer" = 40L),
    "dessem-regdin" = list("history_days.train" = 400L, "history_days.infer" = 400L),
    list())
}

## Area da celula: o LGBM so tem `heat_area` para areas reais (SP); os demais treinam na ZZ1 do fixture.
pcc_area_of <- function(mdir) if (identical(mdir, "lgbm")) "SP" else "ZZ1"

## Deck sintetico (P.0e): SP e ZZ1, 460 dias terminando em O, available_until O 09:30. O feriado futuro do fixture (O+3,
## codigo 3) nao tem amostra historica e o motor DESSEM o recusaria: vira o codigo 12 (nao-modelado => dia normal).
pcc_child_deck <- function(dir) {
  au <- lubridate::ymd_hm("2024-10-15 09:30", tz = PCC_TZ)
  prevcarga_fixture_deck(dir, seed = PCC_SEED, areas = c("SP", "ZZ1"), days = seq(PCC_O - (PCC_DECK_DAYS - 1L), PCC_O, by = "day"),
                         load_until = au, tz = PCC_TZ)
  dir <- pcc_day_effects(dir)
  ## O+3 (codigo 3, sem amostra historica no fixture) vira dia normal (codigo 12 = nao-modelado).
  for (a in c("SP", "ZZ1"))
    dir <- prevcarga_fixture_set_holiday(dir, a, PCC_O + 3L, c(holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = 1L))
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [SP, ZZ1], loss: PX}",
               "order: [X]", "members: []"), file.path(dir, "topology.yaml"))
  dir
}

## Efeito dia-a-dia deterministico (sem RNG) sobre o deck: sem ele a serie do fixture e quase periodica e o ARIMA do
## dessem-regdin (MAX) nao ajusta (modelo nulo). A temperatura ganha uma componente por dia e a carga a segue (+40 MW/grau)
## mais uma componente propria; ambas quase-aleatorias (fase em d^2): sinais de baixa dimensao (senos em d) tornam o lag de 7
## dias do ARIMA colinear com a propria exogena. O previsto recebe o mesmo efeito (por valid_time), como o observado. Usa o reescritor
## de particoes do fixture_store (so em copia: o deck de entrada nunca e escrito).
pcc_day_effects <- function(dir) {
  eff <- function(t) {
    d <- as.numeric(as.Date(t))
    list(temp = 2 * sin(d * d * 0.37) + 1.5 * sin(d * d * 0.113 + 1), load = 60 * sin(d * d * 0.173) + 40 * sin(d * d * 0.271 + 2))
  }
  out <- prevcarga_fixture_copy(dir)
  hit_all <- function(x) rep(TRUE, nrow(x))
  .pc_fx_rewrite_hits(out, "load", NULL, hit_all, function(x, i) {
    e <- eff(x$timestamp[i])
    data.table::set(x, i = i, j = "load_mwh", value = x$load_mwh[i] + e$load + 40 * e$temp)
  })
  .pc_fx_rewrite_hits(out, "weather_observed", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$timestamp[i])$temp))
  .pc_fx_rewrite_hits(out, "weather_forecast", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$valid_time[i])$temp))
  out
}

pcc_child_ctx <- function(root, deck, state, mdir, extra = list()) {
  dir.create(file.path(state, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  ov <- utils::modifyList(pcc_fast_params(mdir), extra)
  list(root = root, data_dir = deck, topology_path = file.path(deck, "topology.yaml"), weather_issue_cutoff = PCC_CUTOFF,
       origin = PCC_O, state_dir = state, code_commit = "c0ffee", renv_lock_hash = "ab12", spec_overrides = ov)
}

pcc_snapshot <- function() {
  g <- globalenv()
  list(globalenv = sort(ls(g, all.names = TRUE)), search = search(),
       seed = if (exists(".Random.seed", envir = g, inherits = FALSE)) get(".Random.seed", envir = g, inherits = FALSE) else NULL)
}

pcc_iso <- function(x, tz = PCC_TZ) if (!length(x)) character() else format(x, "%Y-%m-%d %H:%M:%S", tz = tz)

## Primeira linha da topologia para cada `mdir` (a variante e a do membro).
pcc_member_of <- function(root, mdir, topology_path = file.path(root, "config", "topology.yaml")) {
  mem <- prevcarga_members(path = topology_path, root = root)
  mem[mem$mdir == mdir, , drop = FALSE][1L, ]
}

## 1 treino pela fachada num state_dir proprio; devolve a linha da celula.
pcc_train_once <- function(mdir, ctx, member = NULL, root = ctx$root) {
  if (is.null(member)) member <- pcc_member_of(root, mdir)
  suppressWarnings(suppressMessages(prevcarga_train(member, pcc_area_of(member$mdir), PCC_O, ctx)))
}

pcc_infer_once <- function(member, ctx)
  suppressWarnings(suppressMessages(prevcarga_infer_cell(member, pcc_area_of(member$mdir), PCC_O, ctx)))

## Artefato EM MEMORIA (antes de qualquer freeze): os mesmos passos de prevcarga_train, sem publicar no registry.
pcc_train_artifact <- function(member, ctx) {
  area <- pcc_area_of(member$mdir)
  inp <- prevcarga_train_inputs(member, area, PCC_O, ctx)
  pctx <- list(load_label = inp$cuts$load_label, timezone = inp$cuts$tz, weather_issue_cutoff = inp$cuts$weather_issue_cutoff)
  fit <- suppressWarnings(suppressMessages(prevcarga_plugin_train(inp$plugin, inp$panel, inp$cuts, inp$spec, pctx)))
  list(plugin = inp$plugin, artifact = prevcarga_artifact_build(member, area, inp, fit))
}

## Inferencia de um artefato (em memoria ou descongelado) pelos mesmos passos de prevcarga_infer_cell.
pcc_infer_artifact <- function(plugin, art, member, ctx) {
  area <- pcc_area_of(member$mdir)
  spec <- art$spec_efetivo
  cuts <- .prevcarga_inf_cuts(plugin, spec, PCC_O, ctx, member$mdir)
  panel <- prevcarga_read_panel(ctx, PCC_O, union(cuts$deck_areas, area), spec$horizon_days, spec$history_days$infer)
  cuts$available_until <- panel$attrs$available_until
  cuts$weather_issue_limit <- panel$attrs$weather_issue_limit
  panel <- prevcarga_infer_panel(.prevcarga_inf_select_area(panel, area), cuts, spec)
  suppressWarnings(suppressMessages(prevcarga_infer(plugin, art, panel, cuts, list(), spec = NULL)))
}

## --- cenario `check_members`: a matriz de cobertura dos membros da topologia `opt$topology` ------------------------
pcc_check_members <- function(root, topology_path) {
  mroot <- prevcarga_models_root(root)
  raw <- prevcarga_topology(path = topology_path, root = root)$members_raw
  for (m in raw)
    if (!file.exists(file.path(mroot, m$mdir, "run.yaml")))
      prevcarga_abort("contract.topology_member_invalid", where = sprintf("%s: models/%s/run.yaml ausente", m$sigla, m$mdir))
  mem <- prevcarga_members(path = topology_path, root = root)
  orphan <- setdiff(basename(dirname(Sys.glob(file.path(mroot, "*", "run.yaml")))), unique(mem$mdir))
  if (length(orphan))
    prevcarga_abort("contract.topology_member_invalid", where = sprintf("models/%s/run.yaml sem membro na topologia", orphan[[1L]]))
  rows <- lapply(seq_len(nrow(mem)), function(i) {
    p <- prevcarga_plugin_load(mem$mdir[[i]], mroot)
    d <- p$descriptor
    ex <- mem$exogena[[i]]; vp <- d$variant_param
    if (is.na(ex) != is.na(vp))
      prevcarga_abort("contract.topology_member_invalid", where = sprintf("%s: exogena=%s com variant_param=%s", mem$sigla[[i]], ex, vp))
    if (!is.na(vp) && !(ex %in% unlist(d$params_schema[[vp]]$allowed)))
      prevcarga_abort("contract.topology_member_invalid",
                      where = sprintf("%s: %s exogena=%s fora de params_schema$%s$allowed", mem$sigla[[i]], mem$mdir[[i]], ex, vp))
    list(sigla = mem$sigla[[i]], mdir = mem$mdir[[i]], exogena = ex, variant_param = vp, delivery_days = d$delivery_days,
         allowed = if (is.na(vp)) list() else as.list(unlist(d$params_schema[[vp]]$allowed)))
  })
  list(members = rows, run_yaml_dirs = sort(basename(dirname(Sys.glob(file.path(mroot, "*", "run.yaml"))))))
}

## --- cenario `delivery`: treina e infere os 8 membros pelas fachadas; entrega x prevcarga_delivery_grid ------------
pcc_child_delivery <- function(root, deck, work) {
  mem <- prevcarga_members(root = root)
  chan <- new.env()
  out <- lapply(seq_len(nrow(mem)), function(i) {
    m <- mem[i, , drop = FALSE]
    st <- file.path(work, paste0("state-", m$sigla))
    ctx <- pcc_child_ctx(root, deck, st, m$mdir)
    chan$ctx <- ctx
    pcc_train_once(m$mdir, ctx, m)
    r <- pcc_infer_once(m, ctx)
    plugin <- prevcarga_plugin_load(m$mdir, prevcarga_models_root(root))
    dd <- plugin$descriptor$delivery_days
    grid <- prevcarga_delivery_grid(list(available_until = r$available_until_posix, tz = PCC_TZ, origin_day = PCC_O), dd)
    tg <- r$predictions$target_datetime
    list(sigla = m$sigla, mdir = m$mdir, delivery_days = dd, identical = identical(tg, grid), n = length(tg),
         first = pcc_iso(tg[[1L]]), last = pcc_iso(tg[[length(tg)]]), grid_first = pcc_iso(grid[[1L]]),
         grid_last = pcc_iso(grid[[length(grid)]]), available_until = pcc_iso(r$available_until_posix),
         tz = attr(tg, "tzone"), anyNA = anyNA(r$predictions$y_pred))
  })
  list(members = out)
}

## --- cenario `isolation`: 5 plugins na mesma sessao limpa, treino 1x cada, snapshot antes/depois ----------------
pcc_child_isolation <- function(root, deck, work) {
  chan <- new.env()
  set.seed(7)
  before <- pcc_snapshot()
  mdirs <- pcc_all_plugins(root)
  mroot <- prevcarga_models_root(root)
  plugins <- lapply(stats::setNames(mdirs, mdirs), prevcarga_plugin_load, root = mroot)
  for (md in mdirs) {
    ctx <- pcc_child_ctx(root, deck, file.path(work, paste0("state-", md)), md)
    chan$ctx <- ctx
    pcc_train_once(md, ctx, root = root)
  }
  after <- pcc_snapshot()
  ## o motor (engine_manifest) e o env-pai do env do plugin nos DESSEM; nos demais o pai e o env de imports
  eng_env <- function(p) {
    up <- parent.env(p$env)
    if (exists("engine_manifest", envir = up, inherits = FALSE)) format(up) else NA_character_
  }
  list(before = before, after = after, mdirs = mdirs,
       plugin_env = as.list(vapply(plugins, function(p) format(p$env), "")),
       descriptor_env = as.list(vapply(plugins, function(p) format(environment(p$descriptor$stages$ingestion$fn)), "")),
       engine_env = as.list(vapply(plugins, eng_env, "")))
}

## --- cenario `seeds`: o RF com semente 123 e 124 (e 123 de novo) pelo spec_overrides ---------------------------------
pcc_child_seeds <- function(root, deck, work) {
  chan <- new.env()
  md <- "random-forest"
  m <- pcc_member_of(root, md)
  run <- function(seed, tag) {
    ctx <- pcc_child_ctx(root, deck, file.path(work, tag), md, extra = list(seed = as.integer(seed)))
    chan$ctx <- ctx
    pcc_train_once(md, ctx, m)
    pcc_infer_once(m, ctx)$predictions$y_pred
  }
  a <- run(123L, "s123"); b <- run(124L, "s124"); a2 <- run(123L, "s123b")
  list(max_diff_123_124 = max(abs(a - b)), max_diff_123_123 = max(abs(a - a2)), n = length(a))
}

## --- cenario `portability`: freeze 2x, thaw -> infer, ENV sujo, formals ------------------------------------------------
pcc_child_portability <- function(root, deck, work) {
  chan <- new.env()
  dirty <- c(EXOGENA = "MAX", LGBM_DISABLE_LOESS = "0", LGBM_TRAIN_CV = "0", RF_ARTIFACT_COMPRESS = "gzip",
             PREVCARGA_DATA_DIR = "/nao/existe")
  res <- lapply(stats::setNames(pcc_all_plugins(root), pcc_all_plugins(root)), function(md) {
    m <- pcc_member_of(root, md)
    ctx <- pcc_child_ctx(root, deck, file.path(work, paste0("state-", md)), md)
    chan$ctx <- ctx
    t1 <- pcc_train_artifact(m, ctx)
    t2 <- pcc_train_artifact(m, ctx)
    f1 <- file.path(work, paste0(md, "-1.rds")); f2 <- file.path(work, paste0(md, "-2.rds"))
    sha1 <- prevcarga_freeze(t1$artifact, f1)
    sha2 <- prevcarga_freeze(t2$artifact, f2)
    thawed <- prevcarga_thaw(f1)
    p_mem <- pcc_infer_artifact(t1$plugin, t1$artifact, m, ctx)
    p_thaw <- pcc_infer_artifact(t1$plugin, thawed, m, ctx)
    p_dirty <- withr::with_envvar(dirty, pcc_infer_artifact(t1$plugin, thawed, m, ctx))
    st <- t1$plugin$descriptor$stages
    list(sha1 = sha1, sha2 = sha2, thaw_identical = identical(p_mem, p_thaw), dirty_identical = identical(p_thaw, p_dirty),
         n = nrow(p_mem), features_formals = names(formals(st$inference_features$fn)),
         infer_formals = names(formals(st$inference$fn)))
  })
  list(plugins = res)
}

pcc_child_main <- function(args) {
  scenario <- args[[1L]]
  out <- args[[2L]]
  opt <- if (length(args) > 2L) jsonlite::fromJSON(args[[3L]], simplifyVector = TRUE) else list()
  root <- pc_root()
  pcc_child_lib(root)
  work <- tempfile("pcc-work-"); dir.create(work)
  res <- tryCatch(withCallingHandlers({
    switch(scenario,
      check_members = {
        prevcarga_registry_reset()
        pcc_check_members(root, opt$topology)
      },
      {
        deck <- pcc_child_deck(file.path(work, "deck"))
        switch(scenario,
          delivery = pcc_child_delivery(root, deck, work),
          isolation = pcc_child_isolation(root, deck, work),
          seeds = pcc_child_seeds(root, deck, work),
          portability = pcc_child_portability(root, deck, work),
          stop("cenario desconhecido: ", scenario))
      })
  }, warning = function(w) invokeRestart("muffleWarning")),
  prevcarga_error = function(e) {
    writeLines(c(conditionMessage(e), prevcarga_render_error_text(e)), con = stderr())
    quit(status = e$exit_code, save = "no")
  })
  jsonlite::write_json(res, out, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  invisible(0L)
}

## ---- lado do testthat: roda o filho e devolve status, stderr e o JSON ----------------------------------------------
pcc_run_child <- function(scenario, opt = list(), root = pc_root(), .local_envir = parent.frame()) {
  d <- withr::local_tempdir(pattern = "pcc-child-", .local_envir = .local_envir)
  script <- file.path(d, "child.R")
  out <- file.path(d, "out.json")
  writeLines(c(
    sprintf("pc_root <- function() %s", deparse(normalizePath(root, winslash = "/", mustWork = TRUE))),
    sprintf("source(%s)", deparse(file.path(normalizePath(root, winslash = "/"), "scripts", "lib", "tests", "helper-porte-closure.R"))),
    "pcc_child_main(commandArgs(TRUE))"), script)
  optf <- file.path(d, "opt.json")
  jsonlite::write_json(opt, optf, auto_unbox = TRUE)
  err <- file.path(d, "err.txt")
  st <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script), scenario, shQuote(out), shQuote(optf)),
                                 stdout = FALSE, stderr = err))
  list(status = as.integer(st), stderr = readLines(err, warn = FALSE),
       json = if (file.exists(out)) jsonlite::read_json(out, simplifyVector = FALSE) else NULL)
}
