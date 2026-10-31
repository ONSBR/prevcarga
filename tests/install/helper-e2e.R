## helper-e2e.R -- helper dos ponta a ponta no bundle INSTALADO (M8.9; auto-carregado por testthat::test_dir, depois do helper-install.R
## e do helper-sync.R). Nao e codigo de produto. Tudo roda pelo LANCADOR da plataforma (<R>/bin/prevcarga ou <R>\bin\prevcarga.cmd),
## nunca pelo Rscript da arvore de dev. Nunca fala com bucket real (o "bucket" e um diretorio local servido por httpuv) e nunca instala
## fora de sandbox/ (gitignored, mesmo disco do repo). A perna Windows NAO e executada aqui: ver tests/ab/test-cross-platform.R.
##
## Mundo de teste (pc_e2e_world, memoizado por sessao):
##   - bundle candidato de dist/ (helper-install.R) reempacotado com UMA mudanca: config/topology.yaml de fixture (3 membros reais RF, LGBM,
##     HW; folhas A1 A2 B1; SS1 SS2) e RUNTIME.json regerado. A topologia real (8 membros, 25 areas) levaria horas para treinar; o resto
##     do bundle (runtime, library, code) e o do build;
##   - instalado por install/install.sh numa raiz temporaria;
##   - estado "oficial" PRODUTOR: RF e LGBM treinados pelo lancador (`train-models`), k-hat pelo lancador (`reconcile --mode=train`) e pesos da
##     combina pelo codigo da arvore de dev (o treino da combina pede um painel de varias origens que so a camada de teste monta);
##   - bucket local montado pelo `release-pack` REAL do lancador, servido por httpuv; o `prevcarga-sync` instalado baixa para o estado LOCAL.

.pce_state <- new.env(parent = emptyenv())

## --- lancador ---------------------------------------------------------------------------------------------------------
## Ambiente do filho = o do teste sem as variaveis R_* do R hospedeiro.
pce_clean_env <- function() { e <- Sys.getenv(); e[setdiff(names(e), c("R_LIBS_USER", "R_LIBS", "R_LIBS_SITE", "R_HOME"))] }

## Executa o lancador da plataforma. `args` sem --format (acrescentado como json). Devolve list(status, stdout, stderr, json).
## `wd` = diretorio de trabalho do filho (default: o atual).
pc_e2e_cli <- function(root, args, timeout = 1800, wd = NULL) {
  win <- identical(.Platform$OS.type, "windows")
  cmd <- if (win) file.path(root, "bin", "prevcarga.cmd") else file.path(root, "bin", "prevcarga")
  ## o filho herda o ambiente do R de teste: R_LIBS_USER/R_LIBS/R_LIBS_SITE/R_HOME do R hospedeiro vazariam a library do host para o
  ## bundle (ver o discovery do M8.9); o teste os remove para exercitar o lancador como numa sessao limpa
  r <- processx::run(cmd, c(args, "--format", "json"), error_on_status = FALSE, windows_verbatim_args = FALSE,
                     encoding = "UTF-8", timeout = timeout, wd = wd,
                     env = pce_clean_env())
  stopifnot(!any(charToRaw(paste0(r$stdout, r$stderr)) > as.raw(127)))
  list(status = r$status, stdout = r$stdout, stderr = r$stderr,
       json = if (nzchar(r$stdout)) tryCatch(jsonlite::fromJSON(r$stdout, simplifyVector = FALSE), error = function(e) NULL) else NULL)
}

## --- deck de fixture ------------------------------------------------------------------------------------------------
pce_fx_env <- function() {
  if (!is.null(.pce_state$fx)) return(.pce_state$fx)
  e <- new.env(parent = globalenv())
  lib <- file.path(pci_root(), "scripts", "lib")
  options(prevcarga.lib = lib)
  sys.source(file.path(lib, "errors.R"), envir = e)
  sys.source(file.path(pci_root(), "scripts", "tools", "fixture_store.R"), envir = e)
  .pce_state$fx <- e
}

PCE_TZ <- "Etc/GMT+3"
PCE_LEAVES <- c("SP", "RJ", "RS", "PR")      # areas REAIS da topologia: o LGBM so conhece as areas da tabela heat_area do descriptor
PCE_SS <- c("SECO", "S")     # 2 subsistemas x 2 areas: o verbo `areas` serializa `order`/`areas` de 1 elemento como texto (discovery M8.9)

## Deck sintetico do mundo: folhas + subsistemas, `ndays` dias ate O, carga ate `until` de O (fuso PCE_TZ), rotulo `label`.
pc_e2e_deck <- function(dir, origin, ndays = 800L, until = "09:00", label = "start", seed = 11) {
  fx <- pce_fx_env()
  og <- lubridate::ymd(origin)
  tmp <- tempfile("pc-e2e-deck-", tmpdir = dirname(dir))
  fx$prevcarga_fixture_deck(tmp, seed = seed, areas = c(PCE_LEAVES, PCE_SS), days = seq(og - (ndays - 1L), og, by = "day"),
                            load_label = label, load_until = lubridate::ymd_hm(paste(origin, until), tz = PCE_TZ), tz = PCE_TZ)
  ## O+3 (feriado futuro do fixture, codigo sem amostra historica) vira dia normal (codigo 12), como no e2e do M6.4
  for (a in c(PCE_LEAVES, PCE_SS)) {
    nxt <- fx$prevcarga_fixture_set_holiday(tmp, a, og + 3L, c(holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = 1L))
    unlink(tmp, recursive = TRUE); tmp <- nxt
  }
  stopifnot(file.copy(tmp, dirname(dir), recursive = TRUE), file.rename(file.path(dirname(dir), basename(tmp)), dir))
  dir
}

## O MESMO deck com a carga re-rotulada: `to = "start"` desloca a carga -30 min (interval_end -> interval_start) e declara `start` no
## manifest; `to = "end"` faz o inverso. `declare` e o rotulo escrito no manifest.json (default = `to`); `declare` diferente de `to` e o
## CONTROL (deck pre-deslocado declarado `end` desloca todo intervalo de 30 min). O deck de entrada nunca e escrito: devolve uma copia.
pc_fixture_deck_relabel <- function(deck, to = c("start", "end"), declare = to, env = parent.frame()) {
  to <- match.arg(to)
  fx <- pce_fx_env()
  out <- pci_work_dir(env, "pc-deck-")
  d <- file.path(out, "deck")
  dir.create(d)
  stopifnot(all(file.copy(list.files(deck, full.names = TRUE, all.files = TRUE, no.. = TRUE), d, recursive = TRUE)))
  m <- jsonlite::read_json(file.path(d, "manifest.json"))
  from <- m$load_label
  if (!identical(from, to)) {
    delta <- lubridate::minutes(if (identical(to, "start")) -30L else 30L)
    fx$.pc_fx_rewrite_hits(d, "load", NULL, function(x) rep(TRUE, nrow(x)),
                           function(x, i) data.table::set(x, i = i, j = "timestamp", value = x$timestamp[i] + delta))
  }
  m$load_label <- declare
  jsonlite::write_json(m, file.path(d, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  d
}

## Copia do deck com `delta` somado a `load` de TODAS as areas num dia inteiro `dia` (Date) cujo `interval_end <= available_until`
## (o dia O-1: o deck do mundo tem carga ate O 09:00). Os mutadores do fixture_store trabalham em copia.
pc_perturb_load <- function(deck, dia, delta, areas = NULL) {
  fx <- pce_fx_env()
  m <- jsonlite::read_json(file.path(deck, "manifest.json"))
  tz <- m$timezone$load
  d0 <- lubridate::force_tz(lubridate::as_datetime(lubridate::ymd(dia)), tz)
  fx$prevcarga_fixture_perturb(deck, "load", after = d0 - lubridate::minutes(30L), at_or_before = d0 + lubridate::minutes(23L * 60L + 30L),
                               areas = areas, perturb = function(v) v + delta)
}

## --- bundle de teste e instalacao ----------------------------------------------------------------------------------------
PCE_TOPOLOGY <- c("schema_version: 1", "khat_range: [0.1, 5.0]", "subsystems:",
                  "  SECO: {areas: [SP, RJ], loss: PESE}", "  S: {areas: [RS, PR], loss: PES}",
                  "order: [SECO, S]", "members:",
                  "  - {sigla: RF, mdir: random-forest, exogena: null}",
                  "  - {sigla: LGBM, mdir: lgbm, exogena: null}",
                  "  - {sigla: HW, mdir: holt-winters, exogena: null}")

## Reempacota a base COMPLETA (lightgbm incluido) com a topologia de fixture e a versao dada; mesma mecanica de pc_fixture_bundle
## (RUNTIME.json canonico regerado, files[] com o sha novo). Devolve list(path, sha256, version).
pce_bundle <- function(version, env = parent.frame()) {
  e <- pci_env()
  base <- pci_base(slim = FALSE)
  d <- pci_work_dir(env, "pc-e2e-bundle-")
  root <- file.path(d, "prevcarga")
  pci_sh(sprintf("cp -al %s %s", shQuote(base), shQuote(root)))
  edit <- function(rel, writer) {
    f <- file.path(root, "code", rel)
    unlink(f); writer(f)
    digest::digest(file = f, algo = "sha256")
  }
  sha_v <- edit(file.path("config", "VERSION"), function(f) writeLines(version, f))
  sha_t <- edit(file.path("config", "topology.yaml"), function(f) writeLines(PCE_TOPOLOGY, f))
  rj <- file.path(root, "RUNTIME.json")
  x <- jsonlite::read_json(rj, simplifyVector = FALSE)
  x$version <- version
  paths <- vapply(x$files, function(f) f$path, "")
  x$files[[match("code/config/VERSION", paths)]]$sha256 <- sha_v
  x$files[[match("code/config/topology.yaml", paths)]]$sha256 <- sha_t
  unlink(rj)
  con <- file(rj, open = "wb"); writeBin(charToRaw(e$prevcarga_runtime_json_canonical(x, with_built_at = TRUE)), con); close(con)
  out <- file.path(d, sprintf("prevcarga-%s-linux-x86_64.tar.zst", version))
  pci_sh(sprintf("tar --sort=name --owner=0 --group=0 --numeric-owner -C %s -cf - prevcarga | zstd -3 -T0 -q -f -o %s",
                 shQuote(d), shQuote(out)))
  unlink(root, recursive = TRUE, force = TRUE)
  list(path = out, sha256 = digest::digest(file = out, algo = "sha256"), version = version)
}

## Instala o bundle com o instalador REAL; devolve a raiz <R> (com bin/prevcarga e bin/prevcarga-sync).
pce_install <- function(b, root) {
  r <- pc_install(c("--bundle", b$path, "--sha256", b$sha256, "--root", root, "--format", "json"))
  if (!identical(r$status, 0L)) stop("instalacao do bundle de e2e falhou (", r$status, "): ", r$stdout, r$stderr, call. = FALSE)
  root
}

## --- estado oficial produtor + bucket local -------------------------------------------------------------------------------------
PCE_ORIGIN <- "2026-10-15"

## `--data`/`--state` explicitos em toda chamada (flag > config.yaml): o teste nao depende de config.yaml.
pce_args <- function(verb, deck, state, ...) c(verb, paste0("--data=", deck), paste0("--state=", state), ...)

pce_ok <- function(r, what) {
  if (!identical(r$status, 0L)) stop(what, " falhou (exit ", r$status, "): ", substr(paste(r$stdout, r$stderr), 1L, 1500L), call. = FALSE)
  invisible(r)
}

## Pesos da combina e k-hat do estado produtor: o treino da combina pede um painel de varias origens que so as fixtures do M6
## (helper-m6-fixtures.R, scripts/lib/tests) montam; roda num `Rscript --vanilla` FILHO da arvore de dev (a lib no globalenv, como a CLI),
## sem tocar o processo de teste. As entregas dos membros sao as sentinelas do M6 (na grade do descriptor real e no corte do leitor);
## o k-hat e o treino REAL do Reconcile sobre o deck.
pce_prepare_combina <- function(deck, state, topology, origin, work) {
  root <- pci_root()
  script <- file.path(work, "prepare-combina.R")
  writeLines(c(
    sprintf("ROOT <- %s; DECK <- %s; STATE <- %s; TOPO <- %s; O <- lubridate::ymd(%s)", deparse(root), deparse(deck), deparse(state),
            deparse(topology), deparse(origin)),
    "options(prevcarga.lib = file.path(ROOT, 'scripts', 'lib'))",
    "source(file.path(ROOT, 'scripts', 'lib', 'tests', 'helper-prevcarga.R'))",
    "pc_root <- function() ROOT",            # depois do helper: ele redefine pc_root() por testthat::test_path()
    "source(file.path(ROOT, 'scripts', 'lib', 'tests', 'helper-m6-fixtures.R'))",
    sprintf("M6F_LEAVES <- %s; M6F_SS <- %s", deparse(PCE_LEAVES), deparse(PCE_SS)),   # as constantes do M6 sao A1 A2 B1 / SS1 SS2
    "env <- pc_source_lib(c('errors', 'runtime', 'config', 'topology', 'provenance', 'registry', 'schema_long', 'deck_reader', 'dates',",
    "  'evaluation', 'validation', 'features', 'training', 'inference', 'combination', 'build_long_output', 'reconcile', 'exports', 'forecast'))",
    "sys.source(file.path(ROOT, 'scripts', 'tools', 'fixture_store.R'), envir = env)",
    "suppressMessages(env$prevcarga_runtime_enforce())",
    "env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()",
    "ctx <- m6f_ctx(DECK, STATE, TOPO)",
    "m6f_prepare_origin(env, ctx, O, window_days = 300L)"), script)
  r <- processx::run(file.path(R.home("bin"), "Rscript"), c("--vanilla", script), error_on_status = FALSE, timeout = 3600,
                     env = pce_clean_env())
  if (!identical(r$status, 0L)) stop("preparo da combina falhou: ", substr(paste(r$stdout, r$stderr), 1L, 2000L), call. = FALSE)
  invisible(TRUE)
}

pc_e2e_msg <- function(...) message("[e2e-world] ", ...)

## Chave do cache do mundo: sha256 do bundle candidato + arvores do codigo de produto no HEAD + diff do codigo de produto (scripts/models/config/install/renv.lock).
pce_world_key <- function() {
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(pci_root()), ...), stdout = TRUE, stderr = FALSE))
  head <- paste(git("rev-parse", "HEAD:scripts", "HEAD:models", "HEAD:config", "HEAD:install"), collapse = "")   # arvores: um commit de teste nao muda a chave
  diff <- paste(git("diff", "HEAD", "--", "scripts", "models", "config", "install", "renv.lock"), collapse = "\n")
  substr(digest::digest(paste(digest::digest(file = pci_candidate(), algo = "sha256"), head, diff, sep = "|"), algo = "sha256", serialize = FALSE), 1L, 12L)
}

## Mundo completo (memoizado). Campos: root (instalacao), deck, prod_state, bucket (dir), bundle, origin, work.
## O estado produtor roda a rodada oficial (o `forecast` do lancador treina o HW, origin_scoped, dentro dela); o release-pack REAL
## monta o bucket com os artefatos e a entrega; o bucket NUNCA e o real.
pc_e2e_world <- function() {
  if (!is.null(.pce_state$world)) return(.pce_state$world)
  ## O mundo leva ~15 min (treino real de RF e LGBM): fica em cache em sandbox/ (gitignored; `/tidy` o apaga por idade), chaveado pelo
  ## bundle candidato + o codigo do worktree, e e reaproveitado entre os arquivos de teste e entre sessoes. Chave diferente => mundo novo.
  key <- pce_world_key()
  work <- file.path(pci_root(), "sandbox", paste0("pc-e2e-world-", key))
  for (old in setdiff(Sys.glob(file.path(pci_root(), "sandbox", "pc-e2e-world-*")), work)) unlink(old, recursive = TRUE, force = TRUE)
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  step <- function(name, code) {
    mark <- file.path(work, paste0(".done-", name))
    if (!file.exists(mark)) { pc_e2e_msg(name); force(code); writeLines("ok", mark) }
  }
  O <- PCE_ORIGIN
  root <- file.path(work, "R"); deck <- file.path(work, "deck"); prod <- file.path(work, "prod-state")
  topo <- file.path(work, "topology.yaml"); bucket <- file.path(work, "bucket")
  b <- NULL
  step("bundle-install", { b <- pce_bundle("e2e-0.9.0"); pce_install(b, root); saveRDS(b, file.path(work, "bundle.rds")) })
  b <- readRDS(file.path(work, "bundle.rds"))
  step("deck", { writeLines(PCE_TOPOLOGY, topo); pc_e2e_deck(deck, O); dir.create(prod, showWarnings = FALSE) })
  step("train-origin-agnostic", for (m in c("random-forest", "lgbm"))
    pce_ok(pc_e2e_cli(root, pce_args("train-models", deck, prod, "--origin", O, paste0("--model=", m), "--skip-validate")), paste("train-models", m)))
  step("combina-khat", pce_prepare_combina(deck, prod, topo, O, work))
  step("forecast-oficial", pce_ok(pc_e2e_cli(root, pce_args("forecast", deck, prod, "--origin", O, "--skip-validate")), "forecast oficial"))
  step("release-pack", {
    pub <- file.path(work, "published.txt"); writeLines(character(), pub)
    pce_ok(pc_e2e_cli(root, c("release-pack", "--what=artifacts", paste0("--out=", file.path(work, "bucket-art")), paste0("--published=", pub),
                              paste0("--state=", prod), "--latest")), "release-pack artifacts")
    pce_ok(pc_e2e_cli(root, c("release-pack", "--what=forecasts", paste0("--out=", file.path(work, "bucket-fc")), paste0("--published=", pub),
                              paste0("--state=", prod), paste0("--origin=", O), "--latest")), "release-pack forecasts")
    dir.create(bucket)
    stopifnot(all(file.copy(grep("SHA256SUMS$", c(list.files(file.path(work, "bucket-art"), full.names = TRUE), list.files(file.path(work, "bucket-fc"), full.names = TRUE)),
                                 invert = TRUE, value = TRUE), bucket, recursive = TRUE)))
  })
  .pce_state$world <- list(root = root, deck = deck, prod_state = prod, bucket = bucket, bundle = b, origin = O, work = work, topology = topo)
}

## Mundo do rotulo `end` (memoizado e em cache no mesmo diretorio): o artefato carrega o `load_label` com que foi treinado e o forecast RECUSA um
## deck de outro rotulo (contract.load_label_mismatch), entao a perna `end` do e2e precisa dos seus proprios artefatos: o MESMO deck gerado com
## rotulo `end` (mesmos valores de carga, carimbo = interval_end), RF e LGBM treinados pelo lancador e k-hat/combina como no mundo base.
## Reusa o bundle instalado do mundo base. Campos: deck, state.
pc_e2e_world_end <- function(w = pc_e2e_world()) {
  if (!is.null(.pce_state$end)) return(.pce_state$end)
  step <- function(name, code) {
    mark <- file.path(w$work, paste0(".done-", name))
    if (!file.exists(mark)) { pc_e2e_msg(name); force(code); writeLines("ok", mark) }
  }
  deck <- file.path(w$work, "deck-end"); state <- file.path(w$work, "state-end")
  step("end-deck", { pc_e2e_deck(deck, w$origin, label = "end"); dir.create(state, showWarnings = FALSE) })
  step("end-train-origin-agnostic", for (m in c("random-forest", "lgbm"))
    pce_ok(pc_e2e_cli(w$root, pce_args("train-models", deck, state, "--origin", w$origin, paste0("--model=", m), "--skip-validate")), paste("train-models end", m)))
  step("end-combina-khat", pce_prepare_combina(deck, state, w$topology, w$origin, w$work))
  .pce_state$end <- list(deck = deck, state = state)
}

## --- sincronizacao e rodadas locais ------------------------------------------------------------------------------------------------
## Serve o bucket local por httpuv e roda o `prevcarga-sync` INSTALADO (sob `env -i`) para `state`. Devolve o resultado do utilitario
## (list(status, stdout, stderr, json)); o servidor morre ao sair da funcao.
pc_e2e_sync <- function(w, what = c("artifacts", "forecasts"), state) {
  what <- match.arg(what)
  srv <- pc_serve_bucket(w$bucket)
  on.exit(srv$stop(), add = TRUE)
  r <- pc_sync_run(file.path(w$root, "bin", "prevcarga-sync"),
                   c("--what", what, if (what == "forecasts") c("--origin", w$origin), "--release-base", srv$url, "--state", state, "--format", "json"))
  r$json <- tryCatch(pci_json(r$stdout), error = function(e) NULL)
  r
}

## Estados sincronizados do mundo (memoizados): `local` = so os artefatos oficiais (de onde cada rodada local parte);
## `oficial` = so a inferencia oficial (forecasts/<O>/<AAAAMMDDTHHMM>/<versao>), que o `verificar --oficial` le.
pc_e2e_synced <- function(w) {
  if (!is.null(.pce_state$synced)) return(.pce_state$synced)
  loc <- file.path(w$work, "state-local"); off <- file.path(w$work, "state-oficial")
  unlink(c(loc, off), recursive = TRUE)
  for (x in list(list("artifacts", loc), list("forecasts", off))) {
    r <- pc_e2e_sync(w, x[[1L]], x[[2L]])
    if (!identical(r$status, 0L)) stop("prevcarga-sync ", x[[1L]], " falhou (", r$status, "): ", r$stdout, r$stderr, call. = FALSE)
  }
  .pce_state$synced <- list(local = loc, oficial = off)
}

## Copia um estado para um diretorio novo em sandbox/ (o export e imutavel: cada rodada parte de uma copia) e devolve o caminho.
pc_e2e_state_copy <- function(src, env = parent.frame()) {
  d <- file.path(pci_work_dir(env, "pc-state-"), "state")
  dir.create(d)
  stopifnot(all(file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE), d, recursive = TRUE)))
  d
}

## `forecast --origin O` pelo lancador instalado, num estado proprio (copia de `from`: o sincronizado do mundo base por padrao); devolve list(cli, saida, state).
pc_e2e_forecast <- function(w, deck, origin = w$origin, extra = character(), from = pc_e2e_synced(w)$local, env = parent.frame()) {
  st <- pc_e2e_state_copy(from, env)
  r <- pce_ok(pc_e2e_cli(w$root, pce_args("forecast", deck, st, "--origin", origin, "--skip-validate", extra)), "forecast")
  list(cli = r, saida = r$json$result$saida, state = st)
}

## sha256 por arquivo (caminho relativo -> sha), ordenado radix, de uma entrega; `skip` = arquivos fora da comparacao.
pc_e2e_delivery_shas <- function(dir, skip = character()) {
  rel <- setdiff(sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix"), skip)
  stats::setNames(vapply(rel, function(p) digest::digest(file = file.path(dir, p), algo = "sha256"), ""), rel)
}
