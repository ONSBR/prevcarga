## test-registry-lifecycle.R -- ciclo de vida do registry + verbo `registry` (M5.7; ADR-0030, ADR-0039, ADR-0040, ADR-0038).
## Funcoes in-process (registry.R) + gates de PROCESSO (`Rscript --vanilla <copia>/scripts/prevcarga registry ...`) sobre
## instalacao-copia em tempdir. Tudo em tempdir; sem env vars. Cada FORBIDDEN tem o seu CONTROL. Roda igual em Linux e Windows
## nativo: so o braco do link simbolico pula no Windows (com motivo).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "provenance", "topology", "registry"), env = environment())
reg_env <- environment()   # onde vivem as funcoes: o mock de file.rename precisa entrar aqui
root_cp <- pc_install_copy()

ctx_of <- function(st) list(state_dir = st, root = pc_root())
expect_reg <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}
## celula com versoes publicadas (manifesto minimo) + latest/pins; devolve o caminho da celula
mk_cell <- function(st, rel, versions, latest = NULL, pins = character(), manifest = function(v) list(note = v)) {
  key <- file.path(st, "artifacts", rel)
  for (v in versions) prevcarga_registry_publish(list(x = v), manifest(v), key, v)
  if (!is.null(latest)) prevcarga_registry_promote_latest(key, latest)
  if (length(pins)) writeLines(pins, file.path(key, ".pin"))
  key
}
mock_fail_rename <- function(frame = parent.frame()) {
  assign("file.rename", function(from, to) FALSE, envir = reg_env)
  withr::defer(if (exists("file.rename", envir = reg_env, inherits = FALSE)) rm("file.rename", envir = reg_env), envir = frame)
}
cli <- function(st, ...) pc_cli(root_cp, c("registry", ..., paste0("--state=", st)))
cli_json <- function(st, ...) {
  r <- cli(st, ..., "--format=json")
  list(r = r, j = if (length(r$stdout)) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE))
}

## ---- AC1: gc -------------------------------------------------------------------------------------------------
gc_fixture <- function(st, with_bait = FALSE) {
  rf <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1", "v_c1"), latest = "v_a1", pins = "v_b1")
  cb <- mk_cell(st, "combina/SECO", c("v_aa", "v_bb"), latest = "v_aa")
  rc <- mk_cell(st, "reconcile/SECO", c("v_f1"), latest = "v_f1")
  dir.create(file.path(rf, ".tmp-xyz")); writeLines("x", file.path(rf, ".tmp-xyz", "f"))
  dir.create(file.path(cb, ".tmp-w1")); writeLines("x", file.path(cb, ".tmp-w1", "weights.rds"))
  dir.create(file.path(rc, ".tmp-k1")); writeLines("x", file.path(rc, ".tmp-k1", "artifact.rds"))
  bait <- NULL
  if (with_bait) {
    bait <- file.path(tempfile("fora-"), "fora"); dir.create(file.path(bait, "v_dd"), recursive = TRUE)
    writeLines("keep", file.path(bait, "v_dd", "f"))
    file.symlink(bait, file.path(rf, "v_dd"))
  }
  list(rf = rf, cb = cb, rc = rc, bait = bait)
}

test_that("AC1: --dry-run lista C e os 3 .tmp-* sem apagar; gc real mantem A (latest) e B (pinned) (FORBIDDEN delta 0)", {
  st <- pc5_state_tmp(); f <- gc_fixture(st); ctx <- ctx_of(st)
  before <- pc5_tree_hash(file.path(st, "artifacts"))
  dry <- prevcarga_registry_gc(ctx, dry_run = TRUE)
  expect_setequal(dry$removed, c("random-forest/SECO/v_c1", "random-forest/SECO/.tmp-xyz", "combina/SECO/.tmp-w1",
                                 "reconcile/SECO/.tmp-k1", "combina/SECO/v_bb"))
  expect_identical(pc5_tree_hash(file.path(st, "artifacts")), before)
  res <- prevcarga_registry_gc(ctx)
  expect_setequal(res$removed, dry$removed)
  expect_true(all(dir.exists(file.path(f$rf, c("v_a1", "v_b1"))), dir.exists(file.path(f$cb, "v_aa")), dir.exists(file.path(f$rc, "v_f1"))))
  expect_false(any(file.exists(file.path(f$rf, c("v_c1", ".tmp-xyz"))), file.exists(file.path(f$cb, c(".tmp-w1", "v_bb"))),
                   file.exists(file.path(f$rc, ".tmp-k1"))))
  kr <- res$kept[key == "random-forest/SECO"]
  expect_identical(kr$reason[match(c("v_a1", "v_b1"), kr$version)], c("latest", "pinned"))
  expect_identical(res$kept[key == "combina/SECO"]$version, "v_aa")      # v_<hash> da combina apontado por latest.txt
  expect_identical(prevcarga_resolve_version(f$rf, "latest"), "v_a1")
})

test_that("AC1 CONTROL: sem o pin, B e removido (delta != 0); gc sem nada a podar = removed vazio", {
  st <- pc5_state_tmp(); f <- gc_fixture(st); ctx <- ctx_of(st)
  unlink(file.path(f$rf, ".pin"))
  expect_true("random-forest/SECO/v_b1" %in% prevcarga_registry_gc(ctx)$removed)
  expect_false(dir.exists(file.path(f$rf, "v_b1")))
  again <- prevcarga_registry_gc(ctx)
  expect_length(again$removed, 0L)
})

test_that("AC1: o gc via CLI (processo) sai 0, JSON removed/kept; sem nada a podar removed = []", {
  st <- pc5_state_tmp(); gc_fixture(st)
  d <- cli_json(st, "gc", "--dry-run")
  expect_identical(d$r$status, 0L); expect_length(d$r$stderr, 0L)
  expect_true(d$j$ok); expect_true(d$j$result$dry_run); expect_length(d$j$result$removed, 5L)
  g <- cli_json(st, "gc")
  expect_identical(g$r$status, 0L)
  expect_setequal(vapply(g$j$result$kept, function(k) k$reason, ""), c("latest", "pinned"))
  e <- cli_json(st, "gc")
  expect_identical(e$r$status, 0L); expect_identical(e$j$result$removed, list())
})

test_that("AC1: isca fora da raiz atras de link simbolico fica intacta e o link nao e seguido", {
  skip_on_os("windows", "link simbolico exige privilegio no Windows; o resto do gc roda nos dois sistemas")
  st <- pc5_state_tmp(); f <- gc_fixture(st, with_bait = TRUE); ctx <- ctx_of(st)
  res <- prevcarga_registry_gc(ctx)
  expect_true(file.exists(file.path(f$bait, "v_dd", "f")))
  expect_false("random-forest/SECO/v_dd" %in% res$removed)
  expect_true(file.exists(file.path(f$rf, "v_dd")))   # o link segue la, nao foi removido
})

test_that("AC1: gc recusa alvo fora da raiz (contain) e --model restringe a poda", {
  st <- pc5_state_tmp(); gc_fixture(st); ctx <- ctx_of(st)
  res <- prevcarga_registry_gc(ctx, model = "random-forest")
  expect_setequal(res$removed, c("random-forest/SECO/v_c1", "random-forest/SECO/.tmp-xyz"))
  expect_true(dir.exists(file.path(st, "artifacts", "combina", "SECO", ".tmp-w1")))
  expect_reg(prevcarga_registry_gc(ctx, model = "../.."), "contract.path_outside_root", 2L)
})

## ---- AC2: promote atomico e interrompido ---------------------------------------------------------------------
test_that("AC2: promote normal => latest = B e B ultima linha de .history (CONTROL delta != 0)", {
  st <- pc5_state_tmp(); key <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_a1")
  r <- prevcarga_registry_promote(key, "v_b1", ctx = ctx_of(st))
  expect_identical(r$latest, "v_b1")
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_b1")
  expect_identical(utils::tail(readLines(file.path(key, ".history")), 1L), "v_b1")
  expect_identical(list.files(key, pattern = "^[.]tmp-", all.files = TRUE), character())
  expect_reg(prevcarga_registry_promote(key, "v_00", ctx = ctx_of(st)), "state.artifact_missing", 13L)
})

test_that("AC2: promote INTERROMPIDO (rename falha apos o .tmp-* escrito): 13, latest segue A, .tmp-* existe e o gc o remove", {
  st <- pc5_state_tmp(); key <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_a1")
  hist <- readLines(file.path(key, ".history"))
  mock_fail_rename()
  expect_reg(prevcarga_registry_promote(key, "v_b1", ctx = ctx_of(st)), "state.promote_failed", 13L)
  rm("file.rename", envir = reg_env)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_a1")
  expect_identical(readLines(file.path(key, "latest.txt")), "v_a1")
  expect_identical(readLines(file.path(key, ".history")), hist)
  orf <- list.files(key, pattern = "^[.]tmp-", all.files = TRUE)
  expect_length(orf, 1L)
  res <- prevcarga_registry_gc(ctx_of(st))
  expect_true(paste0("random-forest/SECO/", orf) %in% res$removed)      # B nao foi promovida nem pinada: tambem e orfa
  expect_identical(list.files(key, pattern = "^[.]tmp-", all.files = TRUE), character())
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_a1")
})

test_that("AC2: registry.R sem writeLines( sobre latest.txt e sem .staging-; CONTROL: o grep acusa a isca", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "registry.R"), warn = FALSE)
  code <- sub("\\s+#.*$", "", src[!grepl("^\\s*#", src)])
  expect_false(any(grepl("writeLines\\(.*latest", code)))
  expect_false(any(grepl("\\.staging-", code)))
  bait <- c("writeLines(v, file.path(key, \"latest.txt\"))", "d <- tempfile(\".staging-\", tmpdir = key)")
  expect_true(any(grepl("writeLines\\(.*latest", bait))); expect_true(any(grepl("\\.staging-", bait)))
})

## ---- AC3: rollback, pin/unpin e erros 13 ---------------------------------------------------------------------
test_that("AC3: rollback volta a A; pin B sobrevive ao gc; unpin B + gc o remove (CONTROL)", {
  st <- pc5_state_tmp(); ctx <- ctx_of(st)
  key <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_a1")
  prevcarga_registry_promote(key, "v_b1", ctx = ctx)
  expect_identical(prevcarga_registry_rollback(key, ctx = ctx)$latest, "v_a1")
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_a1")
  expect_true(all(dir.exists(file.path(key, c("v_a1", "v_b1")))))
  expect_identical(prevcarga_registry_pin(key, "v_b1", ctx = ctx)$pinned, "v_b1")
  expect_identical(prevcarga_resolve_version(key, "pin"), "v_b1")
  expect_length(prevcarga_registry_gc(ctx)$removed, 0L)
  expect_true(dir.exists(file.path(key, "v_b1")))
  prevcarga_registry_unpin(key, "v_b1", ctx = ctx)
  expect_false(file.exists(file.path(key, ".pin")))
  expect_identical(prevcarga_registry_gc(ctx)$removed, "random-forest/SECO/v_b1")
  expect_true(dir.exists(file.path(key, "v_a1")))
})

test_that("AC3: rollback sem anterior => 13 registry_no_previous; pin de inexistente => 13 pin_missing; unpin sem pin => 13", {
  st <- pc5_state_tmp(); ctx <- ctx_of(st)
  key <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_a1")
  expect_reg(prevcarga_registry_rollback(key, ctx = ctx), "state.registry_no_previous", 13L)
  expect_reg(prevcarga_registry_pin(key, "v_00", ctx = ctx), "state.pin_missing", 13L)
  expect_reg(prevcarga_registry_pin(key, "../x", ctx = ctx), "contract.version_malformed", 2L)
  expect_reg(prevcarga_registry_unpin(key, "v_b1", ctx = ctx), "state.pin_missing", 13L)
  expect_reg(prevcarga_registry_promote(file.path(st, "artifacts", "random-forest", "ZZ"), "v_a1", ctx = ctx),
             "state.cell_missing", 13L)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_a1")
})

test_that("AC3: o mesmo caminho pelo CLI (processo): exits 13 e ponteiros", {
  st <- pc5_state_tmp()
  key <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_a1")
  expect_identical(cli(st, "promote", "random-forest", "SECO", "v_b1")$status, 0L)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_b1")
  expect_identical(cli(st, "rollback", "random-forest", "SECO")$status, 0L)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_a1")
  expect_identical(cli(st, "pin", "random-forest", "SECO", "v_b1")$status, 0L)
  expect_identical(cli(st, "gc")$status, 0L)
  expect_true(dir.exists(file.path(key, "v_b1")))
  mk_cell(st, "lgbm/SECO", "v_a1", latest = "v_a1")
  e <- cli_json(st, "rollback", "lgbm", "SECO")
  expect_identical(e$r$status, 13L); expect_identical(e$j$error$id, "state.registry_no_previous")
  p <- cli_json(st, "pin", "random-forest", "SECO", "v_00")
  expect_identical(p$r$status, 13L); expect_identical(p$j$error$id, "state.pin_missing")
  expect_identical(cli(st, "unpin", "random-forest", "SECO", "v_b1")$status, 0L)
  expect_identical(cli(st, "gc")$status, 0L)
  expect_false(dir.exists(file.path(key, "v_b1")))
})

## ---- k-hat: o latest do perfil so anda para frente ------------------------------------------------------------
test_that("k-hat: promote/rollback nao recuam o latest do perfil do Reconcile (CONTROL: celula de modelo recua)", {
  st <- pc5_state_tmp(); ctx <- ctx_of(st)
  mf <- function(v) list(produto = "reconcile", khat_origin = if (v == "v_f1") "2026-10-01" else "2026-10-02")
  kk <- mk_cell(st, "reconcile/SECO", c("v_f1", "v_f2"), latest = "v_f1", manifest = mf)
  expect_identical(prevcarga_registry_promote(kk, "v_f2", ctx = ctx)$latest, "v_f2")        # para frente passa
  expect_reg(prevcarga_registry_promote(kk, "v_f1", ctx = ctx), "state.registry_khat_backward", 13L)
  expect_reg(prevcarga_registry_rollback(kk, ctx = ctx), "state.registry_khat_backward", 13L)
  expect_identical(prevcarga_resolve_version(kk, "latest"), "v_f2")
  expect_identical(prevcarga_registry_pin(kk, "v_f1", ctx = ctx)$pinned, "v_f1")          # pin nao move o latest
  expect_identical(prevcarga_resolve_version(kk, "latest"), "v_f2")
  mk <- mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_b1")
  expect_identical(prevcarga_registry_promote(mk, "v_a1", ctx = ctx)$latest, "v_a1")
})

## ---- AC4: ls/show read-only, celula oficial ----------------------------------------------------------------------
test_that("AC4: ls/show JSON com celula oficial (sem .pin/.history); hash da arvore igual antes/depois", {
  st <- pc5_state_tmp()
  man <- function(v) list(model_id = "random-forest", cuts = list(train_origin = "2026-09-30", last_data_day = "2026-09-28"),
                          load_label = "start", timezone = "Etc/GMT+3", weather_issue_cutoff = "available_until",
                          code_commit = "abc1234")
  mk_cell(st, "random-forest/SECO", c("v_a1", "v_b1"), latest = "v_b1", pins = "v_a1", manifest = man)
  off <- mk_cell(st, "lgbm/SECO", c("v_e1"), latest = "v_e1", manifest = man)
  unlink(file.path(off, c(".history", ".pin")))
  before <- pc5_tree_hash(file.path(st, "artifacts"))
  ls <- cli_json(st, "ls")
  expect_identical(ls$r$status, 0L)
  rows <- stats::setNames(ls$j$result, vapply(ls$j$result, function(r) r$key, ""))
  expect_identical(names(rows), c("lgbm/SECO", "random-forest/SECO"))
  expect_identical(rows[["random-forest/SECO"]]$latest, "v_b1")
  expect_identical(unlist(rows[["random-forest/SECO"]]$pinned), "v_a1")
  expect_identical(unlist(rows[["random-forest/SECO"]]$versions), c("v_a1", "v_b1"))
  expect_identical(rows[["lgbm/SECO"]]$latest, "v_e1"); expect_length(rows[["lgbm/SECO"]]$pinned, 0L)
  sh <- cli_json(st, "show", "random-forest", "SECO")
  expect_identical(sh$r$status, 0L)
  r <- sh$j$result
  expect_identical(r$version, "v_b1"); expect_identical(r$model_id, "random-forest")
  expect_identical(r$cuts$train_origin, "2026-09-30"); expect_identical(r$cuts$last_data_day, "2026-09-28")
  expect_identical(r$load_label, "start"); expect_identical(r$timezone, "Etc/GMT+3")
  expect_identical(r$weather_issue_cutoff, "available_until"); expect_identical(r$code_commit, "abc1234")
  expect_identical(cli_json(st, "show", "random-forest", "SECO", "v_a1")$j$result$version, "v_a1")
  expect_identical(cli_json(st, "ls", "random-forest")$j$result[[1L]]$key, "random-forest/SECO")
  expect_identical(pc5_tree_hash(file.path(st, "artifacts")), before)
  miss <- cli_json(st, "show", "random-forest", "SUL")
  expect_identical(miss$r$status, 13L); expect_identical(miss$j$error$id, "state.cell_missing")
  txt <- cli(st, "ls")
  expect_identical(txt$status, 0L); pc_assert_ascii(txt$raw, "ls texto")
})

## ---- verbo auto-registrado e catalogo ------------------------------------------------------------------------------
test_that("o verbo registry e descoberto por cli.R e o Usage lista os 7 subcomandos", {
  cl <- pc_source_lib(c("errors", "config", "cli"))
  verbs <- cl$prevcarga_verbs_discover(dir = file.path(pc_root(), "scripts", "lib", "cli"))
  expect_true("registry" %in% verbs$verb)
  row <- verbs[verbs$verb == "registry", ]
  expect_identical(row$owner, "M5"); expect_true(is.function(row$handler[[1L]]))
  u <- cl$prevcarga_usage(verbs)
  expect_true(grepl("prevcarga registry (ls|show|promote|rollback|pin|unpin|gc)", u, fixed = TRUE))
  expect_identical(prevcarga_registry_subcommands, c("ls", "show", "promote", "rollback", "pin", "unpin", "gc"))
  a <- cl$.prevcarga_parse(c("registry", "gc", "--dry-run"), verbs)
  expect_true(isTRUE(a[["registry"]])); expect_true(isTRUE(a[["gc"]])); expect_true(isTRUE(a[["--dry-run"]]))
})

test_that("catalogo registry_lifecycle.tsv carrega com os demais TSV sem id duplicado; ASCII e LF", {
  cat <- prevcarga_errors_load()
  expect_false(anyDuplicated(cat$id) > 0)
  want <- c("state.registry_no_previous", "state.cell_missing", "state.registry_khat_backward")
  expect_true(all(want %in% cat$id))
  expect_true(all(cat$class[match(want, cat$id)] == "state"))
  expect_identical(unname(prevcarga_exit_codes["state"]), 13L)
  raw <- readBin(file.path(pc_root(), "scripts", "lib", "errors", "registry_lifecycle.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(as.raw(13L) %in% raw)
  expect_false(any(c("state.pin_missing", "state.promote_failed") %in%
                     utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "registry_lifecycle.tsv"))$id))
})
