## test-reconcile-determinism.R -- R.V: determinismo do verbo `reconcile --mode apply` POR PROCESSO. Quatro rodadas (2 sob ambiente
## limpo, 2 sob shell sujo: TZ, LC_ALL e OPENBLAS_NUM_THREADS errados no processo-filho) => sha256 de subsistemas/<ss>.parquet e
## SIN.parquet identico, run_id igual e --out default = <state_dir>/scratch/<run_id>/. runtime.R fixa TZ/locale/BLAS no processo.
## O manifesto de sha256/linhas desta plataforma e gravado em tempdir() para a comparacao Windows x Linux (<= 0,1% relativo).
## Linux: roda aqui. Windows nativo: [DEFERRED M8] reconcile-determinism: a bateria roda no Windows (dono: M8.1, matriz de plataforma),
## que compara o manifesto Windows com o do Linux; este arquivo nao tem skip por plataforma.
skip_if_not_installed("docopt")
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "topology"))
O <- "2026-01-15"
tmp <- withr::local_tempdir()

topo_yaml <- pc_topology_fixture(3L, dir = file.path(tmp, "topo"))
topo <- env$prevcarga_topology(path = topo_yaml, root = tempdir())
root <- pc_install_copy(mutate = function(r) file.copy(topo_yaml, file.path(r, "config", "topology.yaml"), overwrite = TRUE))
deck <- pc_deck_sentinel("start", days = lubridate::ymd(O) + (-45:0), areas = c(topo$order, topo$leaves), issues = 0L,
                         dir = file.path(tmp, "deck"))
state <- file.path(tmp, "state"); dir.create(state)
from <- file.path(tmp, "from"); dir.create(from)
leaves <- pc_leaves_long(topo, O, list(tz = pc_rc_tz))
for (a in unique(leaves$area)) arrow::write_parquet(leaves[leaves$area == a, , drop = FALSE], file.path(from, paste0(a, ".parquet")))

rc <- function(args, env = character()) pc_cli(root, c("reconcile", args, paste0("--data=", deck), paste0("--state=", state),
                                                      "--format=json"), env = env)
j1 <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)

tr <- rc(c("--mode=train", paste0("--origin=", O), "--khat-window=30"))
test_that("train prepara o perfil latest do state_dir (pre-condicao das 4 rodadas)", {
  expect_identical(tr$status, 0L, info = paste(tr$stderr, collapse = "\n"))
})

dirty <- c(TZ = "America/Sao_Paulo", LC_ALL = if (is.na(pc_dirty_lc_all())) "C" else pc_dirty_lc_all(),
           OPENBLAS_NUM_THREADS = "4")
run <- function(env = character()) {
  r <- rc(c("--mode=apply", "--subsistema=all", paste0("--leaves-dir=", from)), env = env)
  j <- j1(r)
  list(status = r$status, stderr = r$stderr, run_id = j$run_id,
       sha = stats::setNames(vapply(j$result$arquivos, function(a) a$sha256, ""),
                             vapply(j$result$arquivos, function(a) a$path, "")))
}
runs <- list(clean1 = run(), clean2 = run(), dirty1 = run(dirty), dirty2 = run(dirty))

test_that("AC2 4 rodadas (2 limpas, 2 sob shell sujo): exit 0, run_id e sha256 por arquivo identicos", {
  for (nm in names(runs)) {
    expect_identical(runs[[nm]]$status, 0L, info = nm)
    expect_identical(runs[[nm]]$stderr, character(0), info = nm)
  }
  r1 <- runs[[1L]]
  expect_match(r1$run_id, "^[0-9a-f]{16}$")
  expect_setequal(names(r1$sha), c("SIN.parquet", file.path("subsistemas", paste0(topo$order, ".parquet"))))
  for (x in runs[-1L]) {
    expect_identical(x$run_id, r1$run_id)
    expect_identical(x$sha, r1$sha)
  }
})

test_that("AC2 --out default = <state_dir>/scratch/<run_id>/ e o conteudo em disco bate com os sha256 do envelope", {
  sc <- file.path(state, "scratch", runs[[1L]]$run_id)
  expect_true(dir.exists(sc))
  for (p in names(runs[[1L]]$sha))
    expect_identical(digest::digest(file = file.path(sc, p), algo = "sha256"), runs[[1L]]$sha[[p]], info = p)
  expect_false(any(grepl("^\\.tmp-", list.files(file.path(state, "scratch"), all.files = TRUE))))
})

test_that("AC2 CONTROL: o ambiente sujo e de fato diferente do limpo e uma entrada diferente muda o run_id", {
  expect_false(identical(dirty[["TZ"]], Sys.timezone()) && identical(dirty[["OPENBLAS_NUM_THREADS"]], Sys.getenv("OPENBLAS_NUM_THREADS")))
  from2 <- file.path(tmp, "from2"); dir.create(from2)
  l2 <- leaves; l2$y_pred[l2$area == topo$leaves[[1L]]] <- l2$y_pred[l2$area == topo$leaves[[1L]]] + 1
  for (a in unique(l2$area)) arrow::write_parquet(l2[l2$area == a, , drop = FALSE], file.path(from2, paste0(a, ".parquet")))
  r <- rc(c("--mode=apply", "--subsistema=all", paste0("--leaves-dir=", from2)))
  expect_identical(r$status, 0L)
  j <- j1(r)
  expect_false(identical(j$run_id, runs[[1L]]$run_id))
  expect_false(identical(stats::setNames(vapply(j$result$arquivos, function(a) a$sha256, ""),
                                         vapply(j$result$arquivos, function(a) a$path, "")), runs[[1L]]$sha))
})

test_that("AC2 manifesto de plataforma (sha256 + linhas + soma de y_pred) gravado em tempdir para a comparacao Windows x Linux", {
  sc <- file.path(state, "scratch", runs[[1L]]$run_id)
  man <- do.call(rbind, lapply(names(runs[[1L]]$sha), function(p) {
    d <- as.data.frame(arrow::read_parquet(file.path(sc, p)))
    data.frame(path = p, sha256 = runs[[1L]]$sha[[p]], rows = nrow(d), sum_y_pred = sum(d$y_pred), stringsAsFactors = FALSE)
  }))
  f <- file.path(tempdir(), sprintf("reconcile-determinism-%s.json", tolower(.Platform$OS.type)))
  writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, digits = NA), f)
  expect_true(file.exists(f))
  expect_true(all(man$rows > 0L))
  ## comparacao entre plataformas: mesmo numero de linhas e delta relativo <= 0,1% (aqui contra si mesmo = 0)
  back <- jsonlite::fromJSON(f)
  expect_identical(back$rows, man$rows)
  expect_lte(max(abs(back$sum_y_pred - man$sum_y_pred) / man$sum_y_pred), 1e-3)
})
