## test-reconcile-cli.R -- R.5: o verbo `reconcile` POR PROCESSO (Rscript --vanilla <copia>/scripts/prevcarga via pc_cli).
## Instalacao-copia em tempdir com topologia-fixture (SS1..SS3, folhas Ls<i>_<j>) e deck sentinela de rotulo `start`.
## Nada de literal da topologia real; nada em data/. Unico skip permitido: docopt ausente.
skip_if_not_installed("docopt")
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "topology"))
O <- "2026-01-15"
tmp <- withr::local_tempdir()

topo_yaml <- pc_topology_fixture(3L, dir = file.path(tmp, "topo"))
topo <- env$prevcarga_topology(path = topo_yaml, root = tempdir())
root <- pc_install_copy(mutate = function(r) file.copy(topo_yaml, file.path(r, "config", "topology.yaml"), overwrite = TRUE))
areas <- c(topo$order, topo$leaves)
deck <- pc_deck_sentinel("start", days = lubridate::ymd(O) + (-45:0), areas = areas, issues = 0L, dir = file.path(tmp, "deck"))

ascii <- function(x) !any(as.integer(unlist(lapply(x, charToRaw))) > 127L)
j1 <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
rc <- function(state, args, fmt = "json") pc_cli(root, c("reconcile", args, paste0("--data=", deck), paste0("--state=", state),
                                                          paste0("--format=", fmt)))
new_state <- function() { d <- tempfile("state-", tmpdir = tmp); dir.create(d); d }
write_from <- function(long, dir) {
  dir.create(dir, showWarnings = FALSE)
  for (a in unique(long$area)) arrow::write_parquet(long[long$area == a, , drop = FALSE], file.path(dir, paste0(a, ".parquet")))
  dir
}
ctx0 <- list(tz = pc_rc_tz)
leaves <- pc_leaves_long(topo, O, ctx0)
from <- write_from(leaves, file.path(tmp, "from"))
ls_files <- function(d) if (dir.exists(d)) list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE) else character()
envelope_keys <- c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version")

## ---- AC5: train pela CLI promove o latest; SIN sem perfil -------------------------------------------------------
st <- new_state()
tr30 <- rc(st, c("--mode=train", paste0("--origin=", O), "--khat-window=30"))
test_that("AC5 train: exit 0, khat por subsistema promovido, latest.txt existe, SIN sem perfil", {
  expect_identical(tr30$status, 0L, info = paste(tr30$stderr, collapse = "\n"))
  expect_identical(tr30$stderr, character(0)); expect_length(tr30$stdout, 1L)
  j <- j1(tr30)
  expect_true(all(envelope_keys %in% names(j))); expect_identical(j$verb, "reconcile")
  ss <- vapply(j$result$khat, function(k) k$ss, "")
  expect_identical(ss, topo$order)
  for (k in j$result$khat) {
    expect_match(k$versao, "^v_[0-9a-f]{16}$"); expect_identical(k$khat_origin, O)
    expect_identical(k$window_days, 30L); expect_true(k$promovido)
    expect_true(all(c("min", "max", "fora_faixa") %in% names(k$k_hat_resumo)))
    expect_true(file.exists(file.path(st, "artifacts", "reconcile", k$ss, "latest.txt")))
  }
  expect_false("SIN" %in% ss)
  expect_false(dir.exists(file.path(st, "artifacts", "reconcile", "SIN")))
})
v1 <- vapply(j1(tr30)$result$khat, function(k) k$versao, "")

## ---- AC2: gramatica; nada em disco ------------------------------------------------------------------------------
test_that("AC2 flags invalidas => exit 8 sem tocar disco; CONTROL --origin aceito; reconcile nao e stub", {
  s <- new_state()
  bad <- list(c("--mode=train", "--eval"), "--mode=xyz", c("--mode=apply", "--khat-window=0"),
              c("--mode=train", paste0("--leaves-dir=", from)), c("--mode=train", "--khat-version=v_x"), character())
  for (a in bad) {
    r <- rc(s, a, "text")
    expect_identical(r$status, 8L, info = paste(a, collapse = " "))
    expect_true(any(grepl("[prevcarga/config]|erro [config]", r$stderr, fixed = FALSE) | grepl("config", r$stderr)))
    expect_identical(r$stdout, character(0))
  }
  expect_length(ls_files(s), 0L)
  r <- rc(s, c("--mode=apply", paste0("--origin=", O), "--subsistema=all", paste0("--leaves-dir=", from), paste0("--out=", file.path(tmp, "o-ctl"))))
  expect_false(identical(r$status, 8L), info = paste(r$stderr, collapse = "\n"))
  other <- pc_cli(root, c("score", "--eval"))
  expect_identical(other$status, 8L)                    # --eval so vale em reconcile (scoped)
  h <- pc_cli(root, "--help")
  expect_true(any(grepl("prevcarga reconcile", h$stdout, fixed = TRUE)))
  expect_false(any(grepl("todo.not_implemented", c(r$stdout, r$stderr))))
})

## ---- AC1: apply, XX e sem --mode: JSON unico, stderr vazio, exits iguais ao texto, ASCII -------------------------
test_that("AC1 envelope JSON unico + exits 0/4/8 iguais ao modo texto, ASCII", {
  cases <- list(ok = list(c("--mode=apply", paste0("--leaves-dir=", from)), 0L, NA),
                xx = list(c("--mode=apply", paste0("--leaves-dir=", from), "--subsistema=XX"), 4L, "topology.area_undeclared"),
                nomode = list(c(paste0("--leaves-dir=", from)), 8L, "config.usage"))
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    rj <- rc(st, cs[[1L]], "json"); rt <- rc(st, cs[[1L]], "text")
    expect_identical(rj$status, cs[[2L]], info = nm); expect_identical(rt$status, cs[[2L]], info = nm)
    expect_identical(rj$stderr, character(0), info = nm); expect_length(rj$stdout, 1L)
    expect_true(ascii(c(rj$stdout, rj$stderr, rt$stdout, rt$stderr)), info = nm)
    j <- j1(rj)
    expect_true(all(envelope_keys %in% names(j)), info = nm)
    if (nm != "nomode") expect_identical(j$verb, "reconcile")   # docopt recusa antes de o verbo ser conhecido
    expect_identical(j$exit_code, cs[[2L]])
    if (cs[[2L]] == 0L) { expect_true(j$ok); expect_null(j$error); expect_identical(rt$stdout[[length(rt$stdout)]], "OK") }
    else {
      expect_false(j$ok); expect_identical(j$error$id, cs[[3L]], info = nm)
      expect_identical(rt$stdout, character(0), info = nm); expect_gt(length(rt$stderr), 0L)
    }
  }
})

## ---- AC3: scratch default, run_id e sha256 estaveis; CONTROL --khat-version ---------------------------------------
test_that("AC3 --out default no scratch: run_id e sha256 estaveis; --khat-version de outra versao muda o run_id", {
  a1 <- rc(st, c("--mode=apply", paste0("--leaves-dir=", from), "--subsistema=all"))
  a2 <- rc(st, c("--mode=apply", paste0("--leaves-dir=", from), "--subsistema=all"))
  expect_identical(a1$status, 0L, info = paste(a1$stderr, collapse = "\n")); expect_identical(a2$status, 0L)
  j <- j1(a1); j2 <- j1(a2)
  expect_match(j$run_id, "^[0-9a-f]{16}$"); expect_identical(j$run_id, j2$run_id)
  sc <- file.path(st, "scratch", j$run_id)
  want <- c("SIN.parquet", file.path("subsistemas", paste0(topo$order, ".parquet")))
  expect_setequal(ls_files(sc), want)
  expect_identical(ls_files(file.path(st, "scratch"))[0], character(0))
  expect_false(any(grepl("^\\.tmp-", list.files(file.path(st, "scratch"), all.files = TRUE))))
  for (a in j$result$arquivos) {
    sha <- digest::digest(file = file.path(sc, a$path), algo = "sha256")
    expect_identical(sha, a$sha256)
  }
  expect_identical(vapply(j$result$arquivos, function(a) a$sha256, ""), vapply(j2$result$arquivos, function(a) a$sha256, ""))
  expect_true(j$result$sin)
  ## CONTROL: um segundo perfil (janela 20) vira o latest; fixar a versao antiga restaura o run_id original
  tr20 <- rc(st, c("--mode=train", paste0("--origin=", O), "--khat-window=20"))
  expect_identical(tr20$status, 0L)
  v2 <- vapply(j1(tr20)$result$khat, function(k) k$versao, "")
  expect_false(any(v2 == v1))
  jd <- j1(rc(st, c("--mode=apply", paste0("--leaves-dir=", from))))
  expect_false(identical(jd$run_id, j$run_id))
  jp <- j1(rc(st, c("--mode=apply", paste0("--leaves-dir=", from), paste0("--khat-version=", v1[[1L]]), "--subsistema=SS1")))
  expect_identical(jp$result$khat[[1L]]$versao, v1[[1L]])
  expect_false(jp$result$sin)
})

## ---- AC4: contrato violado nao deixa nada em --out nem .tmp-* ------------------------------------------------
test_that("AC4 folhas com lead divergente => exit 2 contract.reconcile_input; --out vazio e sem .tmp-*", {
  bad <- leaves
  i <- which(bad$area == "Ls1_1")[[3L]]; bad$lead[[i]] <- bad$lead[[i]] + 1L
  bfrom <- write_from(bad, file.path(tmp, "from-bad"))
  out <- file.path(tmp, "out-bad"); dir.create(out)
  r <- rc(st, c("--mode=apply", paste0("--leaves-dir=", bfrom), paste0("--out=", out)))
  expect_identical(r$status, 2L); expect_identical(r$stderr, character(0))
  j <- j1(r)
  expect_identical(j$error$id, "contract.reconcile_input"); expect_match(j$error$where, "Ls1_1", fixed = TRUE)
  expect_length(ls_files(out), 0L)
  expect_false(any(grepl("^\\.tmp-", list.files(tmp, all.files = TRUE))))
})
