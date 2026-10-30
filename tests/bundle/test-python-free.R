## test-python-free.R -- gates de higiene do bundle (M8.1; AC 3 e 4). Gate [BUNDLE] python_free + net_grep.
## FORBIDDEN: stage real do code/ (e o tarball candidato de dist/, quando existe) com zero linhas. CONTROL: injecao num stage copiado.

e <- bb_env()

test_that("[FORBIDDEN] [BUNDLE] python_free: o stage do bundle (code/ real) nao tem Python nem rede/env fora de runtime.R", {
  root <- bb_stage(e)
  expect_equal(nrow(e$pc_bundle_python_free(root)), 0L)
  expect_equal(nrow(e$pc_bundle_net_grep(file.path(root, "code"))), 0L)
})

test_that("[FORBIDDEN] [BUNDLE] python_free: o tarball candidato extraido (dist/) passa os dois gates", {
  tb <- bb_dist_tarball()
  skip_if(is.na(tb), "sem tarball em dist/: rode scripts/tools/build_bundle_linux.sh <v> (braco do container nao executado)")
  skip_if(!nzchar(Sys.which("zstd")), "zstd ausente no host")
  root <- bb_extract(tb)
  expect_identical(e$pc_bundle_python_free(root)$path, character())
  expect_equal(nrow(e$pc_bundle_net_grep(file.path(root, "code"))), 0L)
})

test_that("[CONTROL] [BUNDLE] python_free: x.py injetado em code/scripts e listado exatamente", {
  root <- bb_stage(e)
  writeLines("print(1)", file.path(root, "code", "scripts", "x.py"))
  h <- e$pc_bundle_python_free(root)
  expect_identical(h$path, "code/scripts/x.py")
})

test_that("[CONTROL] [BUNDLE] python_free: cada padrao (python*, libpython*, pyvenv.cfg, uv.lock, pyproject.toml) e acusado onde estiver", {
  root <- bb_stage(e)
  plant <- c("runtime/bin/python3", "syslib/libpython3.10.so.1.0", "runtime/pyvenv.cfg", "code/uv.lock", "code/pyproject.toml")
  for (p in plant) { dir.create(dirname(file.path(root, p)), recursive = TRUE, showWarnings = FALSE); writeLines("x", file.path(root, p)) }
  h <- e$pc_bundle_python_free(root)
  expect_identical(h$path, sort(plant, method = "radix"))
  expect_setequal(h$motivo, c("python", "libpython", "pyvenv", "uv_lock", "pyproject"))
})

test_that("[CONTROL] [BUNDLE] net_grep: download.file( injetado num .R do code/ e acusado com arquivo e linha", {
  root <- bb_stage(e)
  f <- file.path(root, "code", "scripts", "lib", "dates.R")
  l <- readLines(f)
  writeLines(c(l[1:3], "x <- download.file('http://exemplo.invalido/x', 'y')", l[-(1:3)]), f)
  h <- e$pc_bundle_net_grep(file.path(root, "code"))
  expect_identical(h$path, "scripts/lib/dates.R")
  expect_identical(h$linha, 4L)
  expect_match(h$texto, "download.file(", fixed = TRUE)
})

test_that("[CONTROL] [BUNDLE] net_grep: s3://, aws s3, paws, httr, curl::, Sys.getenv; runtime.R isento; comentario inteiro nao conta", {
  root <- bb_stage(e)
  cd <- file.path(root, "code")
  bait <- c("s3://b/k", "aws s3 sync", "paws::s3()", "httr::GET(u)", "curl::curl_fetch_memory(u)", "Sys.getenv('X')")
  writeLines(paste0("f", seq_along(bait), " <- function() ", sub("^(.*)$", "\"\\1\"", bait)), file.path(cd, "scripts", "lib", "iscas.R"))
  h <- e$pc_bundle_net_grep(cd)
  expect_identical(h$path, rep("scripts/lib/iscas.R", length(bait)))
  expect_identical(h$linha, seq_along(bait))
  ## a isencao e so de scripts/lib/runtime.R (que de fato usa Sys.getenv)
  expect_true(any(grepl("Sys.getenv", readLines(file.path(cd, "scripts", "lib", "runtime.R")), fixed = TRUE)))
  expect_false("scripts/lib/runtime.R" %in% e$pc_bundle_net_grep(cd)$path)
  ## comentario inteiro e prosa (deck_reader.R cita Sys.getenv no cabecalho): nao conta; chamada com comentario no fim conta
  writeLines(c("## nao usa Sys.getenv nem download.file", "x <- 1  # ok", "y <- Sys.getenv('A')  # chamada real"),
             file.path(cd, "scripts", "lib", "iscas.R"))
  h2 <- e$pc_bundle_net_grep(cd)
  expect_identical(h2$linha, 3L)
})

test_that("[CONTROL] [BUNDLE] net_grep: scripts/prevcarga (sem extensao .R) tambem e varrido", {
  root <- bb_stage(e)
  f <- file.path(root, "code", "scripts", "prevcarga")
  cat("x <- curl::curl_fetch_memory('u')\n", file = f, append = TRUE)
  h <- e$pc_bundle_net_grep(file.path(root, "code"))
  expect_identical(h$path, "scripts/prevcarga")
})
