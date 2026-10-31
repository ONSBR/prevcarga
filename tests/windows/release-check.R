## release-check.R -- braco Windows do Python-free da release (M8.12; item W17 do gate da VM Windows). Roda pelo Rscript.exe do bundle INSTALADO
## (so R base + jsonlite/digest do proprio bundle; sem testthat, que o bundle nao traz): mesmos gates de tests/bundle/test-python-free-release.R sobre
## <versions\<v>> do bundle windows-x86_64 instalado. Sem variavel de ambiente, sem rede. Sai 0 (PASS) ou 1 (FAIL), com 1 linha por verificacao.
##   Rscript.exe --vanilla tests\windows\release-check.R --repo=<gate ou repo> --bundle-root=<...\versions\<v>> [--expect-commit=<sha40>] [--platform=<p>, default windows-x86_64]
## [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NAO executado nesta maquina Linux.
args <- commandArgs(TRUE)
arg <- function(n, d = NULL) { h <- grep(paste0("^--", n, "="), args, value = TRUE); if (length(h)) sub(paste0("^--", n, "="), "", h[[1L]]) else d }
repo <- normalizePath(arg("repo"), winslash = "/", mustWork = TRUE)
root <- normalizePath(arg("bundle-root"), winslash = "/", mustWork = TRUE)
code <- file.path(root, "code")
e <- new.env(parent = globalenv())
options(prevcarga.lib = file.path(code, "scripts", "lib"))
sys.source(file.path(code, "scripts", "lib", "errors.R"), envir = e)
sys.source(file.path(code, "scripts", "lib", "runtime.R"), envir = e)
sys.source(file.path(repo, "scripts", "tools", "bundle_gates.R"), envir = e)
fails <- 0L
check <- function(nome, ok, detalhe = "") { if (!isTRUE(ok)) fails <<- fails + 1L; cat(sprintf("[%s] %s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", nome, detalhe)) }
py <- e$pc_bundle_python_free(root)
check("python_free", nrow(py) == 0L, paste(utils::head(py$path, 5L), collapse = ","))
ng <- e$pc_bundle_net_grep(code)
check("net_grep", nrow(ng) == 0L, paste(utils::head(paste0(ng$path, ":", ng$linha), 5L), collapse = ","))
pg <- e$pc_bundle_portability_grep(code)
check("portabilidade", nrow(pg) == 0L, paste(utils::head(paste0(pg$path, ":", pg$linha), 5L), collapse = ","))
rj <- jsonlite::read_json(file.path(root, "RUNTIME.json"))
check("r_version", identical(rj$r_version, "4.5.3"), as.character(rj$r_version))
plat <- arg("platform", "windows-x86_64")
check("platform", identical(rj$platform, plat), as.character(rj$platform))
ec <- arg("expect-commit")
if (!is.null(ec)) check("code_commit", identical(rj$code_commit, ec), paste(rj$code_commit, "esperado", ec))
v <- e$prevcarga_runtime_validate(root)
check("runtime_json", v$ok, paste(utils::head(v$divergentes, 5L), collapse = ","))
cat(sprintf("code_commit=%s runtime_id=%s\n", rj$code_commit, e$prevcarga_runtime_id(file.path(root, "RUNTIME.json"))))
quit(status = if (fails == 0L) 0L else 1L, save = "no")
