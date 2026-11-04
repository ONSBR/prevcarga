## bundle_manifest.R -- ferramenta de DEV (fora do bundle), compartilhada pelos builds Linux e Windows (M8.1).
## Funcoes: pc_code_tree (code/ do bundle) e pc_runtime_json_write (RUNTIME.json). O formato canonico e o runtime_id
## vem de scripts/lib/runtime.R (fonte unica, a mesma do doctor). Sem relogio: `built_at` so entra se o chamador o
## informar (o build o deriva do commit); fora do runtime_id.
##
## CLI (Rscript --vanilla scripts/tools/bundle_manifest.R <sub> ...):
##   code-tree --src <repo> --dest <dir>
##   write --root <raiz do bundle> --version <v> --platform <linux-x86_64|windows-x86_64> --code-commit <sha>
##         [--apt-snapshot <AAAAMMDDTHHMMSSZ>] [--built-at <ISO8601Z>] [--compilados <arquivo: 1 pacote por linha>]
##         [--compilados-tsv <pacote/versao/motivo, com cabecalho>]
##   restore-windows --lock <renv.lock> --lib <library do R do bundle> --repos-bin <raiz P3M> --repos-src <raiz P3M>
##         [--hashes <scripts/tools/windows_binaries.tsv>] [--out-compilados <tsv>]      (so no host Windows de build)
##   zip --stage <dir> --out <arq.zip> --epoch <SOURCE_DATE_EPOCH>
##   validate --root <raiz do bundle>
##   (qualquer sub aceita --tools-lib <dir>: pacotes do build, ex. zip, jsonlite, digest, fora do lock/bundle)
## Quando carregado por source() (testes) so define as funcoes.

.pc_bm_lib <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) == 1L && grepl("bundle_manifest\\.R$", f))
    return(file.path(dirname(dirname(normalizePath(f, winslash = "/"))), "lib"))
  getOption("prevcarga.lib")
}

pc_code_tree <- function(src, dest) {
  dirs <- c("scripts", "models", "config")
  fs <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(file.path(src, d))) return(character())
    file.path(d, list.files(file.path(src, d), recursive = TRUE, all.files = TRUE))
  }))
  fs <- c(fs, "renv.lock", "LICENSE", "NOTICE")                   # Apache-2.0: a licenca e o NOTICE viajam com o bundle
  drop <- grepl("^scripts/(tools|data-prep)/|(^|/)tests/|\\.md$|^models/[^/]+/models/", fs)
  for (f in fs[!drop]) {
    dir.create(dirname(file.path(dest, f)), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(src, f), file.path(dest, f), copy.date = FALSE))
  }
  invisible(sort(fs[!drop], method = "radix"))
}

pc_runtime_json_write <- function(root, version, platform, code_commit, apt_snapshot = NULL, compilados = list(),
                                  built_at = NULL, r_version = paste(R.version$major, R.version$minor, sep = ".")) {
  lock <- file.path(root, "code", "renv.lock")
  stopifnot(file.exists(lock))
  lock_r <- jsonlite::read_json(lock)$R$Version
  if (!identical(lock_r, r_version))
    stop(sprintf("R do build (%s) difere de R.Version do renv.lock (%s)", r_version, lock_r), call. = FALSE)
  rel <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  rel <- sort(setdiff(gsub("\\", "/", rel, fixed = TRUE), "RUNTIME.json"), method = "radix")
  dang <- rel[!file.exists(file.path(root, rel))]
  if (length(dang)) stop("link pendente ou arquivo ilegivel no stage: ", paste(dang, collapse = ", "), call. = FALSE)
  files <- lapply(rel, function(p) list(path = p, sha256 = .prevcarga_runtime_file_sha(file.path(root, p))))
  x <- list(version = version, platform = platform, r_version = r_version,
            renv_lock_hash = digest::digest(file = lock, algo = "sha256"),
            code_commit = code_commit, apt_snapshot = apt_snapshot, compilados = pc_compilados_as_list(compilados),
            built_at = built_at, files = files)
  ## o arquivo carrega built_at (lateral, ausente se nao informado); o runtime_id o ignora
  canon <- prevcarga_runtime_json_canonical(x, with_built_at = TRUE)
  out <- file.path(root, "RUNTIME.json")
  con <- file(out, open = "wb"); writeBin(charToRaw(canon), con); close(con)
  id <- prevcarga_runtime_id(out)
  cat(id, "\n", sep = "")
  invisible(id)
}

## compilados: data.frame(pacote, versao, motivo) -> lista de objetos ordenada por pacote (o formato do RUNTIME.json).
pc_compilados_as_list <- function(x) {
  if (!is.data.frame(x)) return(as.list(x))
  x <- x[order(x$pacote, method = "radix"), , drop = FALSE]
  lapply(seq_len(nrow(x)), function(i)
    list(pacote = x$pacote[i], versao = x$versao[i], motivo = x$motivo[i]))
}

## --- bundle Windows (M8.2; ADR-0038) ----------------------------------------------------------------------------
## Restaura o renv.lock no `lib` do R 4.5.3 do bundle a partir de BINARIOS CRAN para Windows (sem compilar quando ha binario).
## `repos_bin`/`repos_src` = raizes do snapshot P3M pinado (o mesmo do lock). `hashes` = tabela pacote/versao/sha256 pinada
## (scripts/tools/windows_binaries.tsv): o zip baixado e conferido ANTES de instalar. Pacote sem binario => fonte, so com
## Rtools45 (pkgbuild::has_build_tools), e entra em `compilados`. Ao fim confere o Version instalado de cada pacote contra o lock.
## `fetch(url, dest)` e `installer(path, lib, type)` sao injetaveis (testes no Linux); os defaults sao os reais do host Windows.
pc_lock_install_windows <- function(lock, lib, repos_bin, repos_src, hashes = NULL,
                                    fetch = function(url, dest) utils::download.file(url, dest, mode = "wb", quiet = TRUE),
                                    installer = function(path, lib, type)
                                      utils::install.packages(path, lib = lib, repos = NULL, type = type, quiet = TRUE)) {
  pk <- jsonlite::read_json(lock)$Packages
  pk <- pk[order(names(pk), method = "radix")]
  rv <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("pcwin"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  comp <- data.frame(pacote = character(), versao = character(), motivo = character(), stringsAsFactors = FALSE)
  for (p in pk) {
    zf <- file.path(tmp, sprintf("%s_%s.zip", p$Package, p$Version))
    got <- tryCatch({
      fetch(sprintf("%s/bin/windows/contrib/%s/%s_%s.zip", sub("/+$", "", repos_bin), rv, p$Package, p$Version), zf)
      file.exists(zf) && file.size(zf) > 0
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (got) {
      if (!is.null(hashes)) {
        h <- hashes$sha256[hashes$pacote == p$Package & hashes$versao == p$Version]
        if (length(h) != 1L) stop(sprintf("sem hash pinado para %s %s", p$Package, p$Version), call. = FALSE)
        if (!identical(digest::digest(file = zf, algo = "sha256"), tolower(h)))
          stop(sprintf("sha256 do binario %s %s difere do pinado (integrity)", p$Package, p$Version), call. = FALSE)
      }
      installer(zf, lib, "win.binary")
    } else {
      if (!isTRUE(tryCatch(pkgbuild::has_build_tools(debug = FALSE), error = function(e) FALSE)))
        stop(sprintf("%s %s sem binario CRAN e sem Rtools45: abortando", p$Package, p$Version), call. = FALSE)
      sf <- file.path(tmp, sprintf("%s_%s.tar.gz", p$Package, p$Version))
      fetch(sprintf("%s/src/contrib/%s_%s.tar.gz", sub("/+$", "", repos_src), p$Package, p$Version), sf)
      installer(sf, lib, "source")
      comp[nrow(comp) + 1L, ] <- list(p$Package, p$Version, "sem binario CRAN para R 4.5.3")
    }
  }
  bad <- vapply(pk, function(p) {
    ## packageDescription devolve o Version como escrito ("1.7-17"); packageVersion normalizaria "-" para "." e mascararia a troca
    v <- tryCatch(utils::packageDescription(p$Package, lib.loc = lib, fields = "Version"), error = function(e) NA, warning = function(w) NA)
    v <- if (is.na(v)) "<ausente>" else as.character(v)
    if (identical(v, p$Version)) "" else sprintf("%s: instalado %s, lock %s", p$Package, v, p$Version)
  }, "")
  bad <- bad[nzchar(bad)]
  if (length(bad)) stop("versao instalada difere do renv.lock: ", paste(bad, collapse = "; "), call. = FALSE)
  comp[order(comp$pacote, method = "radix"), , drop = FALSE]
}

## Zip reprodutivel: arquivos ordenados por path (separador /), mtime fixo no `epoch` (SOURCE_DATE_EPOCH do commit, passado
## como argumento), zip::zip(mode = "mirror") sem depender de zip.exe do sistema. Devolve o sha256 do zip.
pc_zip_reproducible <- function(stage, out, epoch) {
  fs <- sort(list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
  t0 <- .POSIXct(as.numeric(epoch), tz = "UTC")
  invisible(Sys.setFileTime(file.path(stage, fs), t0))
  unlink(out)
  zip::zip(zipfile = normalizePath(out, winslash = "/", mustWork = FALSE), files = fs, root = stage,
           mode = "mirror", recurse = FALSE, include_directories = FALSE)
  digest::digest(file = out, algo = "sha256")
}

.pc_bm_main <- function(argv) {
  lib <- .pc_bm_lib()
  source(file.path(lib, "errors.R")); source(file.path(lib, "runtime.R"))
  opt <- function(k, default = NULL) { i <- which(argv == paste0("--", k)); if (length(i)) argv[i[1L] + 1L] else default }
  sub <- argv[1L]
  if (!is.null(tl <- opt("tools-lib"))) .libPaths(c(tl, .libPaths()))   # pacotes do BUILD (jsonlite, digest, zip), fora do bundle
  if (identical(sub, "code-tree")) {
    n <- pc_code_tree(opt("src"), opt("dest")); cat(sprintf("code-tree: %d arquivos\n", length(n)))
  } else if (identical(sub, "write")) {
    comp <- list()
    if (!is.null(f <- opt("compilados")) && file.exists(f)) {   # 1 pacote por linha; versao vem do renv.lock empacotado
      pk <- readLines(f, warn = FALSE); pk <- pk[nzchar(pk)]
      lk <- jsonlite::read_json(file.path(opt("root"), "code", "renv.lock"))$Packages
      comp <- lapply(pk, function(p) list(pacote = p, versao = lk[[p]]$Version, motivo = "sem binario P3M jammy; compilado do fonte"))
    }
    if (!is.null(f <- opt("compilados-tsv")) && file.exists(f))   # saida do restore-windows: pacote<TAB>versao<TAB>motivo
      comp <- utils::read.delim(f, colClasses = "character", quote = "")
    pc_runtime_json_write(opt("root"), opt("version"), opt("platform"), opt("code-commit"), opt("apt-snapshot"),
                          compilados = comp, built_at = opt("built-at"))
  } else if (identical(sub, "restore-windows")) {
    hs <- if (!is.null(f <- opt("hashes"))) utils::read.delim(f, colClasses = "character", quote = "") else NULL
    comp <- pc_lock_install_windows(opt("lock"), opt("lib"), opt("repos-bin"), opt("repos-src"), hashes = hs)
    if (!is.null(f <- opt("out-compilados")))
      utils::write.table(comp, f, sep = "\t", quote = FALSE, row.names = FALSE, eol = "\n")
    cat(sprintf("restore-windows: lock restaurado em %s; compilados: %d\n", opt("lib"), nrow(comp)))
  } else if (identical(sub, "zip")) {
    cat(pc_zip_reproducible(opt("stage"), opt("out"), opt("epoch")), "\n", sep = "")
  } else if (identical(sub, "validate")) {   # recalcula o sha256 de todo files[] (bundle.manifest_mismatch, exit 6, se divergir)
    v <- prevcarga_runtime_validate(opt("root"), strict = TRUE)
    cat(sprintf("validate: ok=%s\n", v$ok))
  } else stop("uso: bundle_manifest.R code-tree|write|restore-windows|zip|validate ...", call. = FALSE)
}

if (identical(environment(), globalenv()) && length(grep("--file=.*bundle_manifest\\.R$", commandArgs(FALSE))))
  .pc_bm_main(commandArgs(TRUE))
