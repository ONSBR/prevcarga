## test-emendas.R -- gate de emendas do M8 (M8.12; AC 1). `git diff --name-status m8-base` sobre os testes de todos os epicos
## (scripts/lib/tests, models/*/tests, tests): nenhuma linha D nem R, e toda linha M listada em tests/bundle/emendas_m8.txt (A e livre).
## CONTROL: num clone temporario, modificar/apagar/renomear um teste de outro epico fora da lista faz o gate citar o caminho.
## Sem a tag m8-base o gate FALHA com motivo (nunca skip): sem a base nao ha o que comparar.

em_root <- bb_root()
EM_PATHSPEC <- c("scripts/lib/tests", ":(glob)models/*/tests", "tests")

em_git <- function(repo, args) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(repo), args), stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), out = as.character(out))
}

## Lista de emendas: uma por linha `<caminho>  # <motivo>`; `#` no inicio e comentario; linha vazia ignorada.
em_parse_list <- function(lines) {
  l <- trimws(lines[!grepl("^\\s*(#|$)", lines)])
  data.frame(path = sub("\\s+#.*$", "", l), motivo = ifelse(grepl("#", l, fixed = TRUE), trimws(sub("^[^#]*#", "", l)), ""),
             stringsAsFactors = FALSE)
}

## Entrada do gate: `git diff --name-status -M m8-base` -> data.frame(status, path); renomeacao = R<nnn> com o caminho de destino.
em_diff <- function(repo) {
  tag <- em_git(repo, c("rev-parse", "-q", "--verify", "refs/tags/m8-base"))
  if (tag$status != 0L) return(list(ok = FALSE, motivo = "tag m8-base ausente: crie-a no commit em que a branch do M8 nasceu (git tag m8-base <commit>)"))
  d <- em_git(repo, c("diff", "--name-status", "-M", "m8-base", "--", shQuote(EM_PATHSPEC)))
  if (d$status != 0L) return(list(ok = FALSE, motivo = paste("git diff falhou:", paste(d$out, collapse = " "))))
  p <- strsplit(d$out[nzchar(d$out)], "\t", fixed = TRUE)
  rows <- data.frame(status = vapply(p, `[`, "", 1L), path = vapply(p, function(x) x[[length(x)]], ""), stringsAsFactors = FALSE)
  rows <- rows[grepl("(^|/)tests/", rows$path), , drop = FALSE]
  list(ok = TRUE, rows = rows)
}

## Violacoes: D/R sempre; M fora da lista. Devolve data.frame(path, status, motivo) (vazio = PASS).
em_violations <- function(rows, listed) {
  bad_dr <- rows[grepl("^[DR]", rows$status), , drop = FALSE]
  bad_m <- rows[rows$status == "M" & !(rows$path %in% listed), , drop = FALSE]
  out <- rbind(if (nrow(bad_dr)) data.frame(path = bad_dr$path, status = bad_dr$status, motivo = "D/R proibido", stringsAsFactors = FALSE),
               if (nrow(bad_m)) data.frame(path = bad_m$path, status = bad_m$status, motivo = "M fora de emendas_m8.txt", stringsAsFactors = FALSE))
  if (is.null(out)) out <- data.frame(path = character(), status = character(), motivo = character(), stringsAsFactors = FALSE)
  out[order(out$path, method = "radix"), , drop = FALSE]
}

## Windows nativo sem git (a VM do gate e limpa): o gate de emendas e do host de dev; skip EXPLICITO com dono, nunca verde silencioso
em_skip_sem_git <- function() testthat::skip_if(!nzchar(Sys.which("git")), "git ausente neste host: gate de emendas (git diff m8-base) roda no host de dev [DEFERRED windows-vm do M8, dono: Gabriel; W16]")

em_list_file <- file.path(em_root, "tests", "bundle", "emendas_m8.txt")
em_listed <- em_parse_list(readLines(em_list_file, warn = FALSE))

test_that("[AC] emendas_m8.txt: ASCII, um caminho de teste por linha, com motivo", {
  b <- readBin(em_list_file, "raw", file.size(em_list_file))
  expect_false(any(as.integer(b) > 127L))
  expect_true(all(grepl("(^|/)tests/", em_listed$path)))
  expect_true(all(nzchar(em_listed$motivo)))
  expect_false(anyDuplicated(em_listed$path) > 0L)
})

test_that("[AC] gate de emendas: sem D/R em */tests desde m8-base e todo M listado em emendas_m8.txt", {
  em_skip_sem_git()
  d <- em_diff(em_root)
  if (!d$ok) fail(d$motivo)
  else {
    v <- em_violations(d$rows, em_listed$path)
    expect_identical(v$path, character(), info = paste(v$path, v$motivo, collapse = "; "))
  }
})

test_that("[CONTROL] gate de emendas: M de teste alheio fora da lista, D e R acusam o caminho (clone temporario)", {
  em_skip_sem_git()
  d <- em_diff(em_root)
  if (!d$ok) fail(d$motivo)
  else {
    tmp <- withr::local_tempdir()
    clone <- file.path(tmp, "clone")
    cl <- em_git(tmp, c("clone", "-q", "--no-hardlinks", shQuote(em_root), shQuote(clone)))
    expect_identical(cl$status, 0L, info = paste(cl$out, collapse = " "))
    em_git(clone, c("config", "user.email", "t@t")); em_git(clone, c("config", "user.name", "t"))
    ## o clone copia as tags; a base do clone e a mesma
    expect_identical(em_git(clone, c("rev-parse", "m8-base"))$out, em_git(em_root, c("rev-parse", "m8-base"))$out)
    alvo <- "scripts/lib/tests/test-runtime.R"            # teste do epico M1, fora da lista
    expect_false(alvo %in% em_listed$path)
    expect_true(file.exists(file.path(clone, alvo)))
    ## baseline do clone: o mesmo veredito do repo real (nenhuma violacao)
    expect_identical(em_violations(em_diff(clone)$rows, em_listed$path)$path, character())
    ## M fora da lista
    cat("## emenda de teste de outro epico\n", file = file.path(clone, alvo), append = TRUE)
    v <- em_violations(em_diff(clone)$rows, em_listed$path)
    expect_identical(v$path, alvo); expect_identical(v$motivo, "M fora de emendas_m8.txt")
    ## o mesmo M, uma vez LISTADO, passa
    expect_identical(em_violations(em_diff(clone)$rows, c(em_listed$path, alvo))$path, character())
    ## D
    em_git(clone, c("checkout", "--", shQuote(alvo)))
    expect_true(file.remove(file.path(clone, "scripts/lib/tests/test-dates.R")))
    v <- em_violations(em_diff(clone)$rows, c(em_listed$path, alvo))
    expect_identical(v$path, "scripts/lib/tests/test-dates.R"); expect_identical(v$status, "D")
    em_git(clone, c("checkout", "--", "scripts/lib/tests/test-dates.R"))
    ## R (renomeacao), mesmo com o conteudo identico
    em_git(clone, c("mv", shQuote(alvo), shQuote("scripts/lib/tests/test-runtime-renomeado.R")))
    v <- em_violations(em_diff(clone)$rows, em_listed$path)
    expect_true(all(grepl("^R", v$status[v$path == "scripts/lib/tests/test-runtime-renomeado.R"])) && any(v$path == "scripts/lib/tests/test-runtime-renomeado.R"))
  }
})

test_that("[CONTROL] gate de emendas: sem a tag m8-base o gate falha com motivo (nunca skip)", {
  tmp <- withr::local_tempdir()
  em_git(tmp, "init -q")
  d <- em_diff(tmp)
  expect_false(d$ok)
  expect_match(d$motivo, "m8-base")
})

test_that("[CONTROL] o parser da lista ignora comentario e linha vazia e separa o motivo", {
  p <- em_parse_list(c("# c", "", "scripts/lib/tests/a.R  # porque", "tests/x/b.R"))
  expect_identical(p$path, c("scripts/lib/tests/a.R", "tests/x/b.R"))
  expect_identical(p$motivo, c("porque", ""))
})
