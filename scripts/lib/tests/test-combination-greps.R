## test-combination-greps.R -- M4.V: grep gates de FONTE da combina (arquivo proprio; nao edita test-static-greps.R do M1.V).
## Cada gate = FORBIDDEN real (zero hits no codigo de scripts/lib/combination.R e scripts/lib/cli/combine.R) + CONTROL isca
## (a mesma regra aplicada a uma copia em tempdir com 1 violacao inserida tem de pegar). Sao so codigo: comentarios fora
## (getParseData), entao uma proibicao citada em comentario nao conta. `scripts/prevcarga` e do M1.4 e nao recebe codigo da combina.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
members <- env$prevcarga_members(root = pc_root())

lib_dir <- file.path(pc_root(), "scripts", "lib")
m4_targets <- c(comb = file.path(lib_dir, "combination.R"), verb = file.path(lib_dir, "cli", "combine.R"))

## linhas de CODIGO (tokens terminais sem COMMENT, juntados por linha com espaco)
pc_m4_code <- function(path) {
  ex <- parse(file = path, keep.source = TRUE)
  pd <- utils::getParseData(ex)
  pd <- pd[pd$terminal & pd$token != "COMMENT", , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  vapply(split(pd$text, pd$line1), paste, "", collapse = " ")
}
pc_m4_hits <- function(path, rx) sum(grepl(rx, pc_m4_code(path), perl = TRUE))

sig_rx <- paste0("[\"'](", paste(members$sigla, collapse = "|"), ")[\"']")
m4_grep_gates <- data.table::data.table(
  id   = c("m4_no_leaky_filter", "m4_no_cut_blend", "m4_no_broadcast", "m4_no_env_clock", "m4_no_base_dates",
           "m4_no_net", "m4_no_proc", "m4_no_band_literal", "m4_no_own_metric", "m4_no_own_hash",
           "m4_no_member_cut", "m4_no_sigla_literal", "m4_members_from_topology", "m4_band_from_manifest"),
  rx   = c("ym *< *m\\b", "interp|weights_at_cut", "cut_invariant|broadcast", "Sys\\.getenv|Sys\\.time|Sys\\.Date",
           "as\\.POSIXct|as\\.POSIXlt|strptime|as\\.Date|difftime|with_tz|force_tz",
           "paws|aws\\.s3|s3://|https?://|httr|curl", "system2? *\\(|mclapply|fork",
           "O\\+1|O\\+8|days *\\( *(2|9)L? *\\)", "sum *\\( *abs *\\(", "digest *\\(|toJSON",
           "prevcarga_train_origin", sig_rx, "prevcarga_members *\\(", "delivery_days"),
  mode = c(rep("zero", 12L), "atleast1", "atleast1"),
  files = list("both", "both", "both", "both", "both", "both", "both", "both", "both", "both", "both", "both", "comb", "comb"),
  bait = c("x <- panel$ym < m", "w <- weights_at_cut(a)", "y <- broadcast(w)", "z <- Sys.getenv(\"X\")",
           "d <- as.Date(x)", "u <- \"https://x\"", "r <- system(\"ls\")", "k <- lubridate::days(9L)",
           "s <- sum(abs(x))", "h <- digest(x)", "o <- prevcarga_train_origin(x)", "s <- \"RF\"",
           NA_character_, NA_character_))

pc_m4_target_paths <- function(f) if (identical(f, "both")) unname(m4_targets) else unname(m4_targets[f])
## copia-isca em tempdir: o arquivo original + 1 linha violadora ao fim
pc_m4_bait_copy <- function(path, line) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  out <- file.path(d, basename(path))
  writeLines(c(readLines(path, warn = FALSE), line), out)
  out
}

test_that("M4.V - grep gates: zero hits reais (zero) / >= 1 hit (atleast1) em combination.R e cli/combine.R", {
  expect_true(all(file.exists(m4_targets)))
  for (i in seq_len(nrow(m4_grep_gates))) {
    g <- m4_grep_gates[i]
    for (p in pc_m4_target_paths(g$files[[1L]])) {
      n <- pc_m4_hits(p, g$rx)
      if (g$mode == "zero") expect_identical(n, 0L, info = paste(g$id, basename(p)))
      else expect_gte(n, 1L, label = paste(g$id, basename(p)))
    }
  }
})

test_that("M4.V - grep gates: cada isca e detectada (CONTROL) e a regra e especifica", {
  for (i in which(m4_grep_gates$mode == "zero")) {
    g <- m4_grep_gates[i]
    for (p in pc_m4_target_paths(g$files[[1L]])) {
      b <- pc_m4_bait_copy(p, g$bait)
      expect_gte(pc_m4_hits(b, g$rx), 1L, label = paste("isca", g$id, basename(p)))
    }
  }
  ## a isca so conta como codigo: o mesmo texto em comentario nao e pego
  d <- withr::local_tempdir()
  cm <- file.path(d, "c.R"); writeLines(c("x <- 1", "## ym < m e Sys.getenv e interp"), cm)
  expect_identical(pc_m4_hits(cm, "ym *< *m\\b|Sys\\.getenv|interp"), 0L)
  ## as regras atleast1 reprovam uma copia SEM o padrao
  nb <- file.path(d, "n.R"); writeLines("x <- 1", nb)
  expect_identical(pc_m4_hits(nb, "prevcarga_members *\\("), 0L)
  expect_identical(pc_m4_hits(nb, "delivery_days"), 0L)
})

## ---- consumo por AST: a combina CHAMA os modulos unicos, nao reimplementa ----------------------------------------------------------
## alcance transitivo de nomes dentro do env do modulo (profundidade limitada)
pc_m4_reach <- function(name, e, depth = 6L) {
  all_nm <- character(); frontier <- name; done <- character()
  for (k in seq_len(depth)) {
    nxt <- character()
    for (f in setdiff(frontier, done)) {
      done <- c(done, f)
      fn <- get0(f, envir = e, inherits = FALSE)
      if (!is.function(fn)) next
      nm <- unique(all.names(body(fn)))
      all_nm <- union(all_nm, nm)
      nxt <- c(nxt, nm[nm %in% ls(e, all.names = TRUE)])
    }
    frontier <- setdiff(unique(nxt), done)
    if (!length(frontier)) break
  }
  all_nm
}
test_that("M4.V - consumo por AST: gate->scorecard, join->read_actual, train->freeze/run_manifest/version_hash, apply->thaw", {
  expect_true("prevcarga_scorecard" %in% pc_m4_reach("prevcarga_combina_gate", env))
  expect_true("prevcarga_read_actual" %in% all.names(body(env$prevcarga_join_fresh_truth)))
  nm_train <- all.names(body(env$prevcarga_combina_train))
  expect_true(all(c("prevcarga_freeze", "prevcarga_run_manifest", "prevcarga_version_hash") %in% nm_train))
  expect_true("prevcarga_thaw" %in% all.names(body(env$prevcarga_combina_apply)))
  ## CONTROL: uma funcao que reimplementa (sem chamar o modulo unico) e pega pela mesma regra
  decoy <- function(x) sum(abs(x - 1)) / sum(abs(x))
  expect_false("prevcarga_scorecard" %in% all.names(body(decoy)))
  expect_true("prevcarga_gate_decoy" %in% pc_m4_reach("a", local({ e <- new.env(); e$a <- function() b(); e$b <- function() prevcarga_gate_decoy(); e })))
})

test_that("M4.V - prevcarga_combina_full NAO chama prevcarga_join_fresh_truth (consome y_true como recebido); isca e pega", {
  expect_false("prevcarga_join_fresh_truth" %in% pc_m4_reach("prevcarga_combina_full", env))
  expect_false("prevcarga_read_actual" %in% pc_m4_reach("prevcarga_combina_full", env))
  expect_true("prevcarga_join_fresh_truth" %in% pc_m4_reach("prevcarga_combina_load", env))   # so o pipeline de montagem junta a verdade
  e2 <- new.env(parent = env)
  e2$prevcarga_combina_full_bad <- function(panel, ctx) prevcarga_join_fresh_truth(panel, ctx, "SECO")
  expect_true("prevcarga_join_fresh_truth" %in% pc_m4_reach("prevcarga_combina_full_bad", e2))
})

## ---- m4_docs_live ----------------------------------------------------------------------------------------------------------------
pc_m4_docs_ok <- function(path) {
  ln <- readLines(path, warn = FALSE)
  any(grepl("combination\\.R", ln)) && !any(grepl("combination\\.R", ln) & grepl("VERIFY-READY|CRIAR", ln))
}
test_that("M4.V - m4_docs_live: docs/estado/m4-combina.md declara combination.R vivo, sem VERIFY-READY/CRIAR na mesma linha", {
  doc <- file.path(pc_root(), "docs", "estado", "m4-combina.md")
  expect_true(file.exists(doc))
  expect_true(pc_m4_docs_ok(doc))
  d <- withr::local_tempdir()
  b1 <- file.path(d, "b1.md"); writeLines(c(readLines(doc, warn = FALSE), "CRIAR scripts/lib/combination.R"), b1)
  b2 <- file.path(d, "b2.md"); writeLines(c("# sem a citacao", "texto"), b2)
  b3 <- file.path(d, "b3.md"); writeLines(c(readLines(doc, warn = FALSE), "combination.R VERIFY-READY"), b3)
  expect_false(pc_m4_docs_ok(b1))
  expect_false(pc_m4_docs_ok(b2))
  expect_false(pc_m4_docs_ok(b3))
})
