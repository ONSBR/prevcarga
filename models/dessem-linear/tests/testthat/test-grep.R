## test-grep.R -- DL.8 (AC3 do epico, lado do plugin): grep estatico do codigo do plugin com `pc_grep_code()` (sem comentarios).
## Zero hits dos padroes proibidos em `models/dessem-linear/R` (o +-1 h e SO do despachante, por `model_time_offset_min: 60L`) e,
## para cada padrao, um arquivo-fixture que o contem e e ACUSADO (inclusive `DataHora + 3600L` na ingestao).

DL_GREP_PATTERNS <- c(
  as_date      = "(?<![A-Za-z0-9_.])as\\.Date\\(",
  as_posixct   = "as\\.POSIXct|as\\.POSIXlt",
  strptime     = "strptime",
  difftime     = "difftime",
  sys_time     = "Sys\\.time",
  sys_date     = "Sys\\.Date",
  sys_getenv   = "Sys\\.getenv|Sys\\.setenv",
  system_call  = "(?<![A-Za-z0-9_.])system2?\\(",
  mclapply     = "mclapply|mcparallel",
  super_assign = "<<-",
  globalenv    = "globalenv\\(\\)",
  hours_1      = "(?<![A-Za-z0-9_.])d?hours\\(\\s*1\\s*\\)",
  hours_empty  = "(?<![A-Za-z0-9_.])d?hours\\(\\s*\\)",
  minutes_30   = "(?<![A-Za-z0-9_.])d?minutes\\(\\s*-?30\\s*\\)",
  secs_3600    = "(?<![0-9.])3600L?(?![0-9])",
  to_model     = "to_model_frame\\(|to_display_frame\\(",
  non_ascii    = "[\\x80-\\xFF]")

## `globalenv()` ja existe hoje em register.R (dl_abort/dl_warn) e inference.R (dl_foundation): busca interina da API da
## fundacao (prevcarga_condition/prevcarga_warn/to_semihourly) no global ate o loader expor a fundacao ao plugin. Divida
## do DL.5/DL.7, registrada como discovery do DL.8; este teste a PRENDE ao conjunto exato (qualquer uso novo reprova).
DL_GREP_DEBT <- list(globalenv = c("inference.R:66", "register.R:23", "register.R:31"))

dl_grep_bait <- function(pattern_name) switch(pattern_name,
  as_date = "d <- as.Date(x)", as_posixct = "t <- as.POSIXct(x)", strptime = "t <- strptime(x, '%H')",
  difftime = "d <- difftime(a, b)", sys_time = "t0 <- Sys.time()", sys_date = "d <- Sys.Date()",
  sys_getenv = "v <- Sys.getenv('EXOGENA')", system_call = "system('ls')", mclapply = "r <- parallel::mclapply(x, f)",
  super_assign = "cnt <<- cnt + 1L", globalenv = "e <- globalenv()", hours_1 = "t <- x + lubridate::hours(1)",
  hours_empty = "t <- x + hours()", minutes_30 = "t <- x - lubridate::minutes(30)", secs_3600 = "ingest <- DataHora + 3600L",
  to_model = "m <- to_model_frame(x, 60L)", non_ascii = "msg <- 'açao'")

test_that("[PLUGINS] grep: zero hits dos padroes proibidos em models/dessem-linear/R (divida de globalenv() pinada)", {
  rdir <- file.path(dl_models(), "dessem-linear", "R")
  expect_gt(length(list.files(rdir, pattern = "[.]R$")), 7L)
  for (nm in names(DL_GREP_PATTERNS)) {
    hits <- if (identical(nm, "non_ascii")) {
      unlist(lapply(list.files(rdir, pattern = "[.]R$", full.names = TRUE), function(f) {
        ln <- readLines(f, warn = FALSE, encoding = "bytes")
        h <- which(grepl("[\\x80-\\xFF]", ln, perl = TRUE, useBytes = TRUE))
        if (length(h)) paste0(basename(f), ":", h) else character()
      }))
    } else pc_grep_code(rdir, DL_GREP_PATTERNS[[nm]])
    expect_setequal(if (is.null(hits)) character() else hits, if (is.null(DL_GREP_DEBT[[nm]])) character() else DL_GREP_DEBT[[nm]])
  }
})

test_that("[PLUGINS] grep: CONTROL cada padrao acusa o seu arquivo-fixture (inclusive DataHora + 3600L na ingestao)", {
  d <- tempfile("dl8-bait-"); dir.create(d)
  for (nm in names(DL_GREP_PATTERNS)) {
    f <- file.path(d, paste0(nm, ".R"))
    writeBin(charToRaw(enc2utf8(paste0(dl_grep_bait(nm), "\n"))), f)
  }
  for (nm in names(DL_GREP_PATTERNS)) {
    f <- file.path(d, paste0(nm, ".R"))
    hits <- if (identical(nm, "non_ascii")) {
      ln <- readLines(f, warn = FALSE, encoding = "bytes")
      if (any(grepl("[\\x80-\\xFF]", ln, perl = TRUE, useBytes = TRUE))) paste0(basename(f), ":1") else character()
    } else pc_grep_code(f, DL_GREP_PATTERNS[[nm]])
    expect_length(hits, 1L)
    ## o padrao NAO acusa os outros fixtures por engano (so ele, exceto os que compartilham o simbolo)
  }
  ## comentario nao conta (pc_grep_code corta o comentario): o mesmo texto depois de `#` nao e hit
  cf <- file.path(d, "comentario.R"); writeLines("x <- 1  # as.Date( Sys.time() DataHora + 3600L globalenv()", cf)
  for (nm in names(DL_GREP_PATTERNS)[names(DL_GREP_PATTERNS) != "non_ascii"]) expect_length(pc_grep_code(cf, DL_GREP_PATTERNS[[nm]]), 0L)
  ## ingestao do plugin: um `DataHora + 3600L` seria acusado no ingestion.R real
  ing <- file.path(d, "ingestion.R"); writeLines(c("plugin_ingest <- function(panel) {", "  ld[, DataHora := DataHora + 3600L]", "}"), ing)
  expect_identical(pc_grep_code(ing, DL_GREP_PATTERNS[["secs_3600"]]), "ingestion.R:2")
})
