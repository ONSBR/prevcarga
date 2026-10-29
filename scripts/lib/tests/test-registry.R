## test-registry.R -- registry.R (M1.6): fronteira Tier A em wire-time (CONTROL requires/tier B; POSITIVE cadeia).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology", "registry"), env = environment())

expect_reg_error <- function(expr, id, msg = "registry:") {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_match(conditionMessage(cond), "\\[prevcarga/contract\\] registry:")
  expect_match(conditionMessage(cond), msg)
  invisible(cond)
}
spec_rf <- list(id = "rf", tier = "A", requires = c("deck", "features:rf"), provides = "member_long:rf")
spec_fe <- list(id = "fe_rf", tier = "A", requires = "deck", provides = "features:rf")

test_that("CONTROL: requires nao satisfeito falha e nada e gravado", {
  prevcarga_registry_reset(base = "deck")
  expect_reg_error(prevcarga_register(spec_rf), "contract.registry_requires_unmet", "features:rf")
  expect_false("rf" %in% names(prevcarga_registry()))
})

test_that("POSITIVE: cadeia fe_rf -> rf silenciosa; ordem inversa falha em wire-time", {
  prevcarga_registry_reset(base = "deck")
  expect_silent(prevcarga_register(spec_fe))
  expect_silent(prevcarga_register(spec_rf))
  expect_identical(names(prevcarga_registry()), c("fe_rf", "rf"))
  prevcarga_registry_reset(base = "deck")
  expect_reg_error(prevcarga_register(spec_rf), "contract.registry_requires_unmet")
})

test_that("tier B/C, id duplicado e spec malformado falham sem alterar o estado", {
  prevcarga_registry_reset(base = "deck")
  prevcarga_register(spec_fe)
  before <- prevcarga_registry()
  expect_reg_error(prevcarga_register(modifyList(spec_fe, list(id = "b", tier = "B"))),
                   "contract.registry_tier_invalid", "tier B reservado")
  expect_reg_error(prevcarga_register(modifyList(spec_fe, list(id = "c", tier = "C"))),
                   "contract.registry_tier_invalid")
  expect_reg_error(prevcarga_register(spec_fe), "contract.registry_duplicate_id")
  noid <- spec_fe; noid$id <- NULL
  c1 <- expect_reg_error(prevcarga_register(noid), "contract.registry_malformed")
  expect_identical(c1$where, "id")
  c2 <- expect_reg_error(prevcarga_register(modifyList(spec_fe, list(id = "x", requires = 1L))),
                         "contract.registry_malformed")
  expect_identical(c2$where, "requires")
  expect_reg_error(prevcarga_registry_reset(base = 1L), "contract.registry_malformed")
  expect_identical(prevcarga_registry(), before)
})

test_that("membro vem de prevcarga_members(root): conhecido registra, desconhecido falha", {
  prevcarga_registry_reset()
  expect_silent(prevcarga_register(list(id = "m", tier = "A", member = "RF"), root = pc_root()))
  expect_reg_error(prevcarga_register(list(id = "n", tier = "A", member = "XX_INEXISTENTE"), root = pc_root()),
                   "contract.registry_unknown_member", "XX_INEXISTENTE")
  expect_identical(names(prevcarga_registry()), "m")
})

test_that("reset volta a list() e nenhum arquivo e criado (estado so em memoria)", {
  wd <- tempfile("regwd"); dir.create(wd)
  root <- pc_root()
  repo_files <- function() {
    git <- system2("git", c("-C", shQuote(root), "status", "--porcelain"), stdout = TRUE, stderr = FALSE)
    sort(git)
  }
  snap <- function() sort(list.files(wd, recursive = TRUE, all.files = TRUE))
  before <- list(snap(), repo_files(), sort(list.files(tempdir())))
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  prevcarga_registry_reset(base = "deck")
  prevcarga_register(spec_fe)
  prevcarga_register(modifyList(spec_fe, list(id = "fe2", provides = "x")))
  expect_length(prevcarga_registry(), 2L)
  prevcarga_registry_reset()
  expect_identical(prevcarga_registry(), list())
  after <- list(snap(), repo_files(), sort(list.files(tempdir())))
  expect_identical(after, before)
})

test_that("fonte sem lista de siglas (ciclo de vida: M5.2 o aloja no registry.R; CONTROL de arquivo-isca)", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "registry.R"), warn = FALSE)
  siglas <- "\\b(RF|LGBM|HW)\\b|RGD_|LIN_"
  vida <- "\\b(promote|rollback|gc|pin)\\b|latest\\.txt"
  expect_false(any(grepl(siglas, src)))
  isca <- tempfile(fileext = ".R")
  writeLines(c("promote <- function() NULL", "x <- c('RF', 'RGD_MED')"), isca)
  txt <- readLines(isca)
  expect_true(any(grepl(vida, txt)))
  expect_true(any(grepl(siglas, txt)))
})
