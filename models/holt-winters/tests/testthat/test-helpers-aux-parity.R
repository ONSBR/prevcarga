## test-helpers-aux-parity.R -- gate [PLUGINS] helpers_oraculo (HW.2; AC 4). Tier parity: so roda com
## options(prevcarga.gates.tier = "parity"). O oraculo e so leitura: o funcoes_aux.R canonico e carregado num env de
## teste (sys.source) e cada funcao portada e comparada, com `identical()`, sobre as mesmas tabelas do fixture.

if (!exists("pc_gate_tier", mode = "function"))
  pc_gate_tier <- function(tier) {
    got <- getOption("prevcarga.gates.tier", "fast")
    if (!identical(got, tier)) testthat::skip(sprintf("tier %s (atual: %s)", tier, got))
    invisible(TRUE)
  }
if (!exists("pc_oracle_root", mode = "function"))
  pc_oracle_root <- function() "/opt/ons-source/prevcargadessem3"

## tabelas do deck interno: CargaTempVer/CargaTempMed limpas e versoes com dia repetido e dia faltante
hw_fx_features_input <- function() {
  env <- hw_env()
  st <- hw_fx_store()
  cuts <- hw_fx_cuts(st)
  deck <- env$plugin_ingest(hw_fx_panel(st, cuts, history_days = 60L), cuts)
  ctv <- deck$CargaTempVer; ctm <- deck$CargaTempMed
  ## dia repetido (linha 20 duplicada) e dia faltante (linha 30 removida; tambem nas primeiras 7 linhas na ctm)
  ctv_gap <- ctv[-30L, ]; rownames(ctv_gap) <- NULL
  ctv_dup <- ctv[c(1:20, 20:nrow(ctv)), ]; rownames(ctv_dup) <- NULL
  ctm_gap <- ctm[-4L, ]; rownames(ctm_gap) <- NULL
  ## buracos para o Corrige.Dados: NA, carga fora da faixa e temperatura fora da faixa
  ctv_bad <- ctv
  ctv_bad[5L, "H10"] <- NA; ctv_bad[9L, "H20"] <- 0; ctv_bad[12L, "T05"] <- 80; ctv_bad[15L, "T07"] <- NA
  ## dias atipicos (forma distorcida, deterministica): fazem o MO1 imputar por cluster, senao a semente nao tem efeito
  for (r in c(12L, 19L, 26L, 33L, 40L, 47L))
    ctv_bad[r, hw_env()$hw_slot_cols()] <- 0.5 * ctv_bad[r, hw_env()$hw_slot_cols()] * exp(0.4 * sin((1:48)^2 * (r + 0.7)))
  ctv_bad[7L, "Feriado"] <- NA; ctv_bad[8L, "DiaSemana"] <- NA; ctv_bad[10L, "TIPO_FER"] <- NA
  list(ctv = ctv, ctm = ctm, ctv_gap = ctv_gap, ctv_dup = ctv_dup, ctm_gap = ctm_gap, ctv_bad = ctv_bad)
}

test_that("[PLUGINS] helpers_oraculo: funcoes portadas == oraculo (identical), CONTROL span 0,14, oraculo intocado", {
  pc_gate_tier("parity")
  oracle_file <- file.path(pc_oracle_root(), "models/holt-winters/R/feature_engineering/funcoes_aux.R")
  skip_if_not(file.exists(oracle_file), "oraculo ausente")
  orc <- new.env(parent = globalenv())
  suppressMessages(sys.source(oracle_file, envir = orc))
  env <- hw_env(); tab <- hw_fx_features_input()
  git_before <- system2("git", c("-C", pc_oracle_root(), "status", "--porcelain"), stdout = TRUE)
  q <- function(x) suppressMessages(x)

  ## 1. Corrige.Dias.Faltantes: sem buraco, com dia repetido, com dia faltante (ctv e ctm)
  for (k in c("ctv", "ctv_gap", "ctv_dup"))
    expect_identical(q(env$Corrige.Dias.Faltantes(tab[[k]])), q(orc$Corrige.Dias.Faltantes(tab[[k]])), info = k)
  for (k in c("ctm", "ctm_gap"))
    expect_identical(q(env$Corrige.Dias.Faltantes(tab[[k]])), q(orc$Corrige.Dias.Faltantes(tab[[k]])), info = k)
  fixed <- q(env$Corrige.Dias.Faltantes(tab$ctv_gap))
  expect_identical(nrow(fixed), nrow(tab$ctv))

  ## 2. Corrige.Dados (p_ncol 52, Negativos) sobre tabela com buracos
  ctv <- q(env$Corrige.Dias.Faltantes(tab$ctv))
  for (k in c("ctv", "ctv_bad")) {
    t <- tab[[k]]
    expect_identical(env$Corrige.Dados(t, nrow(t), 52L, TRUE, FALSE, FALSE),
                     orc$Corrige.Dados(t, nrow(t), 52L, TRUE, FALSE, FALSE), info = k)
  }
  cd <- env$Corrige.Dados(tab$ctv_bad, nrow(tab$ctv_bad), 52L, TRUE, FALSE, FALSE)[[1L]]

  ## 3. MO1 (perfil C): o oraculo semeia 123 internamente; o porte recebe p_kmeans_seed = 123L
  mo_e <- q(env$MO1(cd, 1, "C", 12L, 0.8, p_kmeans_seed = 123L))
  mo_o <- q(orc$MO1(cd, 1, "C", 12L, 0.8))
  expect_identical(mo_e, mo_o)
  expect_identical(env$classifica_kmeans(as.matrix(cd[, 4:51]), 12L, p_seed = 123L),
                   orc$classifica_kmeans(as.matrix(cd[, 4:51]), 12L))

  ## 4. Substitui.Feriados, 5. Suaviza.LOESS, 6. Suaviza.LOESS1, 7. SemiFer
  mo <- mo_e[[1L]]
  expect_gt(mo_e[[2L]], 0)
  expect_true(any(mo$Feriado %in% c(1, 2)))
  sf_e <- env$Substitui.Feriados(mo, nrow(mo), 52L, FALSE)
  expect_identical(sf_e, orc$Substitui.Feriados(mo, nrow(mo), 52L, FALSE))
  expect_false(identical(sf_e[[1L]], mo))
  expect_identical(env$Suaviza.LOESS(sf_e[[1L]], nrow(mo), 0.3), orc$Suaviza.LOESS(sf_e[[1L]], nrow(mo), 0.3))
  row <- as.numeric(mo[1L, 4:51]) * (1 + 0.05 * sin((1:48)^2))  # linha irregular: o span importa
  expect_identical(env$Suaviza.LOESS1(row, 0.13), orc$Suaviza.LOESS1(row, 0.13))
  for (fs in c("F", "S"))
    expect_identical(env$SemiFer(mo, fs, 0.5, 1000, 5, 12L), orc$SemiFer(mo, fs, 0.5, 1000, 5, 12L), info = fs)
  expect_identical(env$SemiFer(mo, "F", 0.5, 1000, 4, 12L), orc$SemiFer(mo, "F", 0.5, 1000, 4, 12L))

  ## CONTROL: span 0,20 (porte) vs 0,13 (oraculo) difere (0,14 nao serve: floor(0,14*48) = floor(0,13*48) = 6 vizinhos, mesmo ajuste)
  expect_gt(max(abs(env$Suaviza.LOESS1(row, 0.20)[[1L]] - orc$Suaviza.LOESS1(row, 0.13)[[1L]])), 1e-5)
  ## CONTROL da semente: kmeans_seed 7 nao reproduz o oraculo (seed 123)
  expect_false(identical(q(env$MO1(cd, 1, "C", 12L, 0.8, p_kmeans_seed = 7L)), mo_o))

  ## o oraculo continua intocado: o `git status --porcelain` e identico ao de antes do teste (hoje o oraculo ja traz
  ## docs/roadmap/ nao versionados que nao sao nossos; so a diferenca antes/depois prova que nada foi escrito)
  expect_identical(system2("git", c("-C", pc_oracle_root(), "status", "--porcelain"), stdout = TRUE), git_before)
  expect_false(any(grepl("models/holt-winters", git_before, fixed = TRUE)))
})
