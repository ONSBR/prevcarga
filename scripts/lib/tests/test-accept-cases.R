## test-accept-cases.R -- accept_cases.R (E8-T2): validacao do YAML, expansao deterministica das unidades e overlays.
## Fixture sintetica em tempdir (helper-accept.R); nenhum docker, nenhuma chamada AWS, o store nem e tocado.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology"), env = environment())
source(file.path(pc_root(), "scripts", "tools", "accept_cases.R"), local = environment())

acc_cases_script <- function() file.path(pc_root(), "scripts", "tools", "accept_cases.R")
acc_cases_real <- function() file.path(pc_root(), "scripts", "tools", "accept_docker.yaml")
## roda accept_cases.R como processo (Rscript --vanilla); devolve status, stdout (envelope JSON) e stderr
acc_cases_run <- function(args) {
  se <- tempfile("acc-cases-err-")
  so <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", acc_cases_script(), args), stdout = TRUE, stderr = se))
  st <- attr(so, "status"); if (is.null(st)) st <- 0L
  txt <- paste(so, collapse = "\n")
  list(status = as.integer(st), stdout = txt, stderr = readLines(se, warn = FALSE),
       json = if (startsWith(txt, "{")) jsonlite::fromJSON(txt, simplifyVector = FALSE) else NULL)
}
acc_read_units <- function(f) data.table::fread(f, sep = "\t", na.strings = NULL, colClasses = "character")

test_that("units.tsv deterministico com 384 unidades, colunas e data_flag por versao", {
  o1 <- tempfile(fileext = ".tsv"); o2 <- tempfile(fileext = ".tsv")
  r1 <- acc_cases_run(c("--config", acc_cases_real(), "--stage", "medicao", "--out", o1))
  r2 <- acc_cases_run(c("--config", acc_cases_real(), "--stage", "medicao", "--out", o2))
  expect_equal(c(r1$status, r2$status), c(0L, 0L))
  expect_identical(readBin(o1, "raw", file.size(o1)), readBin(o2, "raw", file.size(o2)))
  u <- acc_read_units(o1)
  expect_identical(names(u), pc_accept_unit_cols)
  expect_identical(nrow(u), 12L * 8L * 2L * 2L)
  expect_identical(nrow(u), 384L)
  expect_false(anyDuplicated(u$unit) > 0)
  expect_true(all(u$stage == "medicao" & u$wave == "1" & u$verbo == "backtest" & u$topologia == "full" & u$janela == "-"))
  expect_true(all(u$data_flag[u$versao == "ref"] == "--data"))
  expect_true(all(u$data_flag[u$versao == "new"] == "--data-train"))
  expect_identical(sort(unique(u$mdir)), c("dessem-linear", "dessem-regdin"))
  expect_true(all(u$artifacts_from == "-"))
  ## ordem radix e ids estaveis
  expect_identical(u$unit, u$unit[order(u$stage, as.integer(u$wave), u$versao, u$origem, u$area, u$mdir, method = "radix")])
  expect_true(all(u$unit == sprintf("medicao-%s-backtest-%s-%s-%s", u$versao, u$area, u$origem, u$mdir)))
  ## ASCII puro
  expect_false(any(as.integer(charToRaw(paste(readLines(o1), collapse = "\n"))) > 127L))
})

test_that("estagio aceite: backtest S nas duas versoes, 8 feriados, treino e infer MED RJ, deck-inference e ondas", {
  o <- tempfile(fileext = ".tsv")
  expect_equal(acc_cases_run(c("--config", acc_cases_real(), "--stage", "aceite", "--out", o))$status, 0L)
  u <- acc_read_units(o)
  expect_identical(names(u), pc_accept_unit_cols)
  cfg <- yaml::read_yaml(acc_cases_real())$aceite
  expect_true(all(u$stage == "aceite"))
  ## celulas na origem; unidades de TEMPO na origem_tempo (default = origem; a real usa uma origem sem feriado no horizonte, onde a ref treina)
  expect_true(all(u[verbo == "backtest"]$origem == cfg$origem))
  expect_true(all(u[verbo != "backtest"]$origem == cfg$origem_tempo))
  expect_false(identical(cfg$origem, cfg$origem_tempo))
  s <- u[area == "S"]
  expect_identical(s$versao[order(s$versao)], c("new", "ref"))
  expect_identical(s[versao == "new"]$topologia, "sem_lin_uni"); expect_identical(s[versao == "new"]$janela, "referencia")
  expect_identical(s[versao == "ref"]$topologia, "full");        expect_identical(s[versao == "ref"]$janela, "-")
  expect_identical(s[versao == "ref"]$data_flag, "--data");      expect_identical(s[versao == "new"]$data_flag, "--data-train")
  ## topologia e janela so na nova
  expect_true(all(u[versao == "ref" & unit != s[versao == "ref"]$unit]$topologia == "full"))
  expect_true(all(u[versao == "ref"]$janela == "-"))
  expect_identical(u[topologia != "full" | janela != "-"]$unit, s[versao == "new"]$unit)
  hol <- u[verbo == "backtest" & mdir == "dessem-linear"]
  expect_identical(sort(hol$area), c("AM", "BAOE", "CE", "DF", "GO", "PA", "RJ", "TON"))
  expect_true(all(hol$versao == "new" & hol$wave == "1"))
  tr <- u[verbo == "train-models"]
  expect_identical(tr[order(versao)]$versao, c("new", "ref")); expect_true(all(tr$area == "RJ" & tr$mdir == "dessem-linear" & tr$wave == "1"))
  im <- u[verbo == "infer-model"]
  expect_identical(nrow(im), 3L); expect_true(all(im$area == "RJ" & im$mdir == "dessem-linear/MED"))
  expect_identical(im[versao == "ref"]$wave, "2"); expect_identical(im[versao == "ref"]$data_flag, "--data")
  expect_identical(im[data_flag == "--data-train"]$wave, "2"); expect_identical(im[data_flag == "--data-inference"]$wave, "3")
  expect_identical(im[versao == "ref"]$artifacts_from, tr[versao == "ref"]$unit)
  new_art <- c(im[data_flag == "--data-train"]$artifacts_from, im[data_flag == "--data-inference"]$artifacts_from,
               u[verbo == "deck-inference"]$artifacts_from)
  expect_identical(unique(new_art), tr[versao == "new"]$unit)
  expect_length(new_art, 3L)
  dk <- u[verbo == "deck-inference"]
  expect_identical(nrow(dk), 1L); expect_identical(dk$versao, "new"); expect_identical(dk$wave, "2"); expect_identical(dk$data_flag, "--data-train")
  ## a onda 2 depende so de unidades da onda 1 e a 3, so da 2
  expect_true(all(u[wave == "2"]$artifacts_from %in% u[wave == "1"]$unit))
  expect_identical(nrow(u), 2L + 8L + 2L + 3L + 1L)
})

test_that("origem_tempo e opcional: ausente = origem; invalida => contract.accept_bad_config; as unidades de tempo seguem a origem_tempo", {
  fx <- acc_cfg_fixture()
  real <- yaml::read_yaml(acc_cases_real())
  w <- function(name, fn) { f <- file.path(fx$dir, name); yaml::write_yaml(fn(real), f, handlers = list(logical = yaml::verbatim_logical)); f }
  sem <- w("sem_ot.yaml", function(x) { x$aceite$origem_tempo <- NULL; x })
  o <- tempfile(fileext = ".tsv")
  expect_equal(acc_cases_run(c("--config", sem, "--stage", "aceite", "--out", o))$status, 0L)
  expect_true(all(acc_read_units(o)$origem == real$aceite$origem))                  # default: tudo na origem
  ruim <- w("ot_ruim.yaml", function(x) { x$aceite$origem_tempo <- "2023-02-30"; x })
  r <- acc_cases_run(c("--config", ruim, "--stage", "aceite", "--out", tempfile()))
  expect_equal(r$status, 2L); expect_match(r$json$error$where, "aceite.origem_tempo", fixed = TRUE)
  o2 <- tempfile(fileext = ".tsv")
  expect_equal(acc_cases_run(c("--config", fx$valid, "--stage", "aceite", "--out", o2))$status, 0L)
  u <- acc_read_units(o2)
  expect_true(all(u[verbo %in% c("train-models", "infer-model", "deck-inference")]$unit %in%
                  grep(real$aceite$origem_tempo, u$unit, value = TRUE, fixed = TRUE)))
})

test_that("YAML invalido => exit 2 contract.accept_bad_config no envelope e nenhum units.tsv gravado; [CONTROL] o valido grava", {
  fx <- acc_cfg_fixture()
  for (k in c("area_zz", "chave", "chave_bloco", "origem", "origem_med", "jobs", "n_sem_aspas")) {
    o <- tempfile(fileext = ".tsv")
    r <- acc_cases_run(c("--config", fx[[k]], "--stage", "medicao", "--out", o))
    expect_equal(r$status, 2L, info = k)
    expect_identical(r$json$error_id, "contract.accept_bad_config", info = k)
    expect_identical(r$json$error$id, "contract.accept_bad_config", info = k)
    expect_identical(r$json$exit_code, 2L, info = k)
    expect_false(file.exists(o), info = k); expect_false(file.exists(paste0(o, ".tmp")), info = k)
    expect_true(any(grepl("contract", r$stderr)), info = k)
  }
  ## onde nomeia o ofensor
  expect_match(acc_cases_run(c("--config", fx$area_zz, "--stage", "medicao", "--out", tempfile()))$json$error$where, "medicao.areas", fixed = TRUE)
  expect_match(acc_cases_run(c("--config", fx$chave, "--stage", "medicao", "--out", tempfile()))$json$error$where, "nao_existe", fixed = TRUE)
  expect_match(acc_cases_run(c("--config", fx$origem, "--stage", "aceite", "--out", tempfile()))$json$error$where, "aceite.origem", fixed = TRUE)
  ## CONTROL: o YAML valido da fixture grava as 384 unidades
  o <- tempfile(fileext = ".tsv")
  expect_equal(acc_cases_run(c("--config", fx$valid, "--stage", "medicao", "--out", o))$status, 0L)
  expect_identical(nrow(acc_read_units(o)), 384L)
  ## a ausencia do arquivo tambem e config invalida
  expect_equal(acc_cases_run(c("--config", file.path(fx$dir, "nao-existe.yaml"), "--stage", "aceite", "--out", tempfile()))$status, 2L)
})

test_that("pc_accept_config_load valida em memoria: precedencia dos defaults e settings k=v", {
  cfg <- pc_accept_config_load(acc_cases_real(), pc_root())
  expect_identical(cfg$jobs, 13L); expect_identical(cfg$memory, "8g"); expect_identical(cfg$ref_rev, "de24021")
  expect_identical(length(cfg$medicao$origens), 12L); expect_identical(length(cfg$medicao$areas), 8L)
  ## chave opcional ausente cai no default embutido
  raw <- yaml::read_yaml(acc_cases_real()); raw$jobs <- NULL; raw$memory <- NULL
  f <- tempfile(fileext = ".yaml"); yaml::write_yaml(raw, f)
  expect_identical(pc_accept_config_load(f, pc_root())$jobs, pc_accept_defaults$jobs)
  r <- acc_cases_run(c("--config", acc_cases_real(), "--settings"))
  expect_equal(r$status, 0L)
  expect_identical(r$stdout, paste(c("jobs=13", "memory=8g", "cpus=1", "min_free_gb_docker=10", "min_free_gb_out=50", "ref_rev=de24021",
                                     "overlay_mdirs=dessem-linear,dessem-regdin"), collapse = "\n"))
})

test_that("overlays: topologia sem LIN_UNI com os demais membros intactos; run.yaml so muda params.janela_dessem", {
  topo_in <- file.path(pc_root(), "config", "topology.yaml")
  o <- tempfile(fileext = ".yaml")
  expect_equal(acc_cases_run(c("--overlay", "topologia", "--in", topo_in, "--out", o))$status, 0L)
  a <- yaml::read_yaml(topo_in); b <- yaml::read_yaml(o)
  expect_false("LIN_UNI" %in% vapply(b$members, function(m) m$sigla, ""))
  expect_identical(b$members, Filter(function(m) !identical(m$sigla, "LIN_UNI"), a$members))
  expect_identical(length(b$members), length(a$members) - 1L)
  expect_identical(b[setdiff(names(b), "members")], a[setdiff(names(a), "members")])
  expect_identical(names(b$subsystems)[3L], "N")                        # "N" volta como texto, nao como FALSE
  ## a topologia do overlay carrega pela fachada: 8 membros, derivados da topologia real
  r <- tempfile("pcroot"); dir.create(file.path(r, "config"), recursive = TRUE)
  file.copy(o, file.path(r, "config", "topology.yaml")); file.copy(file.path(pc_root(), "models"), r, recursive = TRUE)
  expect_identical(nrow(prevcarga_members(root = r)), nrow(prevcarga_members(root = pc_root())) - 1L)
  for (m in c("dessem-linear", "dessem-regdin")) {
    rin <- file.path(pc_root(), "models", m, "run.yaml"); ro <- tempfile(fileext = ".yaml")
    expect_equal(acc_cases_run(c("--overlay", "janela", "--in", rin, "--out", ro))$status, 0L, info = m)
    x <- yaml::read_yaml(rin); y <- yaml::read_yaml(ro)
    expect_identical(y$params$janela_dessem, "referencia", info = m)
    expect_false(identical(x$params$janela_dessem, "referencia"), info = m)
    y$params$janela_dessem <- x$params$janela_dessem
    expect_identical(y, x)
    expect_false(any(as.integer(charToRaw(paste(readLines(ro), collapse = ""))) > 127L))
  }
  ## entrada ausente e tipo desconhecido recusam (exit 2) sem gravar
  bad <- tempfile(fileext = ".yaml")
  expect_equal(acc_cases_run(c("--overlay", "topologia", "--in", file.path(tempdir(), "nao-existe.yaml"), "--out", bad))$status, 2L)
  expect_equal(acc_cases_run(c("--overlay", "outro", "--in", topo_in, "--out", bad))$status, 2L)
  expect_false(file.exists(bad))
})

test_that("catalogo: os 4 ids novos existem com a classe e o exit certos; fontes em ASCII", {
  cat_ <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "accept.tsv"), stringsAsFactors = FALSE, quote = "")
  exits <- pc_source_lib("errors")$prevcarga_exit_codes
  for (id in c("contract.accept_bad_config", "repro.accept_delta_nonzero", "repro.accept_unexpected_error", "repro.accept_infer_not_faster")) {
    row <- cat_[cat_$id == id, ]
    expect_identical(nrow(row), 1L, info = id)
    expect_identical(row$class, sub("\\..*$", "", id), info = id)
  }
  expect_identical(unname(exits[["repro"]]), 12L)
  for (f in c(acc_cases_script(), acc_cases_real()))
    expect_false(any(as.integer(charToRaw(paste(readLines(f), collapse = "\n"))) > 127L), info = f)
  expect_false(any(grepl("Sys\\.getenv|system2?\\(|aws", readLines(acc_cases_script())[!grepl("^#", readLines(acc_cases_script()))])))
})
