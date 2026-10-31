## test-ab-verdict-injection.R -- agregador do veredito do A/B (M8.8; ADR-0034 D23): comparadores MOCKADOS (linhas por estagio fabricadas),
## lista fechada de excecoes e recibos de fixture das pernas Linux, Windows (M8.9), VM limpa (M8.11) e NFR (M8.10). Cada FAIL forcado, um por
## vez, bloqueia listando EXATAMENTE aquele motivo. O arquivo real ab_excecoes.yaml e do Gabriel; os testes de injecao usam a copia de fixture.
FX <- AB$pc_ab_load_excecoes(ab_fixture_excecoes())
REC_OK <- list("cross-platform:linux" = "PASS", "cross-platform:windows" = "PASS", "windows-vm" = "PASS", "nfr-30min" = "PASS")

mk <- function(estagio, membro = "random-forest", area = "SP", status = "PASS", available_until = "2026-09-10T09:00", ...)
  AB$pc_ab_row(estagio, membro = membro, mdir = membro, sigla = membro, area = area, origem = "2026-09-10", modo = "x",
               available_until = available_until, tz = AB_TZ, status = status, ...)

base_rows <- function() AB$pc_ab_bind(list(
  mk("A", "random-forest"), mk("A", "lgbm"), mk("A", "holt-winters"), mk("B", "reconcile", "S"), mk("C", "export", "SIN"),
  mk("D", "combina", "SP"), mk("E", "avaliacao", "SP", "REPORT", detalhe = "wmape=0.01")))

blocked_by <- function(lin, rec = REC_OK, exc = FX) AB$pc_ab_verdict(lin, exc, rec)

test_that("[AB.V] A, B, C, D-propriedade PASS com >= 1 membro no Delta e Linux + Windows cobertos => CORTE-OK; excecoes listadas e fora da contagem", {
  v <- blocked_by(base_rows())
  expect_identical(v$veredito, "CORTE-OK"); expect_length(v$motivos, 0L); expect_identical(v$n_delta, 3L)
  ## um membro com celula em EXCECAO listada nao conta em Delta = 0 e nao bloqueia
  exc <- mk("A", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.mm_horas_completas", available_until = "2026-09-10T09:30")
  v2 <- blocked_by(AB$pc_ab_bind(list(base_rows(), exc)))
  expect_identical(v2$veredito, "CORTE-OK"); expect_identical(v2$excecoes, "regdin.mm_horas_completas")
  expect_false("dessem-regdin" %in% v2$membros_em_delta); expect_identical(v2$n_delta, 3L)
  ## um membro com PASS numa area e EXCECAO em outra tambem sai da contagem
  mixed <- AB$pc_ab_bind(list(base_rows(), mk("A", "dessem-regdin", "RS"), exc))
  expect_false("dessem-regdin" %in% blocked_by(mixed)$membros_em_delta)
})

test_that("[AB.V] CONTROL: FAIL forcado em A, B, C, D ou na perna Windows (um por vez) => CORTE-BLOQUEADO listando exatamente aquele", {
  force <- function(estagio, membro) { l <- base_rows(); i <- which(l$estagio == estagio & l$membro == membro)[[1L]]; l$status[[i]] <- "FAIL"; l }
  expect_identical(blocked_by(force("A", "lgbm"))$motivos, "estagio_A")
  expect_identical(blocked_by(force("B", "reconcile"))$motivos, "estagio_B")
  expect_identical(blocked_by(force("C", "export"))$motivos, "estagio_C")
  expect_identical(blocked_by(force("D", "combina"))$motivos, "estagio_D")
  for (s in c("A", "B", "C", "D")) expect_identical(blocked_by(force(s, c(A = "lgbm", B = "reconcile", C = "export", D = "combina")[[s]]))$veredito, "CORTE-BLOQUEADO")
  r <- REC_OK; r[["cross-platform:windows"]] <- "FAIL"
  vw <- blocked_by(base_rows(), r)
  expect_identical(vw$veredito, "CORTE-BLOQUEADO"); expect_identical(vw$motivos, "recibo:cross-platform:windows")
  for (g in names(REC_OK)) {                                   # recibo ausente (perna Linux, VM limpa, NFR) tambem bloqueia, so aquele
    r2 <- REC_OK; r2[[g]] <- NULL
    expect_identical(blocked_by(base_rows(), r2)$motivos, paste0("recibo:", g))
  }
  ## o detalhe do FAIL aponta a celula
  d <- blocked_by(force("A", "lgbm"))$detalhes
  expect_identical(d$onde, "lgbm/SP/2026-09-10")
})

test_that("[AB.V] divergencia fora de ab_excecoes.yaml => bloqueado; excecao do regdin 00:00 aplicada a deck com manha => bloqueado", {
  out <- mk("A", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.mm_horas_completas", available_until = "2026-09-10T09:00")   # :00, nao :30
  v <- blocked_by(AB$pc_ab_bind(list(base_rows(), out)))
  expect_identical(v$veredito, "CORTE-BLOQUEADO"); expect_match(v$motivos, "^excecao_fora_da_lista:A:dessem-regdin:regdin.mm_horas_completas$")
  desconhecida <- mk("A", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.inventada")
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), desconhecida)))$veredito, "CORTE-BLOQUEADO")
  sem_manha <- function(au) mk("A", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.sem_manha_0000", available_until = au)
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), sem_manha("2026-09-10T00:00"))))$veredito, "CORTE-OK")        # deck sem manha
  vm <- blocked_by(AB$pc_ab_bind(list(base_rows(), sem_manha("2026-09-10T09:00"))))                                           # deck COM manha
  expect_identical(vm$veredito, "CORTE-BLOQUEADO"); expect_match(vm$motivos, "regdin.sem_manha_0000")
  ## o membro certo e o estagio certo: a mesma id em outro estagio nao casa
  wrong_stage <- mk("B", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.sem_manha_0000", available_until = "2026-09-10T00:00")
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), wrong_stage)))$veredito, "CORTE-BLOQUEADO")
  ## o arquivo REAL (lista fechada): nao tem entrada para o apply do reconcile => a excecao do estagio B bloqueia
  real <- AB$pc_ab_load_excecoes(AB_EXC)
  rc <- AB$pc_ab_row("B", membro = "reconcile", mdir = "reconcile", sigla = "khat", area = "S", origem = "2026-09-10", modo = "apply",
                     khat_origin = "2026-09-05", status = "EXCECAO", excecao_id = "reconcile.apply_profile_reuse")
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), rc)), exc = real)$veredito, "CORTE-BLOQUEADO")
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), rc)), exc = FX)$veredito, "CORTE-OK")
})

test_that("[AB.V] 0 membros no Delta => bloqueado (zero_membros_no_delta)", {
  l <- base_rows(); l$status[l$estagio == "A"] <- "EXCECAO"; l$excecao_id[l$estagio == "A"] <- "regdin.mm_horas_completas"; l$available_until[] <- "2026-09-10T09:30"
  l$membro[l$estagio == "A"] <- "dessem-regdin"; l$mdir[l$estagio == "A"] <- "dessem-regdin"
  v <- blocked_by(l)
  expect_identical(v$veredito, "CORTE-BLOQUEADO"); expect_identical(v$motivos, "zero_membros_no_delta"); expect_identical(v$n_delta, 0L)
  vz <- blocked_by(AB$pc_ab_bind(list(mk("B", "reconcile", "S"), mk("C", "export", "SIN"), mk("D", "combina", "SP"))))
  expect_setequal(vz$motivos, c("estagio_A_ausente", "zero_membros_no_delta"))
  for (e in c("B", "C", "D")) {                                  # estagio nao rodado nunca e PASS silencioso
    l <- base_rows(); l <- l[l$estagio != e, ]
    expect_identical(blocked_by(l)$motivos, paste0("estagio_", e, "_ausente"))
  }
})

test_that("[AB.V] Estagio E x10 => veredito inalterado (a qualidade nunca entra no veredito)", {
  v0 <- blocked_by(base_rows())
  for (i in 1:10) {
    e <- mk("E", "avaliacao", "SP", "REPORT", detalhe = sprintf("wmape=%.6f", 0.01 * i), delta_max = i)
    e$status <- "REPORT"
    expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), e)))[c("veredito", "motivos", "excecoes", "n_delta")], v0[c("veredito", "motivos", "excecoes", "n_delta")])
  }
  ef <- mk("E", "avaliacao", "SP", "FAIL")                    # ate um FAIL fabricado no E nao bloqueia: E nao e estagio de veredito
  expect_identical(blocked_by(AB$pc_ab_bind(list(base_rows(), ef)))$veredito, "CORTE-OK")
})

test_that("[AB.V] carregador estrito de ab_excecoes.yaml: campo ausente, desconhecido ou condicao generica => erro de carga; o arquivo real carrega", {
  ex <- AB$pc_ab_load_excecoes(AB_EXC)
  expect_gte(length(ex), 3L)
  for (e in ex) {
    expect_true(all(AB$pc_ab_exc_required %in% names(e)))
    expect_true(nzchar(e$decisao) && nzchar(e$fonte))        # cada item cita a decisao do Gabriel e o doc de origem
  }
  expect_true(all(c("regdin.versao_pacotes", "dessem_linear.sp_2024_11_15_residuo_numerico") %in% vapply(ex, function(e) e$id, "")))
  item <- "{id: x.y, membro: m, estagio: A, condicao: \"minute(available_until) == 30\", justificativa: j, adr: ADR-1}"
  load1 <- function(...) { f <- tempfile(fileext = ".yaml"); writeLines(c("excecoes:", paste0("  - ", sub("^\\{", "{", paste0(...)))), f); tryCatch(AB$pc_ab_load_excecoes(f), prevcarga_error = function(e) e) }
  expect_type(load1(item), "list")
  for (campo in c("id", "membro", "estagio", "condicao", "justificativa", "adr")) {
    bad <- sub(paste0(campo, ": (\"[^\"]*\"|[^,}]*)(, )?"), "", item)
    e <- load1(bad)
    expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "contract.ab_excecoes_bad")
  }
  expect_identical(load1(sub("adr: ADR-1", "adr: ADR-1, extra: 1", item))$id, "contract.ab_excecoes_bad")      # campo desconhecido
  for (generica in c("TRUE", "sempre", "''", "1")) {
    e <- load1(sub("minute\\(available_until\\) == 30", generica, item, fixed = FALSE))
    expect_identical(e$id, "contract.ab_excecoes_bad")
  }
  expect_identical(load1(sub("minute\\(available_until\\) == 30", "system('x') == 1", item))$id, "contract.ab_excecoes_bad")   # funcao fora da whitelist
  expect_identical(load1(sub("estagio: A", "estagio: E", item))$id, "contract.ab_excecoes_bad")                       # E nunca tem excecao
  expect_identical(load1(item, "\n  - ", item)$id, "contract.ab_excecoes_bad")                                          # id duplicado
})

test_that("[AB.V] relatorio: ab_veredito.json + .md em ASCII puro e recibo `ab-corte` por bundle (so se pedido)", {
  dir <- tempfile("rep-"); rdir <- tempfile("rec-")
  bl <- tempfile("bundle-"); writeBin(as.raw(1:50), bl)
  bw <- tempfile("bundle-"); writeBin(as.raw(51:90), bw)
  lin <- AB$pc_ab_bind(list(base_rows(), mk("A", "dessem-regdin", "SP", "EXCECAO", excecao_id = "regdin.mm_horas_completas",
                                           available_until = "2026-09-10T09:30", detalhe = "excecao com acento: ca\u00e7ao")))
  v <- blocked_by(lin)
  out <- AB$pc_ab_report_write(v, lin, dir, bundles = list("linux-x86_64" = bl, "windows-x86_64" = bw), receipts_dir = rdir)
  for (f in c(out$json, out$md)) expect_true(all(as.integer(readBin(f, "raw", file.size(f))) < 128L))
  expect_identical(jsonlite::read_json(out$json)$veredito, "CORTE-OK")
  expect_length(out$recibos, 2L)
  sha <- digest::digest(file = bl, algo = "sha256")
  r <- AB$pc_receipt_read(sha, "ab-corte", rdir)
  expect_identical(r$resultado, "PASS"); expect_identical(r$veredito, "CORTE-OK")
  v2 <- blocked_by(base_rows(), list())                        # sem nenhum recibo: bloqueado e o recibo grava FAIL
  out2 <- AB$pc_ab_report_write(v2, base_rows(), tempfile("rep-"), bundles = list("linux-x86_64" = bl), receipts_dir = tempfile("rec-"))
  expect_identical(jsonlite::read_json(file.path(dirname(out2$json), "ab_veredito.json"))$veredito, "CORTE-BLOQUEADO")
  expect_identical(AB$pc_receipt_read(sha, "ab-corte", dirname(out2$recibos[[1L]]))$resultado, "FAIL")
  expect_null(AB$pc_receipt_read(sha, "ab-corte", tempfile("none-")))
  ## recibos por sha dos bundles (leitura)
  AB$pc_receipt_write("cross-platform", bl, "linux-x86_64", "PASS", dir = rdir, runtime_id = "r1")
  AB$pc_receipt_write("cross-platform", bw, "windows-x86_64", "PASS", dir = rdir, runtime_id = "r2")
  AB$pc_receipt_write("windows-vm", bw, "windows-x86_64", "PASS", dir = rdir, runtime_id = "r2")
  AB$pc_receipt_write("nfr-30min", bl, "linux-x86_64", "PASS", dir = rdir, runtime_id = "r1")
  rec <- AB$pc_ab_recibos_collect(list(linux = sha, windows = digest::digest(file = bw, algo = "sha256")), rdir)
  expect_identical(blocked_by(base_rows(), rec)$veredito, "CORTE-OK")
  expect_identical(blocked_by(base_rows(), AB$pc_ab_recibos_collect(list(linux = sha), rdir))$motivos,
                   c("recibo:cross-platform:windows", "recibo:windows-vm"))
})
