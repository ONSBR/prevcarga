## ab_report.R -- agregador do veredito do A/B do corte (M8.8; ADR-0034 D23). Dev-only, so Linux, fora do bundle.
## Requer carregados: ab_stages.R (pc_ab_match_excecao, pc_ab_bind), receipts.R (so para gravar o recibo `ab-corte`), jsonlite, digest.
## Sem variavel de ambiente; sem relogio no veredito (o recibo carrega a data, pc_receipt_write).
##
## CORTE-OK sse: A, B, C e D-propriedade com linhas e sem FAIL (estagio sem linha = `estagio_<E>_ausente`); >= 1 membro ficou em Delta = 0 (todas as celulas dele PASS no A); toda linha EXCECAO casa
## com a lista fechada ab_excecoes.yaml (membro + estagio + condicao, reavaliada aqui sobre o proprio caso); e os recibos PASS das pernas
## Linux e Windows (cross-platform, M8.9), da VM limpa (windows-vm, M8.11) e do NFR (nfr-30min, M8.10) existem. Caso contrario CORTE-BLOQUEADO
## com a lista EXATA dos motivos. Excecoes ficam fora da contagem de Delta = 0. O estagio E (qualidade) e so reportado, nunca entra.

pc_ab_recibos_exigidos <- c("cross-platform:linux", "cross-platform:windows", "windows-vm", "nfr-30min")

## recibos = lista nomeada chave -> "PASS" | "FAIL" | NULL/ausente (ausente = nao existe). Chaves: pc_ab_recibos_exigidos.
pc_ab_verdict <- function(linhas, excecoes, recibos, exigidos = pc_ab_recibos_exigidos) {
  motivos <- character()
  det <- list()
  for (e in c("A", "B", "C", "D")) {
    if (!any(linhas$estagio == e)) motivos <- c(motivos, paste0("estagio_", e, "_ausente"))      # estagio nao rodado nunca e PASS
    bad <- linhas[linhas$estagio == e & linhas$status == "FAIL", , drop = FALSE]
    if (nrow(bad)) {
      motivos <- c(motivos, paste0("estagio_", e))
      det[[length(det) + 1L]] <- data.frame(motivo = paste0("estagio_", e), onde = paste(bad$membro, bad$area, bad$origem, sep = "/"),
                                            detalhe = bad$detalhe, stringsAsFactors = FALSE)
    }
  }
  exc <- linhas[linhas$status == "EXCECAO", , drop = FALSE]
  ids <- character()
  fora <- character()
  for (i in seq_len(nrow(exc))) {
    e <- pc_ab_match_excecao(exc[i, ], excecoes)
    if (is.null(e) || !identical(e$id, exc$excecao_id[[i]])) fora <- c(fora, paste0(exc$estagio[[i]], ":", exc$membro[[i]], ":", exc$excecao_id[[i]]))
    else ids <- c(ids, e$id)
  }
  if (length(fora)) {
    motivos <- c(motivos, paste0("excecao_fora_da_lista:", unique(fora)))
    det[[length(det) + 1L]] <- data.frame(motivo = "excecao_fora_da_lista", onde = fora, detalhe = "linha EXCECAO sem item casado em ab_excecoes.yaml",
                                          stringsAsFactors = FALSE)
  }
  a <- linhas[linhas$estagio == "A", , drop = FALSE]
  em_delta <- if (nrow(a)) sort(unique(a$membro[ave(a$status == "PASS", a$membro, FUN = all)]), method = "radix") else character()
  if (!length(em_delta)) motivos <- c(motivos, "zero_membros_no_delta")
  for (g in exigidos) if (!isTRUE(identical(as.character(recibos[[g]]), "PASS"))) motivos <- c(motivos, paste0("recibo:", g))
  list(veredito = if (length(motivos)) "CORTE-BLOQUEADO" else "CORTE-OK", motivos = unique(motivos), excecoes = sort(unique(ids), method = "radix"),
       membros_em_delta = em_delta, n_delta = length(em_delta),
       detalhes = if (length(det)) do.call(rbind, det) else data.frame(motivo = character(), onde = character(), detalhe = character()),
       recibos = vapply(exigidos, function(g) if (is.null(recibos[[g]])) "AUSENTE" else as.character(recibos[[g]]), ""),
       contagem = as.list(table(factor(linhas$status, levels = c("PASS", "EXCECAO", "FAIL", "REPORT")))))
}

## Recibos por sha do bundle (pc_receipt_read): bundles = list(linux = <sha256>, windows = <sha256>); dir = tests/.receipts.
pc_ab_recibos_collect <- function(bundles, dir = "tests/.receipts") {
  res <- function(sha, gate) {
    r <- if (is.null(sha)) NULL else pc_receipt_read(sha, gate, dir)
    if (is.null(r)) NULL else as.character(r$resultado)
  }
  out <- list("cross-platform:linux" = res(bundles$linux, "cross-platform"), "cross-platform:windows" = res(bundles$windows, "cross-platform"),
              "windows-vm" = res(bundles$windows, "windows-vm"), "nfr-30min" = res(bundles$linux, "nfr-30min"))
  Filter(Negate(is.null), out)
}

.pc_ab_ascii <- function(x) iconv(x, "UTF-8", "ASCII", sub = "?")

## ab_veredito.json + ab_veredito.md (ASCII) em `dir`; com `bundles` (list plataforma -> caminho do bundle) grava o recibo `ab-corte` de cada um.
pc_ab_report_write <- function(v, linhas, dir, bundles = NULL, receipts_dir = "tests/.receipts") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  js <- list(veredito = v$veredito, motivos = as.list(v$motivos), excecoes = as.list(v$excecoes), n_delta = v$n_delta,
             membros_em_delta = as.list(v$membros_em_delta), recibos = as.list(v$recibos), contagem = v$contagem,
             detalhes = lapply(seq_len(nrow(v$detalhes)), function(i) as.list(v$detalhes[i, ])))
  jf <- file.path(dir, "ab_veredito.json")
  jsonlite::write_json(js, jf, auto_unbox = TRUE, pretty = TRUE)
  fail <- linhas[linhas$status %in% c("FAIL", "EXCECAO"), , drop = FALSE]
  md <- c(paste0("# A/B do corte: ", v$veredito), "",
          sprintf("membros em Delta = 0: %d (%s)", v$n_delta, paste(v$membros_em_delta, collapse = ", ")),
          sprintf("excecoes (fora da contagem de Delta = 0): %s", if (length(v$excecoes)) paste(v$excecoes, collapse = ", ") else "nenhuma"),
          "", "## Motivos", if (length(v$motivos)) paste0("- ", v$motivos) else "- nenhum", "",
          "## Recibos", paste0("- ", names(v$recibos), ": ", v$recibos), "",
          "## Linhas com FAIL ou EXCECAO",
          if (nrow(fail)) sprintf("- %s %s %s %s [%s] %s", fail$estagio, fail$membro, fail$area, fail$origem, fail$status, fail$detalhe) else "- nenhuma")
  mf <- file.path(dir, "ab_veredito.md")
  writeLines(.pc_ab_ascii(md), mf, useBytes = TRUE)
  rec <- character()
  if (!is.null(bundles)) {
    for (p in names(bundles))
      rec <- c(rec, pc_receipt_write("ab-corte", bundles[[p]], platform = p, resultado = if (identical(v$veredito, "CORTE-OK")) "PASS" else "FAIL",
                                     veredito = v$veredito, dir = receipts_dir))
  }
  invisible(list(json = jf, md = mf, recibos = rec))
}
