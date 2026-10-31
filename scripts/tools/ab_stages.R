## ab_stages.R -- comparadores dos estagios A-E do A/B do corte (M8.8; ADR-0034 D23, ADR-0036, ADR-0025, ADR-0032 c). Dev-only, so Linux.
## Requer carregados: errors.R, schema_long.R, features.R (prevcarga_delivery_grid), reconcile.R (prevcarga_khat_fit), combination.R
## (prevcarga_combina_gate), evaluation.R (prevcarga_scorecard), verify_repro.R (pc_vr_integrity), e arrow/jsonlite/digest/yaml/lubridate.
## Nenhuma variavel de ambiente; datas so por lubridate com tz explicito.
##
## Todo estagio devolve um data.frame com UMA linha por (estagio, membro, area, origem) -- a entrada do agregador (ab_report.R):
##   estagio A|B|C|D|E, membro, mdir, sigla, exogena, area, origem (AAAA-MM-DD), available_until (AAAA-MM-DDTHH:MM local), tz, plataforma,
##   modo, khat_origin, delta_max, delta_rel (max|D| / max|ref|), n, status PASS|FAIL|EXCECAO|REPORT, excecao_id, detalhe.
## Um membro com diferenca nunca vira Delta = 0 silencioso: ou PASS (<= limiar), ou EXCECAO casada com a lista fechada ab_excecoes.yaml
## (membro + estagio + condicao [+ teto]), ou FAIL. Excecao nunca entra na contagem de Delta = 0.

pc_ab_row_cols <- c("estagio", "membro", "mdir", "sigla", "exogena", "area", "origem", "available_until", "tz", "plataforma", "modo",
                    "khat_origin", "delta_max", "delta_rel", "n", "status", "excecao_id", "detalhe")

pc_ab_row <- function(estagio, membro = NA_character_, area = NA_character_, origem = NA_character_, status = "PASS", delta_max = NA_real_,
                      delta_rel = NA_real_, n = NA_integer_, mdir = NA_character_, sigla = NA_character_, exogena = NA_character_,
                      available_until = NA_character_, tz = NA_character_, plataforma = "linux-x86_64", modo = NA_character_,
                      khat_origin = NA_character_, excecao_id = NA_character_, detalhe = "") {
  data.frame(estagio = estagio, membro = membro, mdir = mdir, sigla = sigla, exogena = exogena, area = area, origem = origem,
             available_until = available_until, tz = tz, plataforma = plataforma, modo = modo, khat_origin = khat_origin,
             delta_max = as.double(delta_max), delta_rel = as.double(delta_rel), n = as.integer(n), status = status,
             excecao_id = excecao_id, detalhe = detalhe, stringsAsFactors = FALSE)[pc_ab_row_cols]
}

pc_ab_bind <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(pc_ab_row("A")[0L, ])
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

## --- lista fechada de excecoes (ab_excecoes.yaml) ------------------------------------------------------------
pc_ab_exc_required <- c("id", "membro", "estagio", "condicao", "justificativa", "adr")
pc_ab_exc_optional <- c("limite", "janela_final_h", "decisao", "fonte")
pc_ab_exc_funs <- c("==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "(", "%in%", "c", "hour", "minute", "month", "year", "as.Date", "is.na")
pc_ab_case_fields <- c("membro", "mdir", "sigla", "exogena", "area", "origem", "available_until", "modo", "khat_origin")

.pc_ab_exc_bad <- function(path, id, why) prevcarga_abort("contract.ab_excecoes_bad", where = paste0(path, ":", id), details = list(why = why))

pc_ab_load_excecoes <- function(path) {
  y <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (!is.list(y) || !identical(names(y), "excecoes")) .pc_ab_exc_bad(path, "-", "o arquivo deve ter so a chave `excecoes`")
  items <- y$excecoes
  if (is.null(items)) items <- list()
  if (!is.list(items)) .pc_ab_exc_bad(path, "-", "`excecoes` deve ser uma lista")
  seen <- character()
  lapply(items, function(e) {
    id <- if (is.list(e) && is.character(e$id) && length(e$id) == 1L) e$id else "?"
    if (!is.list(e) || is.null(names(e))) .pc_ab_exc_bad(path, id, "item deve ser um mapa")
    miss <- setdiff(pc_ab_exc_required, names(e))
    if (length(miss)) .pc_ab_exc_bad(path, id, paste("campo obrigatorio ausente:", miss[[1L]]))
    unk <- setdiff(names(e), c(pc_ab_exc_required, pc_ab_exc_optional))
    if (length(unk)) .pc_ab_exc_bad(path, id, paste("campo desconhecido:", unk[[1L]]))
    for (f in pc_ab_exc_required)
      if (!is.character(e[[f]]) || length(e[[f]]) != 1L || is.na(e[[f]]) || !nzchar(trimws(e[[f]])))
        .pc_ab_exc_bad(path, id, paste("campo vazio:", f))
    if (!(e$estagio %in% c("A", "B", "C", "D"))) .pc_ab_exc_bad(path, id, "estagio deve ser A, B, C ou D")
    if (id %in% seen) .pc_ab_exc_bad(path, id, "id duplicado")
    seen <<- c(seen, id)
    ex <- tryCatch(parse(text = e$condicao, keep.source = FALSE), error = function(err) NULL)
    if (is.null(ex) || length(ex) != 1L) .pc_ab_exc_bad(path, id, "condicao nao e uma expressao")
    if (!length(intersect(all.vars(ex[[1L]]), pc_ab_case_fields)) || !is.language(ex[[1L]]))
      .pc_ab_exc_bad(path, id, "condicao generica: deve depender de campos do caso")
    bad <- setdiff(all.names(ex[[1L]], functions = TRUE), c(pc_ab_exc_funs, all.vars(ex[[1L]])))
    if (length(bad)) .pc_ab_exc_bad(path, id, paste("funcao nao permitida na condicao:", bad[[1L]]))
    if (!is.null(e$limite) && (!is.list(e$limite) || !length(setdiff(names(e$limite), c("max_abs", "max_rel"))) == 0L ||
                               !all(vapply(e$limite, function(v) is.numeric(v) && length(v) == 1L && v > 0, NA))))
      .pc_ab_exc_bad(path, id, "limite deve ser {max_abs, max_rel} numericos positivos")
    if (!is.null(e$janela_final_h) && !(is.numeric(e$janela_final_h) && length(e$janela_final_h) == 1L && e$janela_final_h > 0))
      .pc_ab_exc_bad(path, id, "janela_final_h deve ser numerico positivo")
    e$.expr <- ex[[1L]]
    e
  })
}

.pc_ab_row_env <- function(linha) {
  tz <- if (!is.null(linha$tz) && !is.na(linha$tz)) linha$tz else "UTC"
  au <- if (!is.null(linha$available_until) && !is.na(linha$available_until))
    lubridate::ymd_hm(sub("T", " ", linha$available_until, fixed = TRUE), tz = tz) else as.POSIXct(NA, tz = tz)
  env <- new.env(parent = baseenv())
  env$hour <- lubridate::hour; env$minute <- lubridate::minute; env$month <- lubridate::month; env$year <- lubridate::year
  env$membro <- linha$membro; env$mdir <- linha$mdir; env$sigla <- linha$sigla; env$exogena <- linha$exogena; env$area <- linha$area
  env$origem <- if (!is.na(linha$origem)) as.Date(linha$origem) else as.Date(NA)
  env$available_until <- au
  env$modo <- linha$modo
  env$khat_origin <- if (!is.na(linha$khat_origin)) as.Date(linha$khat_origin) else as.Date(NA)
  env
}

## A excecao casa so se membro, estagio E condicao casam (membro = o `membro` da linha, o mdir ou a sigla). Devolve TODOS os itens casados.
pc_ab_match_all <- function(linha, excecoes) {
  Filter(function(e) {
    identical(e$estagio, linha$estagio) && e$membro %in% c(linha$membro, linha$mdir, linha$sigla) &&
      tryCatch(isTRUE(eval(e$.expr, .pc_ab_row_env(linha))), error = function(err) FALSE)
  }, excecoes)
}

## O primeiro item casado, ou NULL.
pc_ab_match_excecao <- function(linha, excecoes) {
  m <- pc_ab_match_all(linha, excecoes)
  if (length(m)) m[[1L]] else NULL
}

## Classifica uma linha: PASS (|D| <= tol em tudo) | EXCECAO (algum item casado cobre a divergencia, dentro do teto e da janela) | FAIL.
## `d` = |D| por instante; `tgt` = instante (numerico) de cada um e `band_end` = fim da faixa (para janela_final_h; NULL => sem janela).
.pc_ab_classify <- function(linha, d, tgt, band_end, ref_max, tol, excecoes) {
  if (!length(d) || max(d) <= tol) return(list(status = "PASS", id = NA_character_, detalhe = ""))
  cand <- pc_ab_match_all(linha, excecoes)
  if (!length(cand)) return(list(status = "FAIL", id = NA_character_, detalhe = "divergencia fora de ab_excecoes.yaml"))
  why <- character()
  for (e in cand) {
    din <- d; dout <- numeric()
    if (!is.null(e$janela_final_h)) {
      inwin <- if (is.null(tgt)) rep(TRUE, length(d)) else tgt > band_end - e$janela_final_h * 3600
      din <- d[inwin]; dout <- d[!inwin]
    }
    if (length(dout) && max(dout) > tol) { why <- c(why, sprintf("excecao %s so cobre as ultimas %g h; fora delas max %.3g", e$id, e$janela_final_h, max(dout))); next }
    dmax <- if (length(din)) max(din) else 0
    if (!is.null(e$limite$max_abs) && dmax > e$limite$max_abs) { why <- c(why, sprintf("teto max_abs %.3g da excecao %s excedido: %.6g", e$limite$max_abs, e$id, dmax)); next }
    if (!is.null(e$limite$max_rel) && ref_max > 0 && dmax / ref_max > e$limite$max_rel) {
      why <- c(why, sprintf("teto max_rel %.3g da excecao %s excedido: %.6g", e$limite$max_rel, e$id, dmax / ref_max)); next
    }
    return(list(status = "EXCECAO", id = e$id, detalhe = sprintf("excecao %s: max|D| %.6g", e$id, dmax)))
  }
  list(status = "FAIL", id = cand[[1L]]$id, detalhe = paste(why, collapse = "; "))
}

## --- Estagio A: y_pred por membro x area x origem na faixa delivery_days -----------------------------------
.pc_ab_read <- function(f) as.data.frame(arrow::read_parquet(f))

## Dia O do oraculo: o passo da origem O quando existe; senao o da origem O-1 (RF: infer em O-1 com o MESMO artefato, modo split do runner; o HW
## traz o dia O na propria origem O pelo patch hw_keep_step1; o dessem em corte intradiario ancora em O). Sempre dentro da grade de entrega;
## `fonte` diz de qual origem veio cada instante.
.pc_ab_ref_pick <- function(ref, area, origem, grid_num) {
  fo <- as.character(origem); fo1 <- as.character(origem - 1L)
  r <- ref[ref$area == area & as.numeric(ref$target_datetime) %in% grid_num, , drop = FALSE]
  r0 <- r[r$forecast_origin == fo, , drop = FALSE]
  r1 <- r[r$forecast_origin == fo1 & !(as.numeric(r$target_datetime) %in% as.numeric(r0$target_datetime)), , drop = FALSE]
  r1$.fonte <- rep("O-1", nrow(r1)); r0$.fonte <- rep("O", nrow(r0))
  out <- rbind(r0, r1)
  out[order(as.numeric(out$target_datetime)), , drop = FALSE]
}

## De onde o RUNNER tirou o dia O do oraculo para o caso (`<ref>/fonte_dia_O.tsv` do relayout: modo split = O-1 do mesmo artefato; patch = passo 1
## do fit em O); "" sem o arquivo (fixtures sinteticas).
.pc_ab_oracle_fonte <- function(ref, cs) {
  f <- file.path(ref, "fonte_dia_O.tsv")
  if (!file.exists(f)) return("")
  t <- utils::read.delim(f, colClasses = "character", na.strings = character())
  v <- t$fonte_dia_O[t$origem == cs$origem & t$sigla == cs$sigla & t$area == cs$area]
  if (length(v)) v[[1L]] else ""
}

pc_ab_stage_a <- function(clean, ref, cases, ctx, excecoes = list(), tol = 1e-5, request_end = NULL, plataforma = "linux-x86_64") {
  cases <- cases[cases$executavel, , drop = FALSE]
  for (o in unique(cases$origem)) for (d in c(clean, ref)) {
    od <- file.path(d, o)
    if (!dir.exists(od)) prevcarga_abort("contract.ab_stage_input", where = od)
    if (exists("pc_vr_integrity", mode = "function")) pc_vr_integrity(od)          # fase 1 do verificar: sha256 de todo arquivo da arvore
  }
  rows <- lapply(seq_len(nrow(cases)), function(i) {
    cs <- cases[i, ]
    base <- list(estagio = "A", membro = cs$membro, mdir = cs$mdir, sigla = cs$sigla, exogena = cs$exogena, area = cs$area,
                 origem = cs$origem, available_until = cs$available_until, tz = cs$tz, plataforma = plataforma, modo = "por_origem")
    mk <- function(...) do.call(pc_ab_row, c(base, list(...)))
    O <- as.Date(cs$origem); tz <- cs$tz
    au <- lubridate::ymd_hm(sub("T", " ", cs$available_until, fixed = TRUE), tz = tz)
    grid <- prevcarga_delivery_grid(list(available_until = au, tz = tz, origin_day = O), cs$delivery_days)
    if (!is.null(request_end) && !is.null(request_end[[cs$sigla]]) && as.Date(request_end[[cs$sigla]]) > O + cs$delivery_days - 1L)
      prevcarga_abort("contract.ab_fora_da_faixa", where = cs$sigla,
                      details = list(pedido = as.character(request_end[[cs$sigla]]), ultimo_dia_da_faixa = as.character(O + cs$delivery_days - 1L)))
    cf <- file.path(clean, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet"))
    rf <- file.path(ref, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet"))
    if (!file.exists(cf) || !file.exists(rf))
      return(mk(status = "FAIL", detalhe = paste("entrada ausente:", if (!file.exists(cf)) "clean-room" else "oraculo")))
    gnum <- as.numeric(grid)
    cl <- .pc_ab_read(cf); cl <- cl[cl$area == cs$area & cl$forecast_origin == cs$origem, , drop = FALSE]
    n_fora <- sum(!(as.numeric(cl$target_datetime) %in% gnum))
    cl <- cl[as.numeric(cl$target_datetime) %in% gnum, , drop = FALSE]
    rp <- .pc_ab_ref_pick(.pc_ab_read(rf), cs$area, O, gnum)
    ck <- as.numeric(cl$target_datetime); rk <- as.numeric(rp$target_datetime)
    sem_par <- length(setdiff(gnum, intersect(ck, rk)))
    if (sem_par > 0L)
      return(mk(status = "FAIL", n = length(intersect(ck, rk)),
                detalhe = sprintf("chaves sem par na faixa: %d de %d (clean %d, oraculo %d, fora_da_faixa clean %d)", sem_par, length(gnum),
                                  length(ck), length(rk), n_fora)))
    j <- match(gnum, ck); k <- match(gnum, rk)
    y <- cl$y_pred[j]; z <- rp$y_pred[k]
    d <- abs(y - z)
    d[is.na(d)] <- Inf
    ref_max <- max(abs(z), 0)
    linha <- do.call(pc_ab_row, c(base, list(delta_max = max(d), n = length(d))))
    cl2 <- .pc_ab_classify(linha, d, gnum, as.numeric(grid[length(grid)]), ref_max, tol, excecoes)
    fonte <- if (all(rp$.fonte[k] == "O")) "O" else if (all(rp$.fonte[k] == "O-1")) "O-1" else "O+O-1"
    od <- .pc_ab_oracle_fonte(ref, cs)
    mk(status = cl2$status, excecao_id = cl2$id, delta_max = max(d), delta_rel = if (ref_max > 0) max(d) / ref_max else NA_real_, n = length(d),
       detalhe = trimws(paste0(cl2$detalhe, sprintf(" fonte_dia_O=%s%s fora_da_faixa=%d", fonte, if (nzchar(od)) paste0(" oraculo_dia_O=", od) else "",
                                                    n_fora))))
  })
  pc_ab_bind(rows)
}

## --- Estagio B: k-hat por origem (FORBIDDEN <= 1e-5) e apply de producao ----------------------------------------
## ratio = saida de prevcarga_khat_ratio (interval_start, interval_end, ratio); ref_profile = perfil do oraculo (hour, month, dow, k; rotulo de
## INICIO). `apply` (opcional) = list(khat_origin, khat_age_days, n_fallback, long, ref_long): o apply de producao com um perfil `latest`.
pc_ab_stage_b <- function(ratio, origem, ref_profile, window_days, ss, excecoes = list(), tol = 1e-5, apply = NULL, plataforma = "linux-x86_64") {
  O <- as.Date(origem)
  fit <- prevcarga_khat_fit(ratio, O, window_days)
  p <- as.data.frame(fit$profile)
  key <- function(x) paste(x$hour, x$month, x$dow)
  r <- as.data.frame(ref_profile)
  base <- list(estagio = "B", membro = "reconcile", mdir = "reconcile", sigla = "khat", area = ss, origem = as.character(O), plataforma = plataforma)
  sem_par <- length(setdiff(key(p), key(r))) + length(setdiff(key(r), key(p)))
  out <- list()
  if (sem_par > 0L) {
    out[[1L]] <- do.call(pc_ab_row, c(base, list(modo = "por_origem", status = "FAIL", n = nrow(p), detalhe = sprintf("buckets sem par: %d", sem_par))))
  } else {
    d <- abs(p$k - r$k[match(key(p), key(r))])
    linha <- do.call(pc_ab_row, c(base, list(modo = "por_origem", delta_max = max(d), n = length(d))))
    c1 <- .pc_ab_classify(linha, d, NULL, NULL, max(abs(r$k)), tol, excecoes)
    out[[1L]] <- do.call(pc_ab_row, c(base, list(modo = "por_origem", status = c1$status, excecao_id = c1$id, delta_max = max(d),
                                                 delta_rel = max(d) / max(abs(r$k)), n = length(d), detalhe = c1$detalhe)))
  }
  if (!is.null(apply)) {
    ko <- as.Date(apply$khat_origin)
    det <- sprintf("khat_origin=%s khat_age_days=%d n_fallback=%d", as.character(ko), as.integer(apply$khat_age_days), as.integer(apply$n_fallback))
    ab <- c(base, list(modo = "apply", khat_origin = as.character(ko)))
    if (ko >= O) {                                      # politica atual (khat_origin == O): o apply deve igualar a referencia
      a <- apply$long; z <- apply$ref_long
      j <- match(as.numeric(a$target_datetime), as.numeric(z$target_datetime))
      d <- abs(a$y_pred - z$y_pred[j]); d[is.na(d)] <- Inf
      linha <- do.call(pc_ab_row, c(ab, list(delta_max = max(d), n = length(d))))
      c2 <- .pc_ab_classify(linha, d, NULL, NULL, max(abs(z$y_pred)), tol, excecoes)
      out[[2L]] <- do.call(pc_ab_row, c(ab, list(status = c2$status, excecao_id = c2$id, delta_max = max(d), n = length(d),
                                                 detalhe = trimws(paste(c2$detalhe, det)))))
    } else {                                            # perfil reutilizado: diverge de proposito; so e excecao se a lista a conhece
      linha <- do.call(pc_ab_row, c(ab, list(n = nrow(apply$long))))
      e <- pc_ab_match_excecao(linha, excecoes)
      out[[2L]] <- do.call(pc_ab_row, c(ab, list(status = if (is.null(e)) "FAIL" else "EXCECAO", excecao_id = if (is.null(e)) NA_character_ else e$id,
                                                 n = nrow(apply$long),
                                                 detalhe = paste(if (is.null(e)) "perfil reutilizado fora de ab_excecoes.yaml;" else "perfil reutilizado;", det))))
    }
  }
  pc_ab_bind(out)
}

## --- Estagio C: entregas SIN / subsistemas / folhas (copia de bytes do M6.2) --------------------------------
.pc_ab_c_files <- function(dir) {
  mf <- file.path(dir, "manifest.json")
  m <- if (file.exists(mf)) tryCatch(jsonlite::read_json(mf, simplifyVector = FALSE), error = function(e) NULL) else NULL
  ent <- if (is.null(m)) NULL else if (!is.null(m$arquivos)) m$arquivos else m$files
  if (is.null(ent)) prevcarga_abort("contract.ab_stage_input", where = mf)
  p <- vapply(ent, function(e) as.character(e$path), "")
  p[grepl("^(SIN[.]parquet|subsistemas/|folhas/)", p)]
}

## O sha256 vem do DISCO (nao do manifest): regravar com outro codec muda o sha mesmo com manifest coerente.
pc_ab_stage_c <- function(saida, referencia, origem = NA_character_, plataforma = "linux-x86_64") {
  ps <- .pc_ab_c_files(saida); pr <- .pc_ab_c_files(referencia)
  sha <- function(d, p) if (file.exists(file.path(d, p))) digest::digest(file = file.path(d, p), algo = "sha256") else NA_character_
  all_p <- sort(union(ps, pr), method = "radix")
  rows <- lapply(all_p, function(p) {
    a <- if (p %in% ps) sha(saida, p) else NA_character_
    b <- if (p %in% pr) sha(referencia, p) else NA_character_
    st <- if (is.na(a) || is.na(b)) "FAIL" else if (identical(a, b)) "PASS" else "FAIL"
    pc_ab_row("C", membro = "export", mdir = "export", area = sub("[.]parquet$", "", basename(p)), origem = origem, plataforma = plataforma,
              modo = "bytes", status = st,
              detalhe = if (is.na(a) || is.na(b)) paste("conjunto de arquivos difere:", p) else if (st == "FAIL") paste("sha256 difere:", p) else "")
  })
  if (!length(rows)) rows <- list(pc_ab_row("C", membro = "export", mdir = "export", origem = origem, plataforma = plataforma, modo = "bytes",
                                            status = "FAIL", detalhe = "nenhum arquivo SIN/subsistemas/folhas listado"))
  pc_ab_bind(rows)
}

## --- Estagio D: so os gates de PROPRIEDADE da combina (sem paridade de solver, sem qualidade/dominancia, sem eps; ADR-0025) ----
pc_ab_stage_d <- function(panel, ctx = NULL, origem = NA_character_, area = NA_character_, t_cut = NULL, n_min = 24L, plataforma = "linux-x86_64") {
  g <- prevcarga_combina_gate(panel, ctx = ctx, t_cut = t_cut, n_min = n_min)
  pr <- g$property
  ok <- isTRUE(g$pass)
  pc_ab_row("D", membro = "combina", mdir = "combina", area = area, origem = origem, plataforma = plataforma, modo = "propriedade",
            status = if (ok) "PASS" else "FAIL",
            detalhe = sprintf("simplex_ok=%s max_grad_norm=%.3g", as.character(isTRUE(pr$simplex_ok)), as.numeric(pr$max_grad_norm)))
}

## --- Estagio E: qualidade no holdout real, SO reportada (nunca entra no veredito) ------------------------------
pc_ab_stage_e <- function(long, ctx, origem = NA_character_, plataforma = "linux-x86_64") {
  long <- prevcarga_assert_long(long, ctx)
  sc <- as.data.frame(prevcarga_scorecard(long, by = c("d_plus", "area")))
  if (!nrow(sc)) return(pc_ab_row("E", membro = "avaliacao", origem = origem, status = "REPORT", detalhe = "sem linhas com y_true"))
  rows <- lapply(seq_len(nrow(sc)), function(i)
    pc_ab_row("E", membro = "avaliacao", mdir = "avaliacao", area = as.character(sc$area[[i]]), origem = origem, plataforma = plataforma,
              modo = paste0("d_plus=", sc$d_plus[[i]]), status = "REPORT", n = sc$n[[i]],
              detalhe = sprintf("wmape=%.6g", as.numeric(sc$wmape[[i]]))))
  pc_ab_bind(rows)
}

## --- limiares fixos (tests/ab/limiares_ab.yaml): A e B = 1e-5 absoluto; C = sha256; D = propriedade; E = so reporta ----------------
pc_ab_load_limiares <- function(path) {
  y <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  l <- y$limiares
  ok <- is.list(l) && identical(sort(names(l)), c("A", "B", "C", "D", "E")) &&
    identical(l$A$tipo, "abs") && identical(l$B$tipo, "abs") && identical(l$C$tipo, "sha256") && is.numeric(l$A$valor) && is.numeric(l$B$valor)
  if (!ok) prevcarga_abort("contract.ab_stage_input", where = path)
  list(A = l$A$valor, B = l$B$valor)
}

## --- Produtor do clean-room (estagio A) -------------------------------------------------------------------------
## Por caso executavel: treino pela fachada (a fachada deriva o corte de treino pela classe de split, ADR-0023) e infer com `truncate_at` =
## available_until (o leitor deriva o corte do dado) na regra de clima do oraculo (ADR-0024: origin_midnight_utc); o LONG do membro sai pelo
## conversor unico (build_long_output.R) em `<out>/<origem>/membros/<sigla>/<area>.parquet` + manifest.json. Requer as libs da fachada
## (training.R, inference.R, build_long_output.R, registry.R, dates.R...) carregadas. `ctx` = list(root, data_dir [overlay do store], state_dir
## [NOVO, so deste A/B], topology_path?, code_commit, renv_lock_hash). Nunca escreve em data_dir.
pc_ab_clean_run <- function(cases, ctx, out_dir, weather_cutoff = "origin_midnight_utc") {
  cases <- cases[cases$executavel, , drop = FALSE]
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    O <- as.Date(cs$origem)
    au <- lubridate::ymd_hm(sub("T", " ", cs$available_until, fixed = TRUE), tz = cs$tz)
    c2 <- utils::modifyList(ctx, list(origin = O, weather_issue_cutoff = weather_cutoff, tz = cs$tz))
    member <- list(sigla = cs$sigla, mdir = cs$mdir, exogena = cs$exogena)
    suppressWarnings(prevcarga_train(member, cs$area, O, c2))
    res <- suppressWarnings(prevcarga_infer_cell(member, cs$area, O, c2, truncate_at = au))
    dl <- data.table::data.table(interval_end = res$predictions$target_datetime, y_pred = res$predictions$y_pred)
    long <- prevcarga_build_long(dl, list(name = cs$sigla, delivery_days = cs$delivery_days), cs$area, O, res$available_until_posix,
                                 truth = NULL, ctx = list(tz = cs$tz))
    .pc_ab_write_parquet(as.data.frame(long), file.path(out_dir, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet")))
  }
  for (o in list.dirs(out_dir, recursive = FALSE, full.names = TRUE)) pc_ab_manifest_write(o)
  invisible(out_dir)
}
