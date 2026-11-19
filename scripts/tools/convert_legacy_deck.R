#!/usr/bin/env Rscript
## convert_legacy_deck.R -- utilitario de DESENVOLVIMENTO (fora do pacote): converte o deck legado do ONS
## (CSV.gz por area, `s3://.../prevcarga/decks/<AREA>/`) no deck novo (parquet Hive + manifest.json) lido por deck_reader.R.
## O produto (CLI) nunca baixa nem converte: o download e passo separado (aws s3 sync) e este utilitario so le um diretorio local.
##
## Uso: Rscript --vanilla scripts/tools/convert_legacy_deck.R --source <dir-legado> --dest <dir-novo>
##                                                           [--areas A,B,...] [--load-label end|start]
##
## Regras (espelham scripts/data-prep/migrate_legacy_data.py do oraculo, que gerou o store atual):
## - load            <- CARGASHHIST (semi-horario; CARGAHIST horario e ignorado). A fonte rotula pelo INICIO do intervalo
##                      (00:00 = 00:00-00:30). `--load-label end` (default) grava +30 min (rotulo no FIM) e declara `end`;
##                      `start` grava como vem. Particao `year` = ano do INICIO do intervalo nos dois modos (23:30-24:00 de
##                      31/12 fica no arquivo do proprio ano). Carga <= 0 vira faltante. Meia hora isolada faltando (um unico
##                      slot entre dois presentes) e preenchida pelo ponto medio dos vizinhos e listada no log (generaliza o
##                      reparo PA 2025-07-30 09:00; cobre as 8 meias horas negativas de GO 2020-2022).
## - weather_observed <- TEMPHIST (horario) -> grade de 30 min pela desagregaModeloSH do legado (media de duas monoH.FC no
##                      eixo de indice, pkgfun_funcoesApoio.R:388-410) com ajuste ADITIVO por hora (Gabriel 2026-11-11): a hora H
##                      (media de [H, H+1)) vira o par (H:00, H:30) com media = valor horario, inclusive a ultima hora. Desagrega
##                      por corrida contigua de horas; dentro de buraco (hora ausente) a meia hora sai da reta entre as meias
##                      horas das bordas; heat_index = NA. DESVIO do migrate_legacy_data.py (que usava spline cubica not-a-knot).
## - weather_forecast <- TEMPPREVHIST: issue_time = dat_referencia 00:00 (data da emissao; fuso UTC no manifest),
##                      mesma desagregacao por emissao (a emissao que termina em O+9 23:00 ganha o 23:30), mesma reta de buraco
##                      (emissoes trihorarias de 2020-05 e 2024-02-29); particao `year` = ano de valid_time.
## - holidays        <- FERIADOS (sem duplicata de data) + reparo add-only de TON por consenso das folhas (>= 19 de 23).
## - dst_periods     <- HORAVERAO (igual em todas as areas; usa a primeira que tem).
## - load_tiers      <- PATAMARES no formato novo (`Patamar;Intervalo;DiaSemana;Mes;HoraIni;HoraFim`): UM arquivo para todas as areas
##                      (`auxiliary/load_tiers/data.parquet`, sem particao, como dst_periods). Hoje so SECO/S/N/NE o trazem (identicos);
##                      as folhas ainda tem o arquivo velho (`Hora` x estacao), ignorado. Copias novas divergentes => aborta.
## Saida: 0 ok; 8 [config] uso invalido ou --dest nao vazio; 2 [contract] fonte ausente.
args <- commandArgs(trailingOnly = TRUE)
.arg <- function(name, default = NULL) if (length(i <- which(args == name)) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else default
lib <- file.path(dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]]),
                                               winslash = "/"))), "lib")
root <- dirname(dirname(lib))
options(prevcarga.lib = lib)
source(file.path(lib, "errors.R"))
source(file.path(lib, "runtime.R"))
invisible(prevcarga_runtime_enforce())
source(file.path(lib, "deck_reader.R"))
suppressPackageStartupMessages({ library(data.table); library(arrow); library(lubridate) })

source_dir <- .arg("--source")
dest       <- .arg("--dest")
areas_arg  <- .arg("--areas")
load_label <- .arg("--load-label", "end")

NONLEAF <- c("N", "NE", "S", "SECO")
TON_THR <- 19L
log_line <- function(...) cat(sprintf(...), "\n", sep = "")

## ---- leitura -------------------------------------------------------------------------------------------------
.read_csv <- function(area, name) {
  f <- file.path(source_dir, area, paste0(name, ".csv.gz"))
  if (!file.exists(f)) return(NULL)
  con <- gzfile(f, "rt", encoding = "UTF-8"); on.exit(close(con))
  fread(text = readLines(con), sep = ";", colClasses = "character", na.strings = c("", "NA"))
}
.num <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))
.ts  <- function(x) ymd_hms(x, tz = "UTC", truncated = 3L)   # digitos naive; o fuso real e declarado no manifest

## ---- desagregaModeloSH (horario -> 30 min) ---------------------------------------------------------------------
## Porte da desagregaModeloSH do PrevCargaDESSEM legado (prevcargadessem3/docs/legacy-workflow/PrevCargaDESSEM/R/
## pkgfun_funcoesApoio.R:388-410): no eixo de INDICE de meia hora, o valor horario y_k vira ancora em x1 = 2k-1 (1a meia
## hora) numa spline monoH.FC e em x2 = 2k (2a meia hora) numa segunda; a saida crua e a media das duas avaliadas em
## 1..2n (fora das ancoras a splinefun monoH.FC extrapola linearmente, como no legado).
.desagrega_vout <- function(y) {
  n <- length(y)
  if (n == 1L) return(rep(y, 2L))                         # monoH.FC exige >= 2 pontos: hora isolada = valor constante
  xout <- seq_len(2L * n)
  stats::splinefun(seq(1, 2 * n - 1, 2), y, "monoH.FC")(xout) / 2 + stats::splinefun(seq(2, 2 * n, 2), y, "monoH.FC")(xout) / 2
}
## Ajuste por hora ADITIVO (Gabriel 2026-11-11: Vout + Vin - Vm; o legado multiplica Vout*Vin/Vm, instavel com
## temperatura perto de 0 ou negativa): a media do par de cada hora fica igual ao valor horario de entrada.
.desagrega_modelo_sh <- function(y) {
  vout <- .desagrega_vout(y)
  vm <- (vout[c(TRUE, FALSE)] + vout[c(FALSE, TRUE)]) / 2
  vout + rep(y - vm, each = 2L)
}

## dt: data.table(g, t, v) horario (t POSIXct em hora cheia). A hora H e a media de [H, H+1) e vira o par (H:00, H:30)
## (Gabriel 2026-11-11), inclusive a ultima hora. A desagregacao roda por corrida contigua de horas validas de cada g
## (o eixo do legado e por indice: sem a segmentacao, horas dos dois lados de um buraco virariam vizinhas). Devolve a
## grade de 30 min de min(t) a max(t)+30 min por g (inclusive horas com v NA, como antes); meia hora dentro de buraco
## (entre duas corridas) sai da reta entre as meias horas das bordas; fora dos pontos validos, NA (sem extrapolar).
## attr(, "gap_fill") = numero de meias horas preenchidas pela reta.
.to_semihourly <- function(dt) {
  setorder(dt, g, t)
  span <- dt[, .(lo = min(t), hi = max(t)), by = g]
  ok <- unique(dt[!is.na(v)], by = c("g", "t"))
  ok[, run := cumsum(c(TRUE, diff(as.numeric(t)) != 3600)), by = g]
  sh <- ok[, .(t = rep(t, each = 2L) + c(0, 1800), v = .desagrega_modelo_sh(v)), by = .(g, run)][, run := NULL]
  grid <- span[, .(t = seq(lo, hi + 1800, by = "30 min")), by = g]
  out <- sh[grid, on = c("g", "t")]
  nfill <- 0L
  out[, v := {
    i <- is.na(v); x <- as.numeric(t)
    if (any(i) && sum(!i) >= 2L) {
      fill <- stats::approx(x[!i], v[!i], x[i])$y         # rule = 1: NA fora dos pontos validos
      nfill <<- nfill + sum(!is.na(fill)); v[i] <- fill
    }
    v
  }, by = g]
  setattr(out, "gap_fill", nfill)
  out[]
}

## ---- escrita ---------------------------------------------------------------------------------------------------
## Grava timestamp[ns] SEM fuso (contrato do store; o fuso vem do manifest). POSIXct em UTC => digitos preservados.
.write_part <- function(df, dir, ts_cols) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tb <- arrow::arrow_table(as.data.frame(df))
  fields <- lapply(names(tb), function(cn) {
    if (cn %in% ts_cols) arrow::field(cn, arrow::timestamp("ns")) else tb$schema[[cn]]
  })
  tb <- tb$cast(arrow::schema(fields))
  arrow::write_parquet(tb, file.path(dir, "data.parquet"))
}
.write_by_year <- function(df, base, area, year_of, ts_cols) {
  df[, year_ := year(year_of)]
  for (y in sort(unique(df$year_)))
    .write_part(df[year_ == y][, year_ := NULL], file.path(dest, base, paste0("area_code=", area), paste0("year=", y)), ts_cols)
  uniqueN(df$year_)
}

## ---- datasets --------------------------------------------------------------------------------------------------
conv_load <- function(area) {
  x <- .read_csv(area, "CARGASHHIST"); if (is.null(x)) prevcarga_abort("contract.dataset_missing", where = paste0(area, "/CARGASHHIST"))
  dt <- data.table(start = .ts(x$din_referencia), upd = x$din_atualizacao, load_mwh = .num(x$val_carga))
  setorder(dt, start, upd); dt <- unique(dt, by = "start", fromLast = TRUE)
  if (length(neg <- which(dt$load_mwh <= 0))) {                # carga <= 0 nao e carga: vira faltante (e o ponto medio a repoe)
    log_line("  %s: %d meia(s) hora(s) com carga <= 0 tratada(s) como faltante: %s", area, length(neg),
             paste(format(dt$start[neg], "%Y-%m-%d %H:%M"), collapse = ", "))
    dt$load_mwh[neg] <- NA_real_
  }
  full <- data.table(start = seq(min(dt$start), max(dt$start), by = "30 min"))
  dt <- dt[full, on = "start"]
  miss <- which(is.na(dt$load_mwh))
  iso <- miss[miss > 1L & miss < nrow(dt)]
  iso <- iso[!is.na(dt$load_mwh[iso - 1L]) & !is.na(dt$load_mwh[iso + 1L])]
  if (length(iso)) {
    dt$load_mwh[iso] <- (dt$load_mwh[iso - 1L] + dt$load_mwh[iso + 1L]) / 2
    log_line("  %s: %d meia(s) hora(s) isolada(s) preenchida(s) pelo ponto medio: %s", area, length(iso),
             paste(format(dt$start[iso], "%Y-%m-%d %H:%M"), collapse = ", "))
  }
  if (anyNA(dt$load_mwh)) log_line("  aviso %s: %d meia(s) hora(s) faltando (buraco > 1 slot), nao preenchida(s)", area, sum(is.na(dt$load_mwh)))
  dt <- dt[!is.na(load_mwh)]
  out <- data.table(timestamp = if (load_label == "end") dt$start + minutes(30L) else dt$start, area_code = area, load_mwh = dt$load_mwh)
  .write_by_year(out, "load", area, dt$start, "timestamp")
}

conv_observed <- function(area) {
  x <- .read_csv(area, "TEMPHIST"); if (is.null(x)) prevcarga_abort("contract.dataset_missing", where = paste0(area, "/TEMPHIST"))
  sh <- .to_semihourly(data.table(g = 1L, t = .ts(x$din_referencia), v = .num(x$val_tmp)))
  if (attr(sh, "gap_fill")) log_line("  %s observado: %d meia(s) hora(s) em buraco preenchida(s) pela reta", area, attr(sh, "gap_fill"))
  out <- data.table(timestamp = sh$t, area_code = area, temperature = sh$v, heat_index = NA_real_)
  .write_by_year(out, file.path("weather", "observed"), area, out$timestamp, "timestamp")
}

conv_forecast <- function(area) {
  x <- .read_csv(area, "TEMPPREVHIST"); if (is.null(x)) prevcarga_abort("contract.dataset_missing", where = paste0(area, "/TEMPPREVHIST"))
  issue <- ymd(x$dat_referencia, tz = "UTC")
  key <- data.table(issue = sort(unique(issue)))[, g := .I]
  dt <- data.table(issue = issue, t = .ts(x$din_referencia), v = .num(x$val_tmp))[key, on = "issue"]
  sh <- .to_semihourly(dt[, .(g, t, v)])
  if (attr(sh, "gap_fill")) log_line("  %s previsto: %d meia(s) hora(s) em buraco preenchida(s) pela reta", area, attr(sh, "gap_fill"))
  sh <- sh[key, on = "g", nomatch = NULL]
  out <- data.table(issue_time = sh$issue, valid_time = sh$t, area_code = area, temperature = sh$v, heat_index = NA_real_)
  setorder(out, issue_time, valid_time)
  .write_by_year(out, file.path("weather", "forecast"), area, out$valid_time, c("issue_time", "valid_time"))
}

read_holidays <- function(area) {
  x <- .read_csv(area, "FERIADOS"); if (is.null(x)) prevcarga_abort("contract.dataset_missing", where = paste0(area, "/FERIADOS"))
  dt <- data.table(date = ymd(x$dat_diaespecial), area_code = area,
                   holiday_type_id = bit64::as.integer64(x$id_tipodiaespecial),
                   prevcarga_code = bit64::as.integer64(x$cod_prevcarga),
                   simplified_code = bit64::as.integer64(x$cod_simplificado))
  unique(dt, by = "date")
}

## Reparo add-only de TON (== backfill_ton_holidays.py): datas marcadas por >= 19 folhas (ex-TON) e com triplo de codigos
## unico entram em TON se faltarem; os marcadores proprios de TON ficam.
repair_ton <- function(hol) {
  leaves <- setdiff(names(hol), c(NONLEAF, "TON"))
  if (!"TON" %in% names(hol) || length(leaves) < TON_THR) { log_line("  TON: consenso indisponivel, nao reparado"); return(hol) }
  all <- rbindlist(hol[leaves])
  cnt <- all[, .(n = uniqueN(area_code)), by = date][n >= TON_THR]
  canon <- unique(all[date %in% cnt$date, .(date, holiday_type_id, prevcarga_code, simplified_code)])
  if (anyDuplicated(canon$date)) prevcarga_abort("generic.unexpected", where = paste0("TON consenso ambiguo: ", paste(canon$date[duplicated(canon$date)], collapse = ",")))
  add <- canon[!date %in% hol$TON$date][, area_code := "TON"]
  log_line("  TON: %d datas nacionais; %d -> %d feriados (+%d add-only)", nrow(cnt), nrow(hol$TON), nrow(hol$TON) + nrow(add), nrow(add))
  hol$TON <- setorder(rbindlist(list(hol$TON, add), use.names = TRUE), date)
  hol
}

conv_dst <- function(areas) {
  for (a in areas) if (!is.null(x <- .read_csv(a, "HORAVERAO"))) {
    out <- data.table(start_date = dmy(x$Data.inicial, tz = "UTC"), end_date = dmy(x$Data.final, tz = "UTC"))
    .write_part(out, file.path(dest, "auxiliary", "dst_periods"), c("start_date", "end_date"))
    return(nrow(out))
  }
  0L
}

conv_tiers <- function(areas) {
  new <- Filter(Negate(is.null), lapply(stats::setNames(areas, areas), function(a) {
    x <- .read_csv(a, "PATAMARES"); if (!is.null(x) && "Patamar" %in% names(x)) x else NULL
  }))
  if (!length(new)) { log_line("  aviso: nenhum PATAMARES no formato novo; load_tiers nao gravado"); return(0L) }
  if (!all(vapply(new, identical, NA, new[[1L]]))) prevcarga_abort("generic.unexpected", where = "PATAMARES divergente entre areas")
  x <- new[[1L]]
  out <- data.table(tier = x$Patamar, interval = as.integer(x$Intervalo), weekday = as.integer(x$DiaSemana),
                    month = as.integer(x$Mes), hour_start = x$HoraIni, hour_end = x$HoraFim)
  .write_part(out, file.path(dest, "auxiliary", "load_tiers"), character())
  log_line("load_tiers: %d linhas (formato novo, de %s)", nrow(out), paste(names(new), collapse = ","))
  nrow(out)
}

write_manifest <- function() {
  tpl <- jsonlite::read_json(file.path(root, "config", "deck-manifest.store.json"))
  tpl$load_label <- load_label
  writeLines(jsonlite::toJSON(tpl, auto_unbox = TRUE, pretty = TRUE), file.path(dest, "manifest.json"))
  prevcarga_deck_manifest(dest)                          # aborta [contract] se o manifest gerado for invalido
}

run <- function() {
  if (is.null(source_dir) || !dir.exists(source_dir)) prevcarga_abort("config.usage", where = "--source")
  if (is.null(dest)) prevcarga_abort("config.usage", where = "--dest")
  if (!load_label %in% c("end", "start")) prevcarga_abort("config.usage", where = "--load-label")
  if (dir.exists(dest) && length(list.files(dest, all.files = TRUE, no.. = TRUE))) prevcarga_abort("config.usage", where = "--dest (nao vazio)")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  avail <- sort(list.dirs(source_dir, full.names = FALSE, recursive = FALSE), method = "radix")
  avail <- avail[file.exists(file.path(source_dir, avail, "CARGASHHIST.csv.gz"))]
  areas <- if (is.null(areas_arg)) avail else strsplit(areas_arg, ",", fixed = TRUE)[[1L]]
  if (length(bad <- setdiff(areas, avail))) prevcarga_abort("config.usage", where = paste0("--areas ", paste(bad, collapse = ",")))
  log_line("deck legado: %s -> %s (%d areas, load_label %s)", source_dir, dest, length(areas), load_label)
  hol <- list()
  for (a in areas) {
    t0 <- proc.time()[["elapsed"]]
    nl <- conv_load(a); no <- conv_observed(a); nf <- conv_forecast(a)
    hol[[a]] <- read_holidays(a)
    log_line("%-5s load %d, observado %d, previsto %d particoes (%.0fs)", a, nl, no, nf, proc.time()[["elapsed"]] - t0)
  }
  hol <- repair_ton(hol)
  for (a in names(hol)) .write_by_year(copy(hol[[a]]), file.path("auxiliary", "holidays"), a, hol[[a]]$date, character())
  log_line("dst_periods: %d periodos", conv_dst(areas))
  conv_tiers(areas)
  write_manifest()
  log_line("manifest.json gravado (load_label %s)", load_label)
  invisible(0L)
}

out <- prevcarga_with_errors(run())
if (!is.null(out$cond)) writeLines(prevcarga_render_error(out$cond, format = "text", verb = "convert_legacy_deck"), con = stderr())
quit(save = "no", status = out$exit_code)
