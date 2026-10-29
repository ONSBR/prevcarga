## helper-combination-align.R -- fixtures do alinhamento da combina (M4.2b). Auto-carregado por test_dir; nao e
## codigo de produto. Reusa pc_cmb_write_root/pc_cmb_write_run_manifest (helper-combination-io.R). Tudo em tempdir.
## `members` = data.frame(sigla, mdir, exogena) de prevcarga_members(), passado pelo teste.

## sentinela do deck (M2): carga de um intervalo = slot do interval_start (1..48) + 1000 * indice da area (SECO = 1)
pc_cmb_sentinel <- function(target_datetime, area_idx = 1L) {
  istart <- target_datetime - lubridate::minutes(30)
  lubridate::hour(istart) * 2 + lubridate::minute(istart) / 30 + 1 + 1000 * area_idx
}

## LONG alinhado de um membro: alvos de (O + corte + 30 min) ate O + end_days 00:00, grade de 30 min.
##   first_skip: alvos descartados do inicio; shift: minutos somados a TODOS os alvos (d_plus/lead refeitos).
pc_cmb_aligned_long <- function(sigla, area, origin, cut, end_days, members, value = c("truth", "sentinel"),
                                first_skip = 0L, shift = 0L) {
  value <- match.arg(value)
  idx <- match(sigla, members$sigla)
  o0 <- lubridate::ymd(origin, tz = pc_cmb_tz)
  s0 <- 2L * as.integer(substr(cut, 1L, 2L)) + as.integer(substr(cut, 3L, 4L)) %/% 30L   # slot do corte
  s1 <- 48L * end_days - 1L                                                              # ultimo interval_start
  slots <- seq(s0, s1)
  if (first_skip > 0L) slots <- slots[-seq_len(first_skip)]
  istart <- o0 + lubridate::minutes(30L * slots + shift)
  target <- istart + lubridate::minutes(30)
  d_plus <- as.integer(lubridate::as_date(istart, tz = pc_cmb_tz) - lubridate::as_date(o0))
  slot_of_day <- lubridate::hour(istart) * 2L + lubridate::minute(istart) %/% 30L
  y <- if (value == "sentinel") pc_cmb_sentinel(target) else pc_cmb_truth(istart) * (1 + 0.01 * idx)
  data.frame(area = area, forecast_origin = origin, target_datetime = target,
             lead = as.integer(d_plus * 48L + slot_of_day), y_pred = y, y_true = y + 500,
             d_plus = d_plus, granularity_min = 30L, stringsAsFactors = FALSE)
}

## 8 membros alinhados: o `short_member` com delivery_days = short_days (cauda ate O+1), os demais long_days
pc_cmb_root_aligned8 <- function(members, dir = withr::local_tempdir(.local_envir = parent.frame()),
                                 origin = "2024-03-01", cut = "0900", short_member = members$sigla[[1L]],
                                 short_days = 2L, long_days = 9L, area = "SECO",
                                 value = c("truth", "sentinel"), only = members$sigla) {
  value <- match.arg(value)
  spec <- lapply(only, function(s) {
    dd <- if (identical(s, short_member)) short_days else long_days
    list(sigla = s, mode = "infer", cut = cut, origins = origin, delivery_days = dd,
         long = pc_cmb_aligned_long(s, area, origin, cut, dd, members, value))
  })
  pc_cmb_write_root(dir, area, spec, members)
  for (s in only)    # o writer do M4.2 grava load_label = "end"; o alinhado e do store "start"
    pc_cmb_edit_manifest(pc_cmb_run_dir(dir, s, members, area), function(m) { m$load_label <- "start"; m })
  dir
}

## diretorio do run _infer do membro (mesma regra de localizacao do discover, so para o teste)
pc_cmb_run_dir <- function(root, member, members, area = "SECO") {
  i <- match(member, members$sigla)
  exo <- if (is.na(members$exogena[[i]])) "" else paste0("_", members$exogena[[i]])
  base <- file.path(root, members$mdir[[i]], area)
  ids <- list.files(base)
  file.path(base, ids[grepl(sprintf("_infer%s_c[0-9]{4}$", exo), ids)][[1L]])
}

pc_cmb_edit_manifest <- function(run, fn) {
  mf <- file.path(run, "run_manifest.json")
  m <- jsonlite::read_json(mf, simplifyVector = TRUE)
  jsonlite::write_json(fn(m), mf, auto_unbox = TRUE, null = "null", digits = NA)
  invisible(run)
}

pc_cmb_misalign_kinds <- c("load_label", "load_tz", "weather_issue_cutoff", "tzone", "grade", "deslocamento",
                           "primeira_entrega", "delivery_days_curto", "delivery_days_longo", "available_until",
                           "sem_manifest")

## aplica UMA mutacao ao run de `member`. `tzone` regrava o LONG em outro fuso (o leitor unico o recusa antes,
## contract.long_tz; a checagem de tzone da combina e exercida em memoria pelo teste).
pc_cmb_misalign <- function(root, member, kind, members, area = "SECO", origin = "2024-03-01", cut = "0900") {
  kind <- match.arg(kind, pc_cmb_misalign_kinds)
  run <- pc_cmb_run_dir(root, member, members, area)
  mf <- jsonlite::read_json(file.path(run, "run_manifest.json"), simplifyVector = TRUE)
  dd <- mf$delivery_days
  put <- function(l) arrow::write_parquet(l, file.path(run, "long.parquet"))
  rebuild <- function(...) put(pc_cmb_aligned_long(member, area, origin, cut, members = members, ...))
  switch(kind,
    load_label = pc_cmb_edit_manifest(run, function(m) { m$load_label <- "end"; m }),
    load_tz = pc_cmb_edit_manifest(run, function(m) { m$load_tz <- "America/Sao_Paulo"; m }),
    weather_issue_cutoff = pc_cmb_edit_manifest(run, function(m) { m$weather_issue_cutoff <- "origin_midnight_utc"; m }),
    tzone = {
      l <- pc_cmb_aligned_long(member, area, origin, cut, dd, members)
      l$target_datetime <- lubridate::with_tz(l$target_datetime, "UTC")
      put(l)
    },
    grade = {
      l <- pc_cmb_aligned_long(member, area, origin, cut, dd, members)
      k <- nrow(l) %/% 2L
      l$target_datetime[k] <- l$target_datetime[k] + lubridate::minutes(15)    # 09:45 no padrao do dia
      put(l)
    },
    deslocamento = rebuild(end_days = dd, shift = 30L),
    primeira_entrega = rebuild(end_days = dd, first_skip = 3L),               # comeca 10:30, grade nao deslocada
    delivery_days_curto = rebuild(end_days = dd - 1L),
    delivery_days_longo = rebuild(end_days = dd + 1L),
    available_until = file.rename(run, sub("_c[0-9]{4}$", "_c1700", run)),
    sem_manifest = unlink(file.path(run, "run_manifest.json")))
  invisible(root)
}

## ctx de teste: deck sentinela de rotulo `load_label` cobrindo O..O+10 (para o join fresco) + convencoes
pc_cmb_ctx <- function(dir, load_label = "start", weather_issue_cutoff = "available_until", area = "SECO",
                       days = lubridate::ymd("2024-02-29") + 0:11) {
  deck <- pc_deck_sentinel(load_label, days = days, areas = area, dir = dir)
  c(pc_ctx_deck(deck), list(tz = pc_cmb_tz, root = pc_root(), load_label = load_label,
                            weather_issue_cutoff = weather_issue_cutoff))
}
