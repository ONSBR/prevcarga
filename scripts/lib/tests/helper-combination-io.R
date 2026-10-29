## helper-combination-io.R -- fixtures de DISCO da combina (M4.2). Nao edita o helper-combination.R do M4.1.
## Auto-carregado por test_dir; tudo em withr::local_tempdir(), caminhos por file.path. Nao e codigo de produto.
## `members` = data.frame(sigla, mdir, exogena) de prevcarga_members(), passado pelo teste (o helper nao
## carrega o modulo).

## LONG 8-col de um membro: y_pred por formula fechada; y_true embutido = truth + 500 (provar que e descartado)
pc_cmb_member_long <- function(sigla, area, origins, d_plus = 0L, slots = 0:47, members, cut = "0900") {
  idx <- match(sigla, members$sigla)
  g <- expand.grid(oi = seq_along(origins), d = as.integer(d_plus), slot = as.integer(slots),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  o0 <- lubridate::ymd(origins[g$oi], tz = pc_cmb_tz)
  istart <- o0 + lubridate::days(g$d) + lubridate::minutes(30L * g$slot)
  truth <- pc_cmb_truth(istart)
  data.frame(area = area, forecast_origin = origins[g$oi],
             target_datetime = istart + lubridate::minutes(30),
             lead = g$d * 48L + g$slot, y_pred = truth * (1 + 0.01 * idx), y_true = truth + 500,
             d_plus = g$d, granularity_min = 30L, stringsAsFactors = FALSE)
}

## run-manifest no formato do M1.3 (available_until em UTC ISO-8601; origin ISO; delivery_days inteiro)
pc_cmb_write_run_manifest <- function(dir, origin, available_until, load_label = "end", load_tz = pc_cmb_tz,
                                      weather_issue_cutoff = "available_until", delivery_days = 9L) {
  m <- list(origin = origin,
            available_until = if (is.null(available_until)) NULL else
              lubridate::format_ISO8601(lubridate::with_tz(available_until, "UTC"), usetz = TRUE),
            load_label = load_label, load_tz = load_tz, weather_issue_cutoff = weather_issue_cutoff,
            delivery_days = as.integer(delivery_days))
  jsonlite::write_json(m, file.path(dir, "run_manifest.json"), auto_unbox = TRUE, null = "null", digits = NA)
  invisible(dir)
}

## spec: lista de runs list(sigla, mode = "infer", cut = "0900", origins, d_plus = 0L, slots = 0:47,
##   long = NULL (substitui o LONG), manifest = TRUE, delivery_days = 9L). Grava
## <dir>/<mdir>/<area>/<from>_<to>_<mode>[_<EXO>]_c<HHMM>/{long.parquet, run_manifest.json}
pc_cmb_write_root <- function(dir, area, spec, members) {
  for (r in spec) {
    i <- match(r$sigla, members$sigla)
    mode <- if (is.null(r$mode)) "infer" else r$mode
    cut <- if (is.null(r$cut)) "0900" else r$cut
    org <- sort(r$origins)
    exo <- if (is.na(members$exogena[[i]])) "" else paste0("_", members$exogena[[i]])
    id <- sprintf("%s_%s_%s%s%s", org[[1L]], org[[length(org)]], mode, exo,
                  if (nzchar(cut)) paste0("_c", cut) else "")
    run <- file.path(dir, members$mdir[[i]], area, id)
    dir.create(run, recursive = TRUE, showWarnings = FALSE)
    long <- if (is.null(r$long)) pc_cmb_member_long(r$sigla, area, org,
              if (is.null(r$d_plus)) 0L else r$d_plus, if (is.null(r$slots)) 0:47 else r$slots, members) else r$long
    arrow::write_parquet(long, file.path(run, "long.parquet"))
    if (!identical(r$manifest, FALSE)) {
      au <- if (nzchar(cut)) lubridate::ymd_hm(sprintf("%s %s:%s", org[[1L]], substr(cut, 1, 2), substr(cut, 3, 4)),
                                                tz = pc_cmb_tz) else NULL
      pc_cmb_write_run_manifest(run, org[[1L]], au, delivery_days =
        if (is.null(r$delivery_days)) 9L else r$delivery_days)
    }
  }
  invisible(dir)
}

.pc_cmb_origins <- c("2026-09-30", "2026-10-01")

pc_cmb_root_infer8 <- function(members, area = "SECO", mode = "infer", origins = .pc_cmb_origins,
                               dir = withr::local_tempdir(.local_envir = parent.frame())) {
  spec <- lapply(members$sigla, function(s) list(sigla = s, mode = mode, origins = origins))
  pc_cmb_write_root(dir, area, spec, members)
}

## ..._daily_MED E ..._infer_MED de LIN_MED na mesma area
pc_cmb_root_dup_mode <- function(members, area = "SECO", origins = .pc_cmb_origins,
                                 dir = withr::local_tempdir(.local_envir = parent.frame())) {
  pc_cmb_write_root(dir, area, list(list(sigla = "LIN_MED", mode = "daily", origins = origins),
                                    list(sigla = "LIN_MED", mode = "infer", origins = origins)), members)
}

## runs _infer_MED, _infer_MAX, _infer_MXMN de dessem-linear (variantes disjuntas)
pc_cmb_root_lin_variants <- function(members, area = "SECO", origins = .pc_cmb_origins,
                                     dir = withr::local_tempdir(.local_envir = parent.frame())) {
  pc_cmb_write_root(dir, area, lapply(c("LIN_MED", "LIN_MAX", "LIN_MXMN"), function(s)
    list(sigla = s, origins = origins, slots = if (s == "LIN_MED") 0:47 else if (s == "LIN_MAX") 0:23 else 0:11)),
    members)
}

pc_cmb_root_7of8 <- function(members, area = "SECO", absent = "HW", origins = .pc_cmb_origins,
                             dir = withr::local_tempdir(.local_envir = parent.frame())) {
  pc_cmb_write_root(dir, area, lapply(setdiff(members$sigla, absent), function(s)
    list(sigla = s, origins = origins)), members)
}

## primeiro membro em c0900 e c1700; segundo so em c0900
pc_cmb_root_two_cuts <- function(members, area = "SECO", origins = .pc_cmb_origins,
                                 dir = withr::local_tempdir(.local_envir = parent.frame())) {
  s <- members$sigla
  pc_cmb_write_root(dir, area, list(
    list(sigla = s[[1L]], cut = "0900", origins = origins), list(sigla = s[[1L]], cut = "1700", origins = origins),
    list(sigla = s[[2L]], cut = "0900", origins = origins)), members)
}
