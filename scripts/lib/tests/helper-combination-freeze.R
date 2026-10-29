## helper-combination-freeze.R -- fixtures do freeze/apply da combina (M4.3). Auto-carregado por test_dir; nao e codigo de
## produto. Reusa pc_cmb_panel8_lead/pc_cmb_lead_origins (helper-combination-lead.R). Sem deck, sem env, sem RNG.
## `env` = ambiente com os modulos carregados (pc_source_lib), passado pelo teste.

pc_cmb_ctx_freeze <- function() list(tz = pc_cmb_tz, root = pc_root())

## completa a proveniencia sintetica com as convencoes de carga que o freeze exige (como o discover entrega)
pc_cmb_prov_conv <- function(panel, label = "start") {
  prov <- data.table::copy(data.table::as.data.table(attr(panel, "provenance")))
  data.table::set(prov, j = "load_label", value = label)
  data.table::set(prov, j = "load_tz", value = pc_cmb_tz)
  data.table::set(prov, j = "weather_issue_cutoff", value = "available_until")
  data.table::setattr(panel, "provenance", prov)
  panel
}

## painel de 3 meses (origens 2024-01-01..2024-03-10), corte 0900, 8 membros (o 1o curto ate O+1), y_true por formula
pc_cmb_panel_joined <- function(env, members, load_label = "start", origins = pc_cmb_lead_origins(), cut = "0900") {
  long <- pc_cmb_long8_lead(members, origins = origins, cut = cut)
  panel <- pc_cmb_panel_from_long(env, long)
  pc_cmb_prov_conv(panel, load_label)
}

## painel com 2 cortes (0900 e 1700): empilha os paineis por corte e une coverage/proveniencia
pc_cmb_panel_two_cuts <- function(env, members, origins = pc_cmb_lead_origins("2024-01-01", "2024-02-20")) {
  ps <- lapply(c("0900", "1700"), function(ct) pc_cmb_panel_joined(env, members, origins = origins, cut = ct))
  out <- data.table::rbindlist(ps, use.names = TRUE)
  data.table::setorderv(out, c("area", "cut", "lead", "forecast_origin"))
  data.table::setattr(out, "members_declared", attr(ps[[1L]], "members_declared"))
  data.table::setattr(out, "members_present", attr(ps[[1L]], "members_present"))
  data.table::setattr(out, "provenance", data.table::rbindlist(lapply(ps, attr, "provenance")))
  data.table::setattr(out, "coverage", unique(data.table::rbindlist(lapply(ps, attr, "coverage"))))
  out[]
}

## tabela de membros reduzida (sem o ultimo registro), por argumento
pc_cmb_members_without_last <- function(members) members[-nrow(members), , drop = FALSE]

## treina em tempdir sob <tmp>/state/artifacts/combina; devolve list(dir, res)
pc_cmb_trained <- function(env, panel, members, area = "SECO", trained_at = "2024-01-01T00:00:00Z",
                           dir = NULL, ...) {
  if (is.null(dir)) {
    dir <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "state", "artifacts", "combina")
    dir.create(dir, recursive = TRUE)
  }
  res <- env$prevcarga_combina_train(area, panel, dir, root = pc_root(), members = members,
                                     trained_at = trained_at, ...)
  list(dir = dir, res = res)
}

## copia v_<hash>/ + latest.txt para <tmp>/bucket/artifacts/combina/<area>/ (imita o prevcarga-sync); devolve o artifact_dir
pc_cmb_copy_as_bucket <- function(dir, area) {
  dst <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "bucket", "artifacts", "combina", area)
  dir.create(dst, recursive = TRUE)
  vh <- readLines(file.path(dir, area, "latest.txt"), n = 1L)
  file.copy(file.path(dir, area, vh), dst, recursive = TRUE)
  file.copy(file.path(dir, area, "latest.txt"), dst)
  dirname(dst)
}

## inverte 1 byte no meio do arquivo
pc_cmb_corrupt_byte <- function(path) {
  b <- readBin(path, "raw", file.size(path))
  i <- length(b) %/% 2L
  b[[i]] <- as.raw(bitwXor(as.integer(b[[i]]), 255L))
  writeBin(b, path)
  invisible(path)
}
