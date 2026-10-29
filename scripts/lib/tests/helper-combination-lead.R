## helper-combination-lead.R -- fixtures da combina por lead (M4.2c). Auto-carregado por test_dir; nao e codigo de
## produto. Reusa pc_cmb_truth/pc_cmb_tz (helper-combination.R). Sem RNG, formula fechada, escala O(1).
## `members` = data.frame(sigla, mdir, exogena) de prevcarga_members(), passado pelo teste.

pc_cmb_lead_origins <- function(from = "2024-01-01", to = "2024-03-10") format(seq(as.Date(from), as.Date(to), by = 1))

## dia relativo a O do lead (0 = dia da origem): separa O..O+1 de O+2..O+8 no teste sem literal de faixa
pc_cmb_lead_day <- function(lead) as.integer(lead) %/% 48L

pc_cmb_lead_bias <- c(0.02, -0.01, 0.03, 0.015, -0.02, 0.01, -0.005, 0.025)

## linhas LONG do membro j (indice na lista de membros) para os leads [from_lead, to_lead] de cada origem
pc_cmb_lead_rows <- function(j, sigla, origins, from_lead, to_lead) {
  g <- expand.grid(oi = seq_along(origins), lead = seq.int(from_lead, to_lead), KEEP.OUT.ATTRS = FALSE)
  o0 <- lubridate::ymd(origins[g$oi], tz = pc_cmb_tz)
  istart <- o0 + lubridate::minutes(30L * g$lead)
  k <- g$oi * 7 + g$lead
  y <- pc_cmb_truth(istart) + pc_cmb_lead_bias[[j]] + 0.02 * j * sin(2 * pi * k / (5 + j) + j)
  pc_long_mem(data.frame(member = sigla, run_id = "run1", area = "SECO", forecast_origin = origins[g$oi],
             target_datetime = istart + lubridate::minutes(30), lead = as.integer(g$lead), y_pred = y,
             d_plus = as.integer(g$lead %/% 48L), granularity_min = 30L, stringsAsFactors = FALSE))
}

## LONG de 8 membros (corte `cut`, 1o alvo = corte + 30 min). O `short_member` declara short_days (O..O+1), os
## demais long_days; as linhas do LONG vao ate a faixa declarada (a cauda do curto fica ausente).
pc_cmb_long8_lead <- function(members, short_member = members$sigla[[1L]], short_days = 2L, long_days = 9L,
                              origins = pc_cmb_lead_origins(), cut = "0900") {
  s0 <- 2L * as.integer(substr(cut, 1L, 2L)) + as.integer(substr(cut, 3L, 4L)) %/% 30L
  days <- ifelse(members$sigla == short_member, short_days, long_days)
  rows <- lapply(seq_len(nrow(members)), function(j)
    pc_cmb_lead_rows(j, members$sigla[[j]], origins, s0, 48L * days[[j]] - 1L))
  long <- data.table::as.data.table(do.call(rbind, rows))
  data.table::setattr(long, "members_declared", members$sigla)
  data.table::setattr(long, "members_present", members$sigla)
  data.table::setattr(long, "provenance",
    data.table::data.table(member = members$sigla, run_id = "run1", cut = cut, delivery_days = as.integer(days)))
  long
}

## preenche a cauda FORA da faixa declarada do membro (ate to_days), sem mexer na proveniencia
pc_cmb_fill_tail <- function(long, member, members, to_days = 9L, origins = pc_cmb_lead_origins()) {
  prov <- attr(long, "provenance")
  dd <- prov$delivery_days[prov$member == member]
  j <- match(member, members$sigla)
  tail <- data.table::as.data.table(pc_cmb_lead_rows(j, member, origins, 48L * dd, 48L * to_days - 1L))
  out <- data.table::rbindlist(list(long, tail), use.names = TRUE)
  for (n in c("members_declared", "members_present", "provenance")) data.table::setattr(out, n, attr(long, n))
  out
}

## troca o delivery_days declarado do membro (so a proveniencia; as linhas do LONG ficam como estao)
pc_cmb_set_days <- function(provenance, mem, days) {
  provenance <- data.table::copy(provenance)
  rows <- which(provenance$member == mem)
  data.table::set(provenance, i = rows, j = "delivery_days", value = as.integer(days))
  provenance[]
}

pc_cmb_with_prov <- function(long, prov) {
  out <- data.table::copy(long)
  for (n in c("members_declared", "members_present")) data.table::setattr(out, n, attr(long, n))
  data.table::setattr(out, "provenance", prov)
  out
}

## LONG -> painel (build_panel direto) com y_true por formula fechada (verdade do interval_start + ruido)
pc_cmb_panel_from_long <- function(env, long) {
  panel <- env$prevcarga_build_panel(long)
  k <- match(panel$forecast_origin, sort(unique(panel$forecast_origin))) * 7 + panel$lead
  istart <- panel$target_datetime - lubridate::minutes(30)
  panel[, y_true := pc_cmb_truth(istart) + 0.01 * sin(2 * pi * k / 11)]
  panel[]
}

pc_cmb_panel8_lead <- function(env, members, ...) pc_cmb_panel_from_long(env, pc_cmb_long8_lead(members, ...))

## fit + produce (o caminho que o M4.3 compoe): devolve list(weights, y_pred)
pc_cmb_run_combina <- function(env, panel, n_min = 24L) {
  w <- suppressWarnings(env$prevcarga_combina_fit(panel, n_min = n_min))
  list(weights = w, y_pred = env$prevcarga_produce_combined(panel, w))
}
