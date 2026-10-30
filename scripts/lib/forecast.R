## forecast.R -- rodada operacional diaria: validate-deck -> plan -> infer -> build_long -> combina -> reconcile -> export (M6.4).
##
## O M6 SO COMPOE: cada passo e uma funcao dos outros epicos (M2 validacao/leitor/datas, M5 fachadas de treino e inferencia,
## M6.1 build_long, M4 combina, Reconcile, M6.2/M6.7 export e chave imutavel). Aqui nao ha calculo de k-hat/BU/SIN, de pesos,
## de corte de treino nem aritmetica de hora; a rodada diaria e UMA origem O.
## Ordem invariante: gate-0 (exit 3, antes de qualquer infer/scratch) -> plano de topologia (exit 4) -> infer por membro x folha
## (origin_agnostic so le artefato; origin_scoped treina so se nao ha artefato da origem O) -> LONG por membro -> combina (pesos
## congelados) -> reconcile UMA vez (em memoria; erros 13/15 sobem intactos, antes de criar scratch ou diretorio de entrega) ->
## versao/chave imutavel -> scratch + run.json -> export. Rotulo de hora so nas bordas (interval_start por dentro, interval_end
## na saida do LONG). Sem relogio (`generated_at` vem do ctx), sem variaveis de ambiente. Saida ASCII.
## Requer errors.R, config.R, topology.R, provenance.R, schema_long.R, evaluation.R, deck_reader.R, dates.R, validation.R,
## features.R, registry.R, training.R, inference.R, combination.R, build_long_output.R, reconcile.R e exports.R carregados antes.

## --- helpers de ambiente da rodada -------------------------------------------------------------------------
.forecast_topo <- function(ctx) {
  if (!is.null(ctx$topology_path)) prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  else prevcarga_topology(root = ctx$root)
}

.forecast_member_table <- function(ctx) {
  if (!is.null(ctx$topology_path)) prevcarga_members(path = ctx$topology_path, root = ctx$root)
  else prevcarga_members(root = ctx$root)
}

## Coletor dos avisos do catalogo: o aviso e capturado (nao sinalizado) e sai em `warnings` do retorno.
.forecast_warn_sink <- function() {
  acc <- list()
  list(
    add = function(id, message = NULL) {
      if (is.null(message)) message <- prevcarga_condition(id, type = "prevcarga_warning")$message
      acc[[length(acc) + 1L]] <<- list(id = id, message = message)
      invisible(NULL)
    },
    get = function() acc)
}

.forecast_iso_min <- function(x, tz) format(lubridate::with_tz(x, tz), "%Y-%m-%dT%H:%M")

## --- 1. gate-0 ---------------------------------------------------------------------------------------------
## Origem: o argumento, ou a do ctx (flag), ou o dia do ultimo intervalo de carga do deck (dates.R; nenhum relogio). O veredito
## reprovado vira o abort do primeiro erro da classe dominante (a mesma regra do verbo validate-deck; exit 3 no buraco do dia O).
## `skip_validate` pula so a validacao: os cortes (available_until, regra de clima, fuso) vem SEMPRE do leitor.
.forecast_gate0 <- function(ctx, origin, skip_validate, mem, sink) {
  origin <- if (is.null(origin)) prevcarga_resolve_origin(ctx)$origin else lubridate::as_date(origin)
  lags <- prevcarga_plugin_data_lags(unique(mem$mdir), ctx$root)
  verdict <- NULL
  if (!isTRUE(skip_validate)) {
    verdict <- prevcarga_validate_deck(ctx, origin = origin, scope = NULL, data_lag_days = lags)
    f <- verdict$findings
    if (!isTRUE(verdict$ok)) {
      errs <- f[f$level == "error" & f$class == verdict$class, ]
      first <- errs[1L, ]
      prevcarga_abort(paste0(first$class, ".", first$what),
                      where = if (is.na(first$where) || !nzchar(first$where)) NULL else first$where,
                      details = list(findings = nrow(errs), items = lapply(seq_len(nrow(f)), function(i) as.list(f[i, ])),
                                     origin = format(origin)))
    }
    for (w in unique(f$what[f$level == "warning"])) sink$add(paste0("warning.", w))
  }
  cuts <- prevcarga_resolve_cuts(ctx, origin = origin, data_lag_days = lags)
  ## Cortes entregaveis: O 00:00 (rodada das 00h, sem nenhuma informacao do dia O) e qualquer corte intradiario de O. Dia O completo
  ## (available_until >= O+1 00:00) nao tem representacao no corte da combina (HHMM dentro de O): recusa antes de qualquer infer.
  next_day <- lubridate::force_tz(lubridate::as_datetime(origin + 1L), cuts$tz)
  if (cuts$available_until >= next_day)
    prevcarga_abort("completude.forecast_origin_day_complete", where = format(origin),
                    details = list(available_until = .forecast_iso_min(cuts$available_until, cuts$tz)))
  list(origin = origin, cuts = cuts, verdict = verdict)
}

## --- 2. plano de topologia (reach-control) -------------------------------------------------------------------
## Folhas pedidas (NULL = todas as da topologia; fora dela => topology.area_undeclared, exit 4). O fechamento (closed/stray/
## sin_closed) e do `prevcarga_reconcile_plan`; `covered` = folhas pedidas que fecham algum subsistema. Nada literal aqui.
.forecast_plan <- function(ctx, areas) {
  topo <- .forecast_topo(ctx)
  want <- if (is.null(areas)) topo$leaves else unique(as.character(areas))
  if (!length(want)) prevcarga_abort("config.usage", where = "areas")
  bad <- setdiff(want, topo$leaves)
  if (length(bad)) prevcarga_abort("topology.area_undeclared", where = bad[[1L]])
  leaves <- topo$leaves[topo$leaves %in% want]
  pl <- prevcarga_reconcile_plan(leaves, topo)
  list(leaves = leaves, covered = setdiff(leaves, pl$stray), stray = pl$stray, closed = pl$closed,
       sin_closed = isTRUE(pl$sin_closed), topology = topo)
}

## --- 3. membros ------------------------------------------------------------------------------------------------
## Versao do artefato da origem O de um membro origin_scoped (proprio ou oficial sincronizado: mesmo layout do registry), pelo
## `origin` do run_manifest de cada versao; o `latest` se for dele, senao a ultima em ordem radix. NULL = nao ha artefato de O.
.forecast_scoped_version <- function(ctx, member, area, origin) {
  key <- prevcarga_registry_key(member, area, ctx)
  if (!dir.exists(key)) return(NULL)
  vs <- list.files(key, pattern = "^v_[0-9a-f]{16}$")
  vs <- vs[order(vs, method = "radix")]
  of <- function(v) tryCatch(lubridate::as_date(prevcarga_read_run_manifest(file.path(key, v))$origin),
                             error = function(e) lubridate::NA_Date_)
  hit <- vs[vapply(vs, function(v) isTRUE(of(v) == origin), NA)]
  if (!length(hit)) return(NULL)
  latest <- tryCatch(prevcarga_resolve_version(key, "latest"), prevcarga_error = function(e) NA_character_)
  if (latest %in% hit) latest else hit[[length(hit)]]
}

## Por folha e membro: a entrega do infer (semi-horaria, `interval_end`) e a versao do artefato usada. origin_agnostic so le
## o artefato (ausente => state.artifact_missing, exit 13: nunca treina); origin_scoped treina UMA vez, pela fachada de treino
## (que deriva os cortes de dates.R), so se nao ha artefato da origem O. Nenhuma conta de corte aqui.
.forecast_members <- function(ctx, plan, g, mem) {
  out <- list()
  for (i in seq_len(nrow(mem))) {
    m <- mem[i, , drop = FALSE]
    sigla <- as.character(m$sigla)
    split <- prevcarga_model_spec(m$mdir, ctx$root)$split
    dd <- as.integer(prevcarga_plugin_descriptor(m$mdir, prevcarga_models_root(ctx$root))$delivery_days)
    for (a in c(plan$covered, plan$stray)) {
      ver <- NULL
      if (identical(split, "origin_scoped")) {
        ver <- .forecast_scoped_version(ctx, m, a, g$origin)
        if (is.null(ver)) ver <- as.character(prevcarga_train(m, a, g$origin, ctx)$version)
      }
      res <- prevcarga_infer_cell(m, a, g$origin, ctx, version = ver)
      out[[a]][[sigla]] <- list(
        delivery = data.table::data.table(interval_end = res$predictions$target_datetime, y_pred = res$predictions$y_pred),
        member = list(name = sigla, delivery_days = dd, split = split),
        version = as.character(res$version))
    }
  }
  out
}

## --- 4. LONG por membro e combina -----------------------------------------------------------------------------
## Painel da folha: LONG de cada membro (M6.1) -> proveniencia em memoria (a mesma forma que o run-manifest da combina) ->
## recusa de desalinhado -> painel por corte. `ctx` aqui ja carrega tz e load_label da rodada.
.forecast_leaf_panel <- function(ctx, leaf_members, leaf, g, mem) {
  au <- g$cuts$available_until
  tz <- g$cuts$tz
  wic <- if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff
  cut <- sprintf("%02d%02d", lubridate::hour(au), lubridate::minute(au))
  parts <- list(); prov <- list()
  for (sg in mem$sigla) {
    mb <- leaf_members[[sg]]
    d <- prevcarga_build_long(mb$delivery, mb$member, leaf, g$origin, au, truth = NULL, ctx = ctx)
    d <- data.table::as.data.table(prevcarga_assert_long(d, ctx))        # leitor unico do LONG: anexa o interval_start
    d[, `:=`(member = sg, run_id = "forecast")]
    parts[[length(parts) + 1L]] <- d
    prov[[length(prov) + 1L]] <- data.table::data.table(
      member = sg, run_id = "forecast", run_mode = "infer", cut_suffix = "", origin = format(g$origin), available_until = au,
      cut = cut, load_label = g$cuts$load_label, load_tz = tz, weather_issue_cutoff = wic, delivery_days = mb$member$delivery_days)
  }
  long <- data.table::rbindlist(parts)
  data.table::setattr(long, "members_declared", mem$sigla)
  data.table::setattr(long, "members_present", mem$sigla)
  data.table::setattr(long, "provenance", data.table::rbindlist(prov))
  prevcarga_assert_members_aligned(long, ctx)
  list(panel = prevcarga_build_panel(long, mem), members_long = parts)
}

## Combina por folha com os pesos CONGELADOS (nunca re-fita); por lead so combinam os membros que cobrem o lead.
## Devolve LONG combinado por folha, LONG por membro x folha e a versao dos pesos por folha.
.forecast_leaves <- function(ctx, members, g, plan, mem) {
  art_dir <- file.path(prevcarga_registry_root(ctx), "combina")
  long <- list(); mlong <- list(); wver <- list()
  for (a in c(plan$covered, plan$stray)) {
    lp <- .forecast_leaf_panel(ctx, members[[a]], a, g, mem)
    ap <- prevcarga_combina_apply(a, art_dir, lp$panel, ctx, members = mem, detail = TRUE)
    long[[a]] <- ap$long
    wver[[a]] <- ap$version_hash
    for (d in lp$members_long) {
      sg <- d$member[[1L]]
      mlong[[sg]][[a]] <- d[, names(prevcarga_schema_long), with = FALSE]
    }
  }
  list(long = long, members = mlong, weights_version = wver)
}

## --- 5. reconcile ----------------------------------------------------------------------------------------------
## UMA chamada, em memoria, antes de qualquer scratch ou diretorio de entrega; k-hat `latest` da origem (khat_origin == O sempre).
## Folhas soltas nunca passam pelo reconcile. Erros (honesty.khat_after_origin, state.khat_missing, ...) sobem intactos.
.forecast_reconcile <- function(long_leaves, plan, ctx) {
  if (!length(plan$closed))
    return(list(long = list(), khat = list(), cobertura_descartada = list(), warnings = list()))
  rec <- prevcarga_reconcile_apply(long_leaves[plan$covered], ctx, subsystems = plan$closed, khat_version = NULL, eval = FALSE)
  list(long = rec$long, khat = rec$khat, cobertura_descartada = rec$cobertura_descartada, warnings = rec$warnings)
}

## --- 6. versao, scratch e run.json ---------------------------------------------------------------------------
.forecast_versao_parts <- function(ctx, members, lv, rec, g, plan) {
  topo <- plan$topology
  khat <- lapply(rec$khat, function(k) k$versao)
  ## membro x folha: versao do artefato + hash do que o infer entregou (o mesmo artefato com deck/clima novo muda a entrega)
  mver <- list()
  for (a in names(members)) for (sg in names(members[[a]])) {
    mb <- members[[a]][[sg]]
    mver[[sg]][[a]] <- paste0(mb$version, ":", .prevcarga_sha256(paste(sprintf("%.17g", mb$delivery$y_pred), collapse = ",")))
  }
  list(package = if (is.null(ctx$version)) "unknown" else ctx$version,
       members = mver, combina_weights = lv$weights_version, khat = khat,
       topology_hash = .prevcarga_sha256(.prevcarga_canon_json(topo[c("subsystems", "order", "khat_range_default")])),
       load_label = g$cuts$load_label,
       weather_issue_cutoff = if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff)
}

## Scratch `<state_dir>/scratch/<run_id>/`, recriado SEMPRE (run_id = hash da chave). Subsistemas/SIN pelo escritor do reconcile;
## folhas e membros pelo mesmo escritor deterministico do LONG. Devolve arquivos[] {path, area, linhas, sha256} de TUDO que
## entrega (o export omite com aviso o que nao esta registrado).
.forecast_write_scratch <- function(ctx, key, plan, niveis, incluir_membros, lv, rec) {
  parent <- file.path(ctx$state_dir, "scratch")
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(parent, key$run_id)
  unlink(out, recursive = TRUE)
  rows <- list()
  if (length(rec$long)) {
    rows <- prevcarga_reconcile_write(list(long = rec$long), out, key$run_id)
  } else if (!dir.create(out, recursive = TRUE, showWarnings = FALSE)) {
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "nao criou o scratch"))
  }
  put <- function(long, rel, area) {
    p <- file.path(out, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    n <- .reconcile_write_parquet(long, p)
    rows[[length(rows) + 1L]] <<- list(path = rel, area = area, linhas = as.integer(n),
                                       sha256 = digest::digest(file = p, algo = "sha256"))
  }
  folhas <- c(plan$stray, if (identical(niveis, "tudo")) plan$covered)
  for (a in folhas) put(lv$long[[a]], paste0("folhas/", a, ".parquet"), a)
  if (isTRUE(incluir_membros))
    for (sg in names(lv$members)) for (a in names(lv$members[[sg]]))
      put(lv$members[[sg]][[a]], paste0("membros/", sg, "/", a, ".parquet"), a)
  paths <- vapply(rows, function(r) r$path, "")
  list(dir = out, arquivos = rows[order(paths, method = "radix")])
}

.forecast_khat_rows <- function(khat) {
  unname(lapply(names(khat), function(ss) list(
    ss = ss, versao = khat[[ss]]$versao, khat_origin = format(khat[[ss]]$khat_origin),
    khat_age_days = as.integer(khat[[ss]]$khat_age_days))))
}

.forecast_au_by_area <- function(cuts) {
  b <- cuts$available_until_by_area
  if (is.null(b) || !nrow(b)) return(structure(list(), names = character()))
  stats::setNames(lapply(seq_len(nrow(b)), function(i) .forecast_iso_min(b$until[[i]], cuts$tz)), as.character(b$area_code))
}

.forecast_write_run_json <- function(ctx, scratch, g, plan, lv, rec, versao, sc, incluir_membros) {
  mem_areas <- structure(list(), names = character())
  if (isTRUE(incluir_membros))
    mem_areas <- stats::setNames(lapply(names(lv$members), function(s) as.list(names(lv$members[[s]]))), names(lv$members))
  run <- list(origem = format(g$origin), available_until = .forecast_iso_min(g$cuts$available_until, g$cuts$tz),
              available_until_por_area = .forecast_au_by_area(g$cuts), timezone = g$cuts$tz,
              load_label = g$cuts$load_label, weather_issue_cutoff = g$cuts$weather_issue_cutoff,
              khat = .forecast_khat_rows(rec$khat), covered = as.list(plan$covered), stray = as.list(plan$stray),
              closed = as.list(plan$closed), sin_closed = plan$sin_closed, members = mem_areas,
              arquivos = sc$arquivos, versao = versao, version = if (is.null(ctx$version)) "unknown" else ctx$version,
              runtime_id = paste0("R", as.character(getRversion()), "-", R.version$platform),
              code_commit = ctx$code_commit)
  .export_write_json(run, file.path(scratch, "run.json"))
}

.forecast_result <- function(g, plan, rec, key, versao, niveis, exp, report) {
  list(origem = format(g$origin), available_until = .forecast_iso_min(g$cuts$available_until, g$cuts$tz),
       available_until_por_area = .forecast_au_by_area(g$cuts), weather_issue_cutoff = g$cuts$weather_issue_cutoff,
       versao = versao, saida = exp$saida, niveis = niveis, covered = plan$covered, stray = plan$stray,
       khat = .forecast_khat_rows(rec$khat), arquivos = exp$arquivos, cobertura_descartada = rec$cobertura_descartada,
       ja_publicado = isTRUE(exp$ja_publicado), report = report)
}

## --- API publica ----------------------------------------------------------------------------------------------
## `report`/`report_format`: o passo de relatorio e do M6.5; aqui o resultado e sempre list(status = "skipped").
## Devolve list(result, warnings): `warnings` = avisos do catalogo (reconcile, export, gate-0) como list(id, message).
prevcarga_forecast <- function(ctx, origin = NULL, areas = NULL, niveis = "subsistemas", report = FALSE,
                               report_format = "html", skip_validate = FALSE, incluir_membros = FALSE) {
  if (!is.character(niveis) || length(niveis) != 1L || !niveis %in% prevcarga_export_niveis)
    prevcarga_abort("config.usage", where = "--niveis", details = list(hint = "sin, subsistemas ou tudo"))
  sink <- .forecast_warn_sink()
  mem <- .forecast_member_table(ctx)
  g <- .forecast_gate0(ctx, origin, skip_validate, mem, sink)
  plan <- .forecast_plan(ctx, areas)
  rctx <- ctx                                    # convencao da rodada para o que vem depois do infer (build_long, combina, reconcile)
  rctx$tz <- g$cuts$tz
  rctx$load_label <- g$cuts$load_label
  withCallingHandlers({
    members <- .forecast_members(ctx, plan, g, mem)
    lv <- .forecast_leaves(rctx, members, g, plan, mem)
    rec <- .forecast_reconcile(lv$long, plan, rctx)
    versao <- prevcarga_forecast_versao(.forecast_versao_parts(ctx, members, lv, rec, g, plan))
    key <- prevcarga_export_key(g$origin, g$cuts$available_until, versao, g$cuts$tz)
    sc <- .forecast_write_scratch(rctx, key, plan, niveis, incluir_membros, lv, rec)
    .forecast_write_run_json(ctx, sc$dir, g, plan, lv, rec, versao, sc, incluir_membros)
    exp <- prevcarga_export(sc$dir, key, niveis = niveis, incluir_membros = incluir_membros, ctx = ctx)
  }, prevcarga_warning = function(w) {
    sink$add(w$id, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  for (w in rec$warnings) sink$add(w$id, w$message)
  list(result = .forecast_result(g, plan, rec, key, versao, niveis, exp, list(status = "skipped", path = NULL)),
       warnings = sink$get())
}
