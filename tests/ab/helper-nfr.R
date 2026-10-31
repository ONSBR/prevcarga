## helper-nfr.R -- gate de orcamento do NFR de 30 min da rodada `forecast` (M8.10). Nao e codigo de produto.
## A duracao NUNCA e medida pelo teste: e a registrada pela rodada no `result` do envelope (`duracao_s`, `passos[]`, `jobs`, `platform`,
## `runtime_id`; forecast.R `.forecast_stopwatch`, relogio injetado pela fachada). O tempo nao entra na entrega nem no run_manifest.json
## (a entrega e byte-deterministica; ver docs/estado/nfr-performance.md).

## `run` = caminho de um JSON com os campos de tempo (o `result` do `forecast --format json`, ou o proprio objeto `result`) ou a lista ja lida.
## Devolve list(ok, duracao_s, budget_s, passos (data.frame passo/duracao_s), mais_lento, platform, runtime_id, jobs).
## Erro de programacao (stopifnot) se faltar campo ou se a soma dos passos nao bater com o total (tolerancia 5 s).
pc_nfr_budget <- function(run, budget_s = 1800) {
  m <- if (is.character(run)) jsonlite::read_json(run, simplifyVector = FALSE) else run
  if (!is.null(m$result) && is.null(m$duracao_s)) m <- m$result
  stopifnot(is.numeric(budget_s), length(budget_s) == 1L, budget_s > 0, is.numeric(m$duracao_s), length(m$passos) > 0L)
  p <- data.frame(passo = vapply(m$passos, function(x) as.character(x$passo), ""),
                  duracao_s = vapply(m$passos, function(x) as.numeric(x$duracao_s), 0), stringsAsFactors = FALSE)
  stopifnot(abs(sum(p$duracao_s) - m$duracao_s) <= 5)
  i <- which.max(p$duracao_s)
  list(ok = m$duracao_s <= budget_s, duracao_s = m$duracao_s, budget_s = budget_s, passos = p, mais_lento = p$passo[[i]],
       platform = as.character(m$platform), runtime_id = as.character(m$runtime_id), jobs = m$jobs)
}

## Mensagem de reprovacao: cita plataforma, duracao, orcamento e o passo mais lento.
pc_nfr_verdict <- function(g)
  sprintf("%s: %.1f s %s %.0f s (passo mais lento: %s)", g$platform, g$duracao_s, if (g$ok) "<=" else ">", g$budget_s, g$mais_lento)

## CONTROL do orcamento: `run(sleep_s)` executa uma rodada `forecast` cujo `infer` espera `sleep_s` e devolve o `result` com os tempos.
## Devolve o `result` da rodada com a espera injetada. A espera e do passo de teste, nunca da topologia oficial.
pc_nfr_injected_round <- function(run, sleep_s = 3) {
  stopifnot(is.function(run), is.numeric(sleep_s), sleep_s > 0)
  run(sleep_s)
}
