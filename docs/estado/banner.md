# Estado: banner (M1.4c)

Fonte de verdade do desenho e das regras: blueprint "Banner da CLI" (`01M5G8B3PEF80G6DZT9G2E5KB3`) da spec SpecForge "Modular CLI".
Refeito em 2026-11-04 (pedido do Gabriel: "a banner do prevcarga nao e essa curva de carga, olha o blueprint"); o banner anterior
(titulo `prevcarga - previsao de carga (CLI modular)` + 6x48 de `_`/`|` de perfil inventado + `versao <v>`) foi removido.

- `scripts/lib/banner.R`: `prevcarga_banner(ctx, tty, os, version_line = NULL)` devolve 15 linhas ASCII de 7 bits, <= 63 colunas:
  nome em figlet standard (5), vazio, curva (4 linhas, niveis `_ - '`), eixo `|---+---|` e rotulos `0h..24h` (2), vazio,
  `  Previsao de carga do SIN - ONS`, linha de versao. Nome, curva e eixo sao TEXTO copiado literalmente do blueprint (o blueprint
  admite "ou guardada como texto"; os 48 valores do dia real de carga nao estao no repo -- nao foram inventados).
  Geometria: col 3 = 00:00-00:30 .. col 50 = 23:30-24:00 (48 colunas); marcas do eixo nas fronteiras (col 3/15/27/39/51 =
  0h/6h/12h/18h/24h; a linha do eixo tem 49 caracteres entre os `|` inclusive = 48 intervalos + 1 fronteira). Toda linha da
  curva cabe em col 3..50.
- Partes puras (sem dependencia de outro modulo; usadas por instaladores e testes): `.prevcarga_banner_name()`,
  `.prevcarga_banner_curve(axis = TRUE)` (curva + eixo, 6 linhas), `.prevcarga_banner_identity()`, `.prevcarga_banner_static()`
  (as 14 linhas fixas = o bloco copiado nos instaladores).
- Linha de versao `.prevcarga_banner_version_line(ctx)`: `  prevcarga <ctx$version> | R <getRversion()> | <plataforma> | runtime <8 hex>`.
  Bundle instalado: plataforma e `runtime_id` do `RUNTIME.json` via `prevcarga_bundle_runtime(root)` (runtime.R; o mesmo leitor do
  doctor, `prevcarga_platform_check`). Arvore de dev (sem RUNTIME.json): plataforma do host (`.prevcarga_host_platform()`) e
  `runtime dev`. Requer runtime.R carregado (o shim carrega antes).
- Cor por cima do texto puro: nome 34 (azul), curva 33 (amarelo), eixo/identidade/versao 90 (cinza); linhas vazias sem cor.
  Sem os ESC o texto e identico. `prevcarga_use_color(ctx, tty, os)`: `always` sim; `never` nao; `auto` so em TTY fora do Windows.
  Nunca le env (nem NO_COLOR).
- `prevcarga_banner_wanted(ctx, verb, tty = .prevcarga_stdout_tty())`: TRUE so em `--help`, `--version`, sem verbo e `doctor`, e so
  com stdout em terminal; nunca com `format = json` nem com stdout redirecionado (nem com `--color=always`). `.prevcarga_stdout_tty()`
  isolado para os testes in-process simularem o TTY. Consumidores: fachada (cli.R: topo e `prevcarga_emit`) e doctor.
- Testes: `scripts/lib/tests/test-banner.R` (snapshot = bloco do blueprint, 15 linhas, <= 63/80 colunas, ASCII, geometria do eixo,
  versao bundle x dev, cores por parte, CONTROL), `test-facade.R` (TTY simulado; CONTROL nao-TTY sem banner), `test-ascii-bytes.R`
  (processo real: redirecionado sem banner/ESC; CONTROL sob pty com banner e cor), paridade dos instaladores em
  `tests/install/test-install-windows-static.R` e `test-install-etapas-linux.R`.
- Pendencias: (1) `banner: false` no `config.yaml` (blueprint) NAO implementado -- a chave nao existe no schema v1 (`config.R`
  aborta chave desconhecida); entra com decisao de schema. (2) Curva gerada dos 48 valores de um dia real de carga semi-horaria do
  SIN (o blueprint cita o dia fornecido pelo Gabriel): os valores nao estao no repo; hoje a curva e o texto do blueprint.
  (3) `--version --format json` continua imprimindo `prevcarga <v>` em texto (comportamento anterior da fachada, nao do banner).
