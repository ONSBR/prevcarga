# Manual do usuario: prevcarga

Referencia para quem INSTALA e OPERA o produto (bundle R 4.5.3, Linux x86_64 e Windows nativo). Para desenvolver, ver `docs/ESTADO-ATUAL.md` e `docs/estado/`.
O produto nao le variaveis de ambiente: toda configuracao e flag > `config.yaml` > default. WSL2 nao e requisito em nenhum passo (ADR-0038).

## 1. Instalacao

### Linux (`install.sh`)
`install.sh [--version <v>|latest] [--release-base <url>] [--bundle <arq> [--sha256 <h>]] [--root <dir>] [--data <dir>] [--state <dir>] [--add-to-path]`.
Sem administrador; raiz padrao `~/.local/share/prevcarga` (`<R>`). Exige glibc >= 2.35, x86_64, `tar`, `zstd`, `sha256sum` e `curl` ou `wget`.

### Windows (`install.ps1`)
`install.ps1 [-Version <v>|latest] [-ReleaseBase <url>] [-Bundle <zip> [-Sha256 <h>]] [-Root <dir>] [-Data <dir>] [-State <dir>]`, com
`powershell -NoProfile -ExecutionPolicy Bypass -File install.ps1 ...` (Windows PowerShell 5.1 ou pwsh 7). Raiz padrao `%LOCALAPPDATA%\prevcarga`; o PATH muda
so em HKCU. Nao precisa de administrador nem de R instalado: o R e o do bundle.

### Em comum
- O sha256 do bundle e obrigatorio e conferido antes de qualquer extracao; o `RUNTIME.json` do bundle e validado (todo arquivo) antes de ativar.
- Layout: `<R>/versions/<v>/{runtime,code,RUNTIME.json}` (imutavel), `<R>/current` (ponteiro de 1 linha), `<R>/previous`, `<R>/config.yaml`, `<R>/state/`, `<R>/bin/`.
- Retencao: ficam a versao atual e a anterior. **Rollback**: `install.sh --rollback` / `install.ps1 -Rollback`. **Uninstall**: `--uninstall --yes` /
  `-Uninstall -Yes`; o `state/` so e removido com `--purge`.
- Conferir: `prevcarga doctor` (bundle, plataforma, R, integridade do `RUNTIME.json`, artefatos).

## 2. Rodada operacional: `forecast`
`prevcarga forecast --origin <AAAA-MM-DD> [--subsistema <s> | --area <a>] [--report] [--format json]`. E UMA origem por rodada: valida o deck, infere cada
modelo (o re-treino dos modelos `origin_scoped` acontece dentro da rodada), combina, reconcilia e exporta a entrega em `<state>/forecasts/...` com
`manifest.json` e `run_manifest.json`. `--origin` e o unico flag de data humano; os cortes de treino e de dado saem do descriptor e do proprio deck.
A rodada das 0h (sem carga do dia da origem) e valida. Orcamento de desempenho: 30 minutos (`docs/estado/nfr-performance.md`).
O `export` e o `features` existem como verbos soltos para depuracao; a rotina diaria e so o `forecast`.

## 3. Sincronizar com o bucket publico: `prevcarga-sync`
`prevcarga-sync --what artifacts|forecasts [--release-base <url>] [--state <dir>] [--model <m>] [--area <a>] [--origin <AAAA-MM-DD>] [--format text|json]`
(`.sh` no Linux; `prevcarga-sync.ps1` e `.cmd` no Windows, mesmos parametros em `-Pascal`). So leitura HTTP(S), sem credencial. Dois tempos: baixa tudo para
um staging conferindo o sha256 de cada arquivo e so entao publica no estado local; falha no meio deixa o estado intacto.

## 4. Verificar uma entrega: `verificar`
`prevcarga verificar --saida <dir> (--referencia <dir> | --oficial) [--trilho mesma-plataforma|entre-plataformas]`. Fases estritas: integridade (sha256 de todo
arquivo listado), identidade (`data_hash`, `bundle_id`, `model_versions`), pareamento e valores. Dois trilhos: mesma plataforma |d| <= 1e-5; entre plataformas
|d| <= 0,001 do valor de referencia. `--oficial` compara com a entrega oficial ja sincronizada pela chave do proprio `run_manifest.json`.

## 5. Publicar (mantenedor, so Linux)
`prevcarga release-pack --what runtime|artifacts|forecasts --out <dir> --published <arq> [--release <v>] [--latest]` monta localmente o diretorio no layout
do bucket publico (sem rede; recusa conteudo proibido, chave ja publicada, recibo ausente). `scripts/tools/upload --bucket <b> <dir>` e o unico programa que envia
(nunca sobrescreve chave versionada; `latest.txt` por ultimo). Nada e publicado por testes.

## 6. Erros e codigos de saida
Texto: uma linha `prevcarga: erro [<classe>] (exit <n>)` em ASCII; `--format json`: envelope unico. Classes que aparecem na operacao: 6 (bundle), 8 (config), 9 (integrity), 10 (network), 12 (repro), 13 (state), detalhadas abaixo.

| exit | classe | quando |
|---|---|---|
| 6 | bundle | bundle/RUNTIME.json invalido, plataforma errada, pacote ausente |
| 8 | config | argumento ou configuracao invalidos (inclui flag desconhecido) |
| 9 | integrity | sha256 divergente, chave ja publicada, manifest adulterado |
| 10 | network | rede indisponivel (instalador, sync, upload) |
| 12 | repro | `verificar` fora da tolerancia |
| 13 | state | artefato ou entrega oficial ausente no estado |

## 7. Excecoes conscientes do A/B do corte
O A/B contra o sistema anterior e Delta = 0 por celula; so divergem as entradas da lista fechada `tests/ab/ab_excecoes.yaml` (dono: Gabriel):
- `regdin.versao_pacotes`: dessem-regdin, pacotes do R 4.5.3 (ADR-0036), max_rel 1,3e-5;
- `dessem_linear.hora_incompleta_clima_previsto`: hora incompleta descartada tambem no clima previsto (ultimas ~3 h de O+8; ADR-0035);
- `dessem_linear.sp_2024_11_15_residuo_numerico`: SP em 2024-11-15, max_abs 0,5 MW e max_rel 5e-5 (ADR-0035).
Estado do veredito: CORTE-BLOQUEADO ate a matriz completa e os recibos `ab-corte` existirem (`docs/estado/ab.md`).
