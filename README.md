# prevcarga

CLI modular de previsao de carga (R-only, zero-dependencia): um bundle autocontido (R 4.5.3 + bibliotecas + codigo) que se instala
**sem administrador**, em Linux x86_64 e em Windows nativo (PowerShell; WSL2 nao e requisito, ADR-0038). Manual completo:
`docs/reference/MANUAL-USUARIO.md`. Estado duravel do bundle: `docs/estado/m8-bundle.md`.

## Instalar

Linux (`install.sh`, POSIX sh; instala em `~/.local/share/prevcarga`, `--root` troca):

```sh
sh install.sh --version latest
sh install.sh --bundle prevcarga-<v>-linux-x86_64.tar.zst --sha256 <hash>
```

Windows nativo (`install.ps1`; Windows PowerShell 5.1 ou pwsh 7; instala em `%LOCALAPPDATA%\prevcarga`, sem administrador):

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File install.ps1 -Version latest
powershell -NoProfile -ExecutionPolicy Bypass -File install.ps1 -Bundle prevcarga-<v>-windows-x86_64.zip -Sha256 <hash>
```

O sha256 do bundle e obrigatorio e conferido antes de extrair. Layout instalado: `<R>/versions/<v>/` (imutavel) e o ponteiro `<R>/current`;
`--rollback` volta a versao anterior; `--uninstall --yes` remove (o `state/` so sai com `--purge`). Depois: `prevcarga doctor`.

## Usar

A rodada operacional e o verbo `forecast`; `--origin` (AAAA-MM-DD) e o unico flag de data:

```sh
prevcarga forecast --origin 2026-10-15
prevcarga verificar --saida <entrega> --oficial
```

- **Sync**: `prevcarga-sync --what artifacts` e `prevcarga-sync --what forecasts` (`.sh` no Linux, `.ps1`/`.cmd` no Windows) trazem do bucket publico os
  artefatos e as inferencias oficiais para o estado local; so leitura, sem credencial.
- **verificar**: compara a entrega com outra (`--referencia`) ou com a oficial (`--oficial`); trilhos `mesma-plataforma` e `entre-plataformas`.
- **Publicar** (mantenedor, so Linux): `prevcarga release-pack` monta o diretorio no layout do bucket (sem rede) e `scripts/tools/upload` e o unico
  que envia.
- Verbos disponiveis: `prevcarga --help` (o catalogo e descoberto de `scripts/lib/cli/`).

## Erros

Saida de erro ASCII, uma linha `prevcarga: erro [<classe>] (exit <n>)`; `--format json` da um envelope unico. Classes que o operador encontra:
6 (bundle), 8 (config), 9 (integrity), 10 (network), 12 (repro), 13 (state). O catalogo completo e `install/error-catalog.tsv` (gerado).

## Excecoes conscientes do A/B

O A/B do corte contra o sistema anterior exige Delta = 0, com a lista fechada `tests/ab/ab_excecoes.yaml` (so o Gabriel acrescenta):
`regdin.versao_pacotes` (R 4.5.3, ADR-0036), `dessem_linear.hora_incompleta_clima_previsto` e `dessem_linear.sp_2024_11_15_residuo_numerico`.
Veredito atual: **CORTE-BLOQUEADO** (matriz completa e recibos `ab-corte` ainda nao existem); detalhe em `docs/estado/ab.md`.
