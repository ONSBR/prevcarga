# Estado: verbo `verificar` + extensoes do doctor (M8.7; ADR-0040, ADR-0036, ADR-0038)

## verificar (scripts/lib/verify_repro.R + scripts/lib/cli/verificar.R)
`prevcarga verificar --saida=<dir> (--referencia=<dir> | --oficial) [--trilho=<t>] [--alvo=<nome> --limiares=<arq>] [--sem-membros]`.
Compara duas entregas nativas do `forecast` (manifest.json do M6.2 + run_manifest.json + parquets LONG). Pipeline ESTRITO em 4 fases, cada
uma aborta antes da seguinte (nenhum valor e lido antes da integridade passar; o comparador e injetavel e os testes provam que NAO e chamado):
1. **integridade** (`pc_vr_integrity`): sha256 de TODO arquivo listado em `arquivos[]` do manifest.json; divergente, ausente ou nao listado =>
   `integrity.manifest_sha` (exit 9, `where` = o path). `manifest.json` e `run_manifest.json` ficam fora da lista.
2. **identidade** (`pc_vr_identity`, via `prevcarga_read_run_manifest`): `data_hash`, `bundle_id`, `model_versions` diferentes OU nulos =>
   `contract.identity_mismatch` (exit 2, `details$campo`); `runtime_id` diferente => PASS com aviso `repro.runtime_id_differs` (vai em
   `result$avisos` e no `warnings` do envelope). Sem run_manifest.json a identidade nao e verificavel => exit 2.
3. **pareamento** (`pc_vr_pair`): por `path` relativo; ausente/a mais => `contract.pairing` (exit 2, `details$ausentes/a_mais`).
   `--sem-membros` ignora `membros/` nos dois lados.
4. **valores** (`pc_vr_values`): `prevcarga_read_long` (leitor unico), join por `area, forecast_origin, target_datetime (= interval_end), lead`;
   chave sem par => `repro.key_mismatch` (exit 12); valor fora => `repro.value_out_of_tol` (exit 12) com `result.arquivos[] = {path, pior_delta, n_fora}`.

Trilhos: `mesma-plataforma` |d| <= 1e-5 absoluto; `entre-plataformas` |d| <= 0,001*|ref| (piso absoluto 1e-5 quando ref = 0). Default pela
`platform` do run_manifest (iguais => mesma; diferentes ou desconhecida => entre); `--trilho=entre-plataformas` com plataformas iguais =>
`config.verify_trilho_forced` (exit 8). `--alvo` + `--limiares`: o `limiar_mw` do alvo substitui a tolerancia (todos os arquivos pareados);
o arquivo traz `sha256 = sha256(yaml::as.yaml(limiares))` conferido ANTES de qualquer valor (`integrity.limiares_signature`, exit 9).
`--tol` esta declarado so para o docopt aceita-lo e o handler emitir `config.verify_tol_removed` (exit 8); `--corte` e flag de escopo de
backtest/plan (=> `config.usage`). Removidos do desenho antigo: pareamento legado, normalizacao de argv WSL, limiar 0,1 MW.

`--oficial`: `pc_vr_resolve_oficial` le a chave (`origin|forecast_origin|origem`, `available_until`, `versao`; fuso = `load_tz`/`timezone`) do
run_manifest.json (manifest.json como complemento) e resolve `<state_dir>/forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/` por
`prevcarga_export_key` (exports.R, lubridate + tz). Ausente => `state.official_forecast_missing` (exit 13, `where` = caminho esperado).
Ids novos em `scripts/lib/errors/verificar.tsv` (`state.artifact_missing` ja existe em registry_core.tsv e e reusado para --saida/--referencia
inexistentes).

## doctor (scripts/lib/runtime.R `prevcarga_platform_check`, `prevcarga_doctor_extend`; scripts/lib/cli/doctor.R)
O handler do verbo acrescenta 7 linhas ao construtor (introspect.R nao mudou) e os campos JSON `runtime_id, version, platform, install_root,
state_dir, artefatos[], state_bytes`. Bundle instalado = `RUNTIME.json` na raiz (`ctx$root`) ou um nivel acima (o `code/` do bundle); na
arvore de dev as 7 linhas sao `N/A (nao-instalado)`. Linhas: `bundle.plataforma` (platform do RUNTIME.json == host; Linux exige glibc >= 2.35),
`bundle.r_version`, `bundle.integridade` (`prevcarga_runtime_validate`) => FAIL classe 6; `bundle.versao`; `bundle.current` (arquivo-ponteiro
`<raiz>/current` com o bundle em `<raiz>/versions/<v>` (layout do install.sh do M8.3), sem symlink; versao diferente => WARN; ausente => N/A); `artefatos.presenca` (`latest.txt` -> `v_<hash>`
ausente => FAIL classe 13) e `artefatos.sha` (SHA256SUMS local divergente => FAIL classe 9) sobre `<state_dir>/artifacts/**/latest.txt`.
`state_bytes` = bytes por subarvore de `state_dir`. O exit combina as classes pela precedencia do ADR-0040.

## Pendencias
- O `export` (M6.2) NAO grava `run_manifest.json` na entrega; sem ele o `verificar` sai com exit 2 (identidade). Fechar no forecast/export.
- [DEFERRED M8.6] braco Windows e gate `cross_os` (artefato Linux -> Windows) rodam este verbo entre plataformas; nao executavel nesta maquina.
- A paridade/A-B Delta=0 contra o oraculo e do M8.8.
