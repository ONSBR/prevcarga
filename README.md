<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="website/static/img/prevcarga-logo-c-dark.svg">
    <img alt="PrevCarga" src="website/static/img/prevcarga-logo-c-light.svg" width="420">
  </picture>
</p>

<p align="center">
  <b>Previsão de carga semi-horária do Sistema Interligado Nacional</b><br>
  24 áreas · 8 modelos combinados · reconciliação até os subsistemas e o SIN · D−1 a D+7
</p>

<p align="center">
  <a href="https://github.com/onsbr/prevcarga/releases/latest"><img alt="Release" src="https://img.shields.io/github/v/release/onsbr/prevcarga?sort=semver&label=release"></a>
  <a href="https://github.com/onsbr/prevcarga/actions/workflows/release.yml"><img alt="Build dos bundles" src="https://img.shields.io/github/actions/workflow/status/onsbr/prevcarga/release.yml?label=build"></a>
  <a href="https://github.com/onsbr/prevcarga/actions/workflows/deploy-site.yml"><img alt="Site" src="https://img.shields.io/github/actions/workflow/status/onsbr/prevcarga/deploy-site.yml?branch=main&label=site"></a>
  <a href="https://onsbr.github.io/prevcarga/"><img alt="Documentação" src="https://img.shields.io/badge/docs-onsbr.github.io%2Fprevcarga-blue"></a>
  <a href="LICENSE"><img alt="Licença" src="https://img.shields.io/github/license/onsbr/prevcarga?label=licen%C3%A7a"></a>
  <img alt="R 4.5.3" src="https://img.shields.io/badge/R-4.5.3-276DC3?logo=r&logoColor=white">
  <img alt="Plataformas" src="https://img.shields.io/badge/plataformas-linux%20%7C%20windows-informational">
</p>

<p align="center">
  <a href="#instalação">Instalação</a> ·
  <a href="#primeira-rodada">Primeira rodada</a> ·
  <a href="#atualizar-voltar-e-remover">Atualizar e remover</a> ·
  <a href="#documentação">Documentação</a>
</p>

---

O `prevcarga` é uma CLI que prevê a carga de cada uma das 24 áreas do SIN em 48 patamares por dia e compõe os subsistemas (SECO, S, N, NE) e o SIN a partir delas. Cada rodada parte de uma origem O e entrega de D−1 a D+7.

📖 **Documentação completa: [onsbr.github.io/prevcarga](https://onsbr.github.io/prevcarga/)**, com a metodologia, a arquitetura e o passo a passo de execução.

| | |
|---|---|
| **Modelos** | Random Forest, LightGBM, Holt-Winters, dessem-linear (3 variantes) e dessem-regdin (2 variantes) |
| **Combinação** | por área, com pesos convexos e intercepto, aplicados congelados na rodada |
| **Reconciliação** | Bottom-Up × k-hat por subsistema; o SIN é a soma dos subsistemas |
| **Distribuição** | um bundle fechado por plataforma, com R 4.5.3 e todas as bibliotecas dentro |
| **Plataformas** | Linux x86_64 e Windows 64-bit, sem administrador e sem R instalado |

## Instalação

O instalador confere o sha256 do bundle antes de extrair, valida cada arquivo antes de ativar e troca de versão por um ponteiro de uma linha. O diretório de estado nunca é tocado por atualização, rollback ou desinstalação.

### Requisitos

| | Linux | Windows |
|---|---|---|
| Sistema | x86_64 com glibc 2.35 ou mais nova (musl não é suportado) | Windows 64-bit |
| Shell | POSIX `sh` | Windows PowerShell 5.1 ou pwsh 7 |
| Ferramentas | `tar`, `zstd`, `sha256sum`, `awk` e `curl` ou `wget` | nenhuma além do PowerShell |
| Espaço | — | 2 GiB livres no volume de instalação |
| Instala em | `~/.local/share/prevcarga` | `%LOCALAPPDATA%\prevcarga` |

### Linux

**Em uma linha.** Baixa o instalador da [release mais recente](https://github.com/onsbr/prevcarga/releases/latest) e já o executa:

```sh
curl -fsSL https://github.com/onsbr/prevcarga/releases/latest/download/install.sh | sh -s -- --add-to-path
```

**Baixando o arquivo.** Para ler o instalador antes de rodar, guardá-lo ou instalar numa máquina sem rede:

```sh
curl -fsSLO https://github.com/onsbr/prevcarga/releases/latest/download/install.sh
sh install.sh --add-to-path

# sem rede: com o bundle e o SHA256SUMS da release copiados para a mesma pasta
sh install.sh --bundle prevcarga-<v>-linux-x86_64.tar.zst
```

`--add-to-path` acrescenta `<raiz>/bin` ao `PATH` num bloco marcado do `~/.profile`. Para apontar o deck e o estado já na instalação, use `--data <dir>` e `--state <dir>`; para outra raiz, `--root <dir>`. Na forma em uma linha, as opções vão depois de `sh -s --`.

### Windows

**Em uma linha**, no PowerShell. Baixa o instalador da release mais recente para a pasta temporária e já o executa:

```powershell
[Net.ServicePointManager]::SecurityProtocol = 'Tls12'; iwr -useb https://github.com/onsbr/prevcarga/releases/latest/download/install.ps1 -OutFile $env:TEMP\install.ps1; powershell -NoProfile -ExecutionPolicy Bypass -File $env:TEMP\install.ps1
```

**Baixando o arquivo.** Pelo navegador, na página da [release](https://github.com/onsbr/prevcarga/releases/latest), ou pelo PowerShell:

```powershell
iwr -useb https://github.com/onsbr/prevcarga/releases/latest/download/install.ps1 -OutFile install.ps1
powershell -NoProfile -ExecutionPolicy Bypass -File install.ps1

# sem rede: com o .zip e o SHA256SUMS da release copiados para a mesma pasta
powershell -NoProfile -ExecutionPolicy Bypass -File install.ps1 -Bundle prevcarga-<v>-windows-x86_64.zip
```

No Windows o `PATH` é ajustado sempre, só no registro do usuário (`HKCU`). Os parâmetros espelham os do Linux: `-Root`, `-Data`, `-State`, `-Rollback`, `-Uninstall -Yes`, `-Purge`; na forma em uma linha, vão depois do `-File ...\install.ps1`. O `Tls12` do começo é para o Windows PowerShell 5.1, que nem sempre oferece TLS 1.2 por padrão.

> [!NOTE]
> O bundle vem das GitHub Releases de `onsbr/prevcarga` (tag `v<v>`). Para um espelho, passe `--release-base <url>` (ou `-ReleaseBase`); fora do `github.com` vale o layout `<base>/releases/<v>/`. Sem rede, instale de um arquivo local com `--bundle`. Os artefatos oficiais continuam vindo do repositório online, pelo `prevcarga-sync`.

### Conferir a instalação

```sh
prevcarga doctor
```

O `doctor` mostra a versão ativa, o runtime do bundle, a configuração e o estado. Se algo faltar, ele diz o quê.

## Primeira rodada

A rodada precisa de um **deck canônico** num diretório local: carga verificada, clima previsto e observado, feriados e um `manifest.json` na raiz.

```sh
# 1. artefatos oficiais: modelos, pesos da combinação e perfis k-hat (em breve)
prevcarga-sync --what artifacts

# 2. conferir o deck antes de rodar
prevcarga validate-deck --origin 2026-10-15 --data ./deck

# 3. a rodada: infere os membros, combina, reconcilia e entrega
prevcarga forecast --origin 2026-10-15 --data ./deck
```

> [!NOTE]
> **Em breve:** o repositório online de artefatos ainda não está no ar, então o passo 1 ainda não funciona. Até lá, os artefatos são treinados na própria máquina (`prevcarga train-models`, `combine --mode train` e `reconcile --mode train`).

- `--origin` é o único flag de data. Sem ele, a origem é o último dia de carga do deck.
- O corte intradiário sai do próprio dado de carga; não há flag de corte.
- Com `data_dir` no `config.yaml` (ou `--data` na instalação), `--data` pode ser omitido.
- A entrega fica em `<state>/forecasts/<origem>/…`, com chave imutável. `--report` gera também o relatório da rodada.

Para conferir que a sua entrega reproduz a oficial:

```sh
prevcarga-sync --what forecasts --origin 2026-10-15 --state /caminho/estado-oficial
prevcarga verificar --saida <entrega> --oficial --state /caminho/estado-oficial
```

## Atualizar, voltar e remover

```sh
sh install.sh --version latest          # atualizar: a versão ativa vira a anterior
sh install.sh --rollback                # voltar um nível
sh install.sh --uninstall --yes         # remover; o estado é preservado
sh install.sh --uninstall --yes --purge # remover também o estado
```

Ficam retidas duas versões, a atual e a anterior. O rollback vale um nível só.

> [!CAUTION]
> `--purge` apaga o diretório de estado inteiro: entregas, artefatos treinados, cache e backtest. Não há como desfazer.

### Layout instalado

```text
<raiz>/
  current            # 1 linha: a versão ativa
  previous           # 1 linha: a versão anterior
  config.yaml        # data_dir, state_dir, format, color
  bin/               # prevcarga, prevcarga-sync, catálogo de erros
  versions/<v>/      # imutável: runtime, código e RUNTIME.json
  state/             # entregas, artefatos, cache (nunca tocado por atualização)
```

## Erros

Toda falha sai como uma linha ASCII, `prevcarga: erro [<classe>] (exit <n>)`, com o código estável e uma dica. Com `--format json`, a saída é um envelope único. Os mais comuns na instalação:

| Exit | Classe | Quando |
|---|---|---|
| 6 | `bundle` | plataforma fora do suportado ou bundle incompleto |
| 8 | `config` | argumento inválido ou rollback sem versão anterior |
| 9 | `integrity` | sha256 ausente ou diferente do esperado |
| 10 | `network` | falha no download |
| 13 | `state` | artefato ou perfil k-hat ausente para a origem |

## Documentação

A documentação está publicada em **[onsbr.github.io/prevcarga](https://onsbr.github.io/prevcarga/)** e tem três seções:

- **[Metodologia](https://onsbr.github.io/prevcarga/metodologia/visao-geral/)** — dados de entrada, cortes de data, os modelos, combinação, reconciliação e avaliação;
- **[Arquitetura](https://onsbr.github.io/prevcarga/arquitetura/principios/)** — módulos, CLI, plugins, fluxo de dados, registry, cache e distribuição;
- **[Execução](https://onsbr.github.io/prevcarga/execucao/inicio-rapido/)** — instalação, configuração, sync, rodada diária, backtest, verificação e referência da CLI.

Para começar, veja o [Início rápido](https://onsbr.github.io/prevcarga/execucao/inicio-rapido/) e a página de [Instalação, atualização e remoção](https://onsbr.github.io/prevcarga/execucao/instalacao/).

O código-fonte do site mora em [`website/`](website/). Para ver localmente (Node 20 ou mais novo):

```sh
cd website
npm ci
npm run start
```

## Licença

Copyright 2026 Operador Nacional do Sistema Elétrico - ONS.

Distribuído sob a [Apache License 2.0](LICENSE). Ver também o [`NOTICE`](NOTICE). Os dois arquivos acompanham o bundle, em `code/`.

---

<p align="center">
  <sub>ONS · Operador Nacional do Sistema Elétrico</sub>
</p>
