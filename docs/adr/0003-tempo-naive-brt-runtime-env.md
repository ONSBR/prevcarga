# ADR-0003 — Convenção temporal naive-BRT (relabel tz=UTC, nunca converter) + contrato de ENV de runtime (TZ=UTC + locale C)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0002](0002-grade-semihoraria-schema-long.md), [ADR-0008](0008-honestidade-determinismo-falsificavel.md), [ADR-0009](0009-arquitetura-modular-modelo.md), [ADR-0016](0016-orquestracao-io-nuvem.md)

## Contexto
O legado e os decks DESSEM raciocinam em relógio-de-parede BRT, e o DST histórico brasileiro já foi absorvido na malha semi-horária. Houve um bug de shift cross-model (−3h de RF/HW) causado por conversão de fuso real e por sessões em pt_BR.

## Decisão
Tempo é um relógio BRT carregado dentro de um contêiner nominalmente UTC: todo datetime é POSIXct construído/lido com `tz='UTC'` sobre dígitos BRT (**relabel, nunca convert**); nenhuma aritmética de fuso em todo o pipeline. O contrato é cravado no runtime: `TZ=UTC` + `LANG/LC_ALL=C.UTF-8` forçados no launcher e reafirmados no entrypoint (belt-and-suspenders); pt_BR é proibido (reintroduz o shift −3h).

## Consequências
Toda a correção do shift cross-model depende disto; qualquer `as.Date(tz=)`/conversão real reintroduz o deslocamento e desalinha picos entre modelos e contra y_true. É também pré-condição da comparação cross-língua (combina Py / reconcilia R concordam na grade). Uma decisão correlata (`reference_date` como issue_time) APAGA a hora UTC de emissão do clima — em aberto (ver abaixo).

## Alternativas consideradas
Armazenar tudo em UTC real tz-aware e converter para America/Sao_Paulo na borda (lida com DST corretamente, mas muda o alinhamento histórico); gravar strings sem tipo; deixar threads/locale ao host.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/lib/build_long_output.R:85
- scripts/lib/build_long_output.R:129
- scripts/lib/combina/io.py:100-101
- scripts/install.sh:122-138
- jobs/entrypoint.sh:17-30
- docs/ESTADO-ATUAL.md:863-866

## Decisão (resolvida 2026-10-11, Gabriel) — PRESERVAR a emissão no instante cheio (opção B)
Não se achata a emissão para data local. O `issue_time` do clima previsto fica como **`timestamp` no instante
cheio** (alias canônico = **`din_origemprevisaoutc` preservado**, nunca reatribuído de `dat_referencia` como
fazia `scripts/data-prep/migrate_legacy_data.py:368` na referência).

**Fato de domínio que força a decisão (Gabriel):** o clima previsto em geral tem **4 emissões/dia, de 6 em 6h
a partir de 00 UTC** (00Z/06Z/12Z/18Z). O achatamento pra data (opção A) **colapsa as 4 numa só e PERDE as
rodadas 06/12/18Z** — perda de dado real, não acadêmica; inviabiliza D+0/nowcast pegar a rodada mais fresca.
(A amostra `/opt/source/prevcarga-R/data/weather/forecast` MS/2023 só tinha 00Z — recorte histórico pobre; o
schema do store já é `timestamp[ns]`, aguenta N rodadas sem mudança.)

**Risco de horizonte negativo (a razão do achatamento antigo) e como se resolve:** emissões são UTC
(00/06/12/18Z), a grade é naive-BRT (UTC−3), então 00Z→21:00 do dia anterior, 06Z→03:00, 12Z→09:00, 18Z→15:00.
O relabel ingênuo faz o 00Z "parecer" posterior a alvos do dia → horizonte < 0. **Fix (desenho do M5/data-prep):**
emissão com resolução de 6h + **regra explícita de dia-de-origem** da rodada + **horizonte UTC-safe** (não do
relabel cru). Isso **não** viola o invariante naive-BRT da Decisão principal: a grade/`valid_time` seguem
naive-BRT; a emissão é o único instante genuinamente UTC, usado só pra ordenação/freshness do corte.

**Habilita** o corte intradiário de clima no espírito do [ADR-0007](0007-contrato-corte-intradiario.md): o
filtro de honestidade passa a ser "usar a emissão mais fresca com `issue_time ≤ instante de corte`". Contrato
registrado em [`docs/reference/DECK-CANONICO.md`](../reference/DECK-CANONICO.md). (Esta subparte é
**changed-in-rebuild**; a Decisão principal naive-BRT continua carry-over.)
