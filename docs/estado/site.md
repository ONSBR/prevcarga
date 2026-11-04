# Estado: site de documentacao (`website/`)

Docusaurus 3.10.2 em `website/` (Node 22, `.nvmrc`); tres secoes autogeradas: Metodologia, Arquitetura, Execucao.
`node_modules/`, `build/` e `.docusaurus/` sao ignorados.

## Comandos
- `npm ci` e `npm run start` (dev) ou `npx docusaurus build` (estatico em `website/build/`).
- `npm run check`: `scripts/check-sources.mjs` confere que todo caminho em `fontes:` do frontmatter existe em `git ls-files`.
- `npm run check:mdx`: `scripts/check-mdx.mjs` compila cada pagina isoladamente.
- `@mermaid-js/layout-elk` e dependencia obrigatoria do build (peer opcional do `theme-mermaid`, o build falha sem ele).

## Convencoes (decisoes do Gabriel, 2026-11)
- O site existe sem a NT: a NT subsidia, mas nao e fonte e nao e citada.
- O site nao apresenta resultados nem desempenho.
- Tom de V1 (o produto como ele e), sem "refatoracao" nem comparacao com o sistema de referencia.
- Execucao e so o que e para executar: nada de testes, gates, A/B, release ou extensao.
- "Repositorio online" no lugar de "bucket" no texto (o `--bucket` do `upload` fica literal; "bucket" do k-hat e outro conceito).
- O sync esta marcado "Em breve" ate o repositorio online de artefatos existir.
- Frontmatter de pagina: `fontes`, `adrs`, `revisado_em`, `commit_base` (lidos pelo `DocItem/Footer` swizzled).
- Mermaid passa pelo `src/theme/Mermaid` swizzled: zoom 30-600%, ajuste a largura e tela cheia dentro do corpo
  (sidebar visivel, so rolagem horizontal na figura, roda do mouse rola o painel).
- Estilo: tokens do PrevCarga Design System em `src/css/ds/` mapeados para Infima em `src/css/custom.css`; logo C em `static/img/`.
- A licenca (Apache-2.0, ONS) mora no repositorio e no bundle, nao no site.
