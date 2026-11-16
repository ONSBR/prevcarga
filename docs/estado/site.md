# Estado: site de documentacao (`website/`)

Docusaurus 3.10.2 em `website/` (Node 22, `.nvmrc`); tres secoes autogeradas: Metodologia, Arquitetura, Execucao.
`node_modules/`, `build/` e `.docusaurus/` sao ignorados.

## Comandos
- `npm ci` e `npm run start` (dev) ou `npx docusaurus build` (estatico em `website/build/`).
- `npm run check`: `scripts/check-sources.mjs` confere que todo caminho em `fontes:` do frontmatter existe em `git ls-files`.
- `npm run check:mdx`: `scripts/check-mdx.mjs` compila cada pagina isoladamente.
- `npm run check:terms`: `scripts/check-terms.mjs` reprova termos e exemplos antigos em `README.md`, `docs/reference/MANUAL-USUARIO.md` e `website/docs`.
  `node scripts/check-terms.mjs --self-test` roda o braco CONTROL de cada regra (o texto plantado tem de ser reprovado; a nota de apelido o isenta onde a regra tem `allow`).
  Regras novas (regdin-linear-refactor):
  - contagem antiga de membros/variantes ("8 membros", "3 variantes"): a lista vem da topologia;
  - siglas do glossario e do `prevcarga models` iguais a `config/topology.yaml` (9 membros, com `LIN_UNI`);
  - `--data` e `--artifacts-dir` so em linha com "apelido" (a nota de migracao); use `--data-train`, `--data-inference` e `--data-artifacts`;
  - avisos da combina (`warning.combina_eqw`, `combina_stale`, `combina_member_set`) citados na pagina de erros;
  - conteudo minimo da rodada diaria (`08-rodada-diaria.mdx`) e da referencia da CLI (`12-referencia-cli.mdx`).
- `npm run build` quebra com link ou ancora quebrada (`onBrokenLinks` e `onBrokenAnchors` em `throw`, `docusaurus.config.js`); `npm run check` imprime `aviso` ("fontes alteradas depois de <commit_base>") para fonte alterada depois de `commit_base`, resolvido atualizando `revisado_em` e `commit_base` (entre aspas) so nas paginas de fato revisadas.
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

## Publicacao (GitHub Pages)
`.github/workflows/deploy-site.yml` compila `website/` e publica no Pages a cada push na `main` que toque `website/` (ou por `workflow_dispatch`).
A fonte do Pages no repositorio tem de ser **GitHub Actions**, nao "Deploy from a branch": o site precisa de build e nao ha `gh-pages`.
O endereco vem do `actions/configure-pages` (`PC_SITE_URL`, `PC_SITE_BASE`); localmente o `docusaurus.config.js` usa `https://prevcarga.invalid` e `/`.
Toda imagem ou link absoluto passa pelo `baseUrl` (`useBaseUrl` nas paginas React, `${baseUrl}` no item html da navbar, link markdown nas paginas),
para o site funcionar numa subpasta (`/prvc-modelo/`). `commit_base` com zero a esquerda vai entre aspas (o YAML le `0172512` como numero).
