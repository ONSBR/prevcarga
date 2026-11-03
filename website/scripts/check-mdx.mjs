// Compila cada .mdx com o MDX v3 (o mesmo do Docusaurus) para pegar erro de sintaxe sem o build inteiro.
// Uso: node scripts/check-mdx.mjs docs/metodologia/01-visao-geral.mdx [...]
import fs from 'node:fs';
import { compile } from '@mdx-js/mdx';
let bad = 0;
for (const f of process.argv.slice(2)) {
  const src = fs.readFileSync(f, 'utf8').replace(/^---\n[\s\S]*?\n---\n/, '');
  try { await compile(src, { format: f.endsWith('.md') ? 'md' : 'mdx' }); console.log(`ok ${f}`); }
  catch (e) { bad++; console.log(`ERRO ${f}: ${e.message}`); }
}
process.exit(bad ? 1 : 0);
