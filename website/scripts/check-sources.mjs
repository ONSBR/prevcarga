// Confere o frontmatter de toda pagina do site contra o repositorio (plano do site, secao 6.1).
// Regras: 1) fontes nao vazio; 2) todo caminho/glob de fontes existe no git ls-files;
// 3) ids de adrs no formato ADR-NNNN (aviso); 4) <Figura fontes=...> cobertas pelo frontmatter;
// 5) aviso de defasagem quando git log commit_base..HEAD toca alguma fonte.
// Uso: node scripts/check-sources.mjs [arquivo.mdx ...]
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import yaml from 'js-yaml';
import micromatch from 'micromatch';

const site = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..');
const repo = execFileSync('git', ['rev-parse', '--show-toplevel'], { cwd: site }).toString().trim();
const tracked = execFileSync('git', ['ls-files'], { cwd: repo }).toString().split('\n').filter(Boolean);
const trackedSet = new Set(tracked);

function pages(dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
    const p = path.join(dir, e.name);
    return e.isDirectory() ? pages(p) : /\.mdx?$/.test(e.name) ? [p] : [];
  });
}
const files = process.argv.slice(2).length ? process.argv.slice(2).map((f) => path.resolve(f)) : pages(path.join(site, 'docs'));

function exists(entry) {
  if (entry.endsWith('/')) return tracked.some((f) => f.startsWith(entry));
  if (/[*?{[]/.test(entry)) {
    const alts = entry.match(/\{([^}]*)\}/);
    if (alts) return alts[1].split(',').every((a) => exists(entry.replace(alts[0], a)));
    return micromatch(tracked, entry).length > 0;
  }
  return trackedSet.has(entry);
}
function covered(src, fontes) {
  return fontes.some((f) => f === src || (f.endsWith('/') && src.startsWith(f)) || (/[*?{[]/.test(f) && micromatch.isMatch(src, f)));
}

let fails = 0, warns = 0;
for (const file of files) {
  const rel = path.relative(site, file);
  const text = fs.readFileSync(file, 'utf8');
  const m = /^---\n([\s\S]*?)\n---/.exec(text);
  const fm = m ? yaml.load(m[1]) || {} : {};
  const fontes = Array.isArray(fm.fontes) ? fm.fontes.map(String) : [];
  const err = (msg) => { fails++; console.log(`FALHA ${rel}: ${msg}`); };
  const warn = (msg) => { warns++; console.log(`aviso ${rel}: ${msg}`); };
  if (!fontes.length) err('frontmatter sem fontes');
  for (const f of fontes) if (!exists(f)) err(`fonte inexistente no git ls-files: ${f}`);
  for (const a of fm.adrs || []) if (!/^ADR-\d{4}$/.test(String(a))) warn(`id de ADR fora do formato ADR-NNNN: ${a}`);
  if (!fm.revisado_em) err('frontmatter sem revisado_em');
  for (const fig of text.matchAll(/<Figura\b[^>]*fontes=\{\[([^\]]*)\]\}/g)) {
    for (const s of fig[1].split(',').map((x) => x.trim().replace(/^['"]|['"]$/g, '')).filter(Boolean)) {
      if (!covered(s, fontes)) err(`fonte de figura fora do frontmatter: ${s}`);
    }
  }
  if (fm.commit_base && fontes.length) {
    try {
      const touched = execFileSync('git', ['log', '--format=%h', `${fm.commit_base}..HEAD`, '--', ...fontes.filter((f) => !/[*?{[]/.test(f))], { cwd: repo }).toString().trim();
      if (touched) warn(`fontes alteradas depois de ${fm.commit_base}: revisar a pagina`);
    } catch { warn(`commit_base ${fm.commit_base} nao encontrado`); }
  }
}
console.log(`check-sources: ${files.length} paginas, ${fails} falhas, ${warns} avisos`);
process.exit(fails ? 1 : 0);
