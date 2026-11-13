// check-terms.mjs -- termos proibidos/exigidos nas docs (spec Fixes v0.1.4). Sem dependencia externa.
// Uso: node scripts/check-terms.mjs [--self-test]
import { readFileSync, readdirSync } from 'node:fs';
import { resolve, join } from 'node:path';

const ROOT = resolve(import.meta.dirname, '..', '..');
// {files, forbid?|require?, plant?, msg}; caminhos relativos a raiz do repo. `plant` e o texto que o braco CONTROL planta
// para a regra forbid (sem ele, usa o texto da regra de issue_time).
const globMdx = (dir) => {
  const out = [];
  const walk = (d) => {
    for (const e of readdirSync(resolve(ROOT, d), { withFileTypes: true })) {
      const p = join(d, e.name);
      if (e.isDirectory()) walk(p); else if (e.name.endsWith('.mdx')) out.push(p);
    }
  };
  walk(dir);
  return out.sort();
};

export const RULES = [
  { files: ['docs/reference/DECK-CANONICO.md', 'website/docs/metodologia/02-dados-de-entrada.mdx', 'website/docs/execucao/06-formato-deck.mdx'],
    forbid: /n[aã]o\*{0,2} reatribuir/i, msg: 'issue_time vem de dat_referencia (regra antiga removida)' },
  { files: ['docs/reference/DECK-CANONICO.md', 'website/docs/metodologia/02-dados-de-entrada.mdx', 'website/docs/execucao/06-formato-deck.mdx'],
    require: /dat_referencia/, msg: 'issue_time = dat_referencia 00:00 UTC' },
  { files: ['website/docs/execucao/06-formato-deck.mdx'], require: /23:30/, msg: 'clima previsto exige os 48 instantes ate 23:30' },
  { files: ['website/docs/metodologia/03-tempo-rotulo-fusos.mdx'], require: /2019/, msg: 'ressalva de horario de verao no store ate 2019' },
  { files: globMdx('website/docs'), forbid: /--niveis\b/, plant: 'prevcarga forecast --niveis=tudo', msg: '--niveis saiu do forecast (o escopo define a saida)' },
  { files: globMdx('website/docs'), forbid: /fill_forecast_tail/, plant: 'a cauda usa fill_forecast_tail', msg: 'fill_forecast_tail nao e mais a regra: clima curto corta o horizonte' },
  { files: ['website/docs/metodologia/14-avaliacao.mdx'], forbid: /18:00\s*[-a]\s*23:00/, plant: 'ponta de 18:00 a 23:00', msg: 'ponta e 18:00-22:00' },
  { files: ['website/docs/metodologia/14-avaliacao.mdx'], require: /18:00\s*(?:a|-)\s*22:00/, msg: 'avaliacao cita a ponta 18:00-22:00' },
  { files: ['website/docs/metodologia/14-avaliacao.mdx'], require: /\bMAPE\b[\s\S]*\bWMAPE\b|\bWMAPE\b[\s\S]*\bMAPE\b/, msg: 'avaliacao mostra MAPE e WMAPE' },
  ...['set-data-dir', '--artifacts-dir', '--jobs'].map((t) => ({ files: ['website/docs/execucao/12-referencia-cli.mdx'], require: new RegExp(t), msg: `referencia da CLI cita ${t}` })),
];

// Regra de ids: todo token `classe.codigo` entre crases em 13-erros-troubleshooting.mdx precisa existir em scripts/lib/errors/*.tsv.
const ID_CLASSES = ['state', 'contract', 'plugin', 'forecast', 'completude', 'warning', 'partial', 'config', 'topology', 'honesty',
  'runtime', 'integrity', 'export', 'window', 'io', 'repro', 'sync', 'install', 'hw'];
export function catalogIds(readDir, readFile) {
  const ids = new Set();
  for (const f of readDir('scripts/lib/errors').filter((n) => n.endsWith('.tsv')))
    for (const l of readFile(`scripts/lib/errors/${f}`).split('\n')) if (l) ids.add(l.split('\t')[0]);
  return ids;
}
const ARQUIVO = /\.(yaml|sh|ps1|r|md|tsv|json|rds|parquet)$/;   // nome de arquivo, nao id
export function idViolations(page, text, ids) {
  const re = new RegExp(`\`((?:${ID_CLASSES.join('|')})\\.[a-z0-9_]+)`, 'g');
  const out = [];
  text.split('\n').forEach((l, i) => { for (const m of l.matchAll(re)) if (!ARQUIVO.test(m[1]) && !ids.has(m[1])) out.push(`${page}:${i + 1}: id ${m[1]} nao existe em scripts/lib/errors/*.tsv`); });
  return out;
}
const ERROS_PAGE = 'website/docs/execucao/13-erros-troubleshooting.mdx';

export function violations(rules, read) {
  const out = [];
  for (const r of rules) for (const f of r.files) {
    const lines = read(f).split('\n');
    if (r.forbid) lines.forEach((l, i) => { if (r.forbid.test(l)) out.push(`${f}:${i + 1}: ${r.msg}`); });
    if (r.require && !r.require.test(lines.join('\n'))) out.push(`${f}: falta -- ${r.msg}`);
  }
  return out;
}

const read = (f) => readFileSync(resolve(ROOT, f), 'utf8');

if (process.argv.includes('--self-test')) {
  // braco CONTROL: planta o texto proibido (forbid) / um texto vazio (require) e exige que a regra dispare
  const bad = RULES.filter((r) => {
    const text = r.forbid ? (r.plant ?? 'Não reatribuir issue_time de dat_referencia') : '';
    return violations([{ ...r, files: ['x'] }], () => text).length === 0;
  });
  if (bad.length) { console.error('self-test: regra nao dispara:', bad.map((r) => r.msg).join('; ')); process.exit(1); }
  // CONTROL da regra de ids: um id inventado dispara; um id do catalogo, nao
  const ids = new Set(['state.artifact_missing']);
  const falso = idViolations('x', 'a `state.nao_existe_xyz` b', ids);
  const certo = idViolations('x', 'a `state.artifact_missing` b', ids);
  if (falso.length !== 1 || certo.length !== 0) { console.error('self-test: a regra de ids nao distingue id inventado de id do catalogo'); process.exit(1); }
  console.log(`self-test ok (${RULES.length} regras e a regra de ids disparam)`);
} else {
  const ids = catalogIds((d) => readdirSync(resolve(ROOT, d)), read);
  const v = [...violations(RULES, read), ...idViolations(ERROS_PAGE, read(ERROS_PAGE), ids)];
  if (v.length) { console.error(v.join('\n')); process.exit(1); }
  console.log('check-terms ok');
}
