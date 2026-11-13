// check-terms.mjs -- termos proibidos/exigidos nas docs (spec Fixes v0.1.4). Sem dependencia externa.
// Uso: node scripts/check-terms.mjs [--self-test]
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';

const ROOT = resolve(import.meta.dirname, '..', '..');
// {files, forbid?|require?, msg}; caminhos relativos a raiz do repo
export const RULES = [
  { files: ['docs/reference/DECK-CANONICO.md', 'website/docs/metodologia/02-dados-de-entrada.mdx', 'website/docs/execucao/06-formato-deck.mdx'],
    forbid: /n[aã]o\*{0,2} reatribuir/i, msg: 'issue_time vem de dat_referencia (regra antiga removida)' },
  { files: ['docs/reference/DECK-CANONICO.md', 'website/docs/metodologia/02-dados-de-entrada.mdx', 'website/docs/execucao/06-formato-deck.mdx'],
    require: /dat_referencia/, msg: 'issue_time = dat_referencia 00:00 UTC' },
  { files: ['website/docs/execucao/06-formato-deck.mdx'], require: /23:30/, msg: 'clima previsto exige os 48 instantes ate 23:30' },
  { files: ['website/docs/metodologia/03-tempo-rotulo-fusos.mdx'], require: /2019/, msg: 'ressalva de horario de verao no store ate 2019' },
];

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
    const planted = r.forbid ? 'Nao reatribuir issue_time de dat_referencia' : '';
    const text = r.forbid ? planted.replace(/Nao/, 'Não') : '';
    return violations([{ ...r, files: ['x'] }], () => text).length === 0;
  });
  if (bad.length) { console.error('self-test: regra nao dispara:', bad.map((r) => r.msg).join('; ')); process.exit(1); }
  console.log(`self-test ok (${RULES.length} regras disparam)`);
} else {
  const v = violations(RULES, read);
  if (v.length) { console.error(v.join('\n')); process.exit(1); }
  console.log('check-terms ok');
}
