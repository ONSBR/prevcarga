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

const ERROS_PAGE = 'website/docs/execucao/13-erros-troubleshooting.mdx';
const RODADA_PAGE = 'website/docs/execucao/08-rodada-diaria.mdx';
const CLI_PAGE = 'website/docs/execucao/12-referencia-cli.mdx';
const DOCS = ['README.md', 'docs/reference/MANUAL-USUARIO.md', ...globMdx('website/docs')];

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
  { files: ['website/docs/metodologia/07-lightgbm.mdx'], require: /\bBASE\b/, msg: 'heat_area inclui BASE' },
  { files: ['website/docs/metodologia/12-reconciliacao.mdx'], require: /incomplet[\s\S]*recus|recus[\s\S]*incomplet/i, msg: 'subsistema incompleto e recusado' },
  { files: ['website/docs/metodologia/12-reconciliacao.mdx'], forbid: /Nota defasada|reutilizado em O/i, plant: 'Nota defasada no estado do modulo: perfil reutilizado em O', msg: 'nota defasada de k-hat removida' },
  { files: ['website/docs/metodologia/13-backtest.mdx'], require: /retrain_harness/, msg: 'default dos origin_scoped e retrain_harness' },
  { files: globMdx('website/docs'), forbid: /\bNT\b|MAPE medido/, plant: 'conforme a NT, o MAPE medido', msg: 'site nao cita a NT nem resultado medido' },
  { files: DOCS, forbid: /\b(8|oito)\s+(membros|modelos)\b|\b(3|tr[e\u00ea]s)\s+variantes\b/i,
    plant: 'combina os 8 membros', msg: 'contagem antiga de membros/variantes (a topologia define a lista)' },
  { files: DOCS, forbid: /--data(?![-\w])/, allow: /apelido/, plant: 'prevcarga forecast --data=./deck',
    msg: '--data sem sufixo so na nota de apelido; use --data-train/--data-inference/--data-artifacts' },
  { files: DOCS, forbid: /--artifacts-dir\b/, allow: /apelido/, plant: 'prevcarga forecast --artifacts-dir=./a',
    msg: '--artifacts-dir so na nota de apelido; use --data-artifacts' },
  ...['set-data-dir', '--data-train', '--data-inference', '--data-artifacts', '--jobs', 'apelido']
    .map((t) => ({ files: [CLI_PAGE], require: new RegExp(t), msg: `referencia da CLI cita ${t}` })),
  ...[[/--data-inference/, '--data-inference'], [/anexo de feriados/i, 'anexo de feriados'],
    [/deck-inference/, 'o verbo deck-inference'], [/03:00/, 'o treino das 03:00'],
    [/08:00/, 'a rodada das 08:00'], [/mensal|todo m[e\u00ea]s/i, 'a cadencia mensal da combina']]
    .map(([re, t]) => ({ files: [RODADA_PAGE], require: re, msg: `rodada diaria cita ${t}` })),
  ...['warning.combina_stale', 'warning.combina_member_set']
    .map((t) => ({ files: [ERROS_PAGE], require: new RegExp(t.replace('.', '\\.')), msg: `pagina de erros cita ${t}` })),
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
export function violations(rules, read) {
  const out = [];
  for (const r of rules) for (const f of r.files) {
    const lines = read(f).split('\n');
    if (r.forbid) lines.forEach((l, i) => { if (r.forbid.test(l) && !(r.allow && r.allow.test(l))) out.push(`${f}:${i + 1}: ${r.msg}`); });
    if (r.require && !r.require.test(lines.join('\n'))) out.push(`${f}: falta -- ${r.msg}`);
  }
  return out;
}

// Siglas dos membros: topologia x glossario x bloco Terminal de `prevcarga models` (sem parser YAML).
const GLOSS = 'website/docs/metodologia/16-glossario.mdx';
const CONF = 'website/docs/execucao/03-configuracao.mdx';
export const topologyMembers = (read) => [...read('config/topology.yaml').matchAll(/sigla:\s*([A-Z_]+)/g)].map((m) => m[1]);
const diff = (page, got, want) => {
  const falta = want.filter((s) => !got.includes(s)), sobra = got.filter((s) => !want.includes(s));
  return falta.length || sobra.length ? [`${page}: siglas diferentes da topologia (falta: ${falta.join(',') || '-'}; sobra: ${sobra.join(',') || '-'})`] : [];
};
export function membersViolations(read) {
  const top = topologyMembers(read), out = [];
  const par = read(GLOSS).split('\n').find((l) => l.startsWith('**Membro.**'));
  if (!par) out.push(`${GLOSS}: falta -- paragrafo **Membro.**`);
  else out.push(...diff(GLOSS, [...par.matchAll(/`([A-Z][A-Z_]+)`/g)].map((m) => m[1]), top));
  const conf = read(CONF), i = conf.indexOf("text:'prevcarga models'");
  if (i < 0) { out.push(`${CONF}: falta -- bloco Terminal de prevcarga models`); return out; }
  const bloco = conf.slice(i, conf.indexOf(']}', i));
  const n = bloco.match(/models: (\d+) membros/);
  if (!n || Number(n[1]) !== top.length) out.push(`${CONF}: 'models: ${top.length} membros' esperado`);
  out.push(...diff(CONF, [...bloco.matchAll(/text:'([A-Z][A-Z_]+)\s/g)].map((m) => m[1]), top));
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
  // isencao: a linha plantada com 'apelido' nao pode disparar
  const semIsencao = RULES.filter((r) => r.allow && violations([{ ...r, files: ['x'] }], () => `${r.plant} (apelido)`).length !== 0);
  if (semIsencao.length) { console.error('self-test: isencao por apelido falhou:', semIsencao.map((r) => r.msg).join('; ')); process.exit(1); }
  // CONTROL de membersViolations: topologia de 9 siglas contra texto de 8 viola; 9 contra 9 nao
  const s9 = ['RF', 'LGBM', 'HW', 'RGD_MED', 'RGD_MAX', 'LIN_MED', 'LIN_MAX', 'LIN_MXMN', 'LIN_UNI'];
  const fake = (gl, n) => (f) => f.endsWith('topology.yaml') ? s9.map((s) => `  - {sigla: ${s}}`).join('\n')
    : f.endsWith('16-glossario.mdx') ? `**Membro.** ${gl.map((s) => '`' + s + '`').join(', ')}`
    : ["{type:'cmd', text:'prevcarga models'},", `{type:'out', text:'models: ${n} membros'},`, ...gl.map((s) => `{type:'out', text:'${s} x'},`), ']} />'].join('\n');
  if (membersViolations(fake(s9.slice(0, 8), 8)).length === 0 || membersViolations(fake(s9, 9)).length !== 0) {
    console.error('self-test: membersViolations nao distingue 9 siglas de 8'); process.exit(1);
  }
  // CONTROL da regra de ids: um id inventado dispara; um id do catalogo, nao
  const ids = new Set(['state.artifact_missing']);
  const falso = idViolations('x', 'a `state.nao_existe_xyz` b', ids);
  const certo = idViolations('x', 'a `state.artifact_missing` b', ids);
  if (falso.length !== 1 || certo.length !== 0) { console.error('self-test: a regra de ids nao distingue id inventado de id do catalogo'); process.exit(1); }
  console.log(`self-test ok (${RULES.length} regras, a regra de ids e a de membros disparam)`);
} else {
  const ids = catalogIds((d) => readdirSync(resolve(ROOT, d)), read);
  const v = [...violations(RULES, read), ...membersViolations(read), ...idViolations(ERROS_PAGE, read(ERROS_PAGE), ids)];
  if (v.length) { console.error(v.join('\n')); process.exit(1); }
  console.log('check-terms ok');
}
