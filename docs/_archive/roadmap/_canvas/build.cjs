#!/usr/bin/env node
// build.cjs — gera canvas.html (self-contained, sem CDN) a partir de data/_resolve.json + data/M*.json.
// Uso: node build.cjs            (re-render)        · regenerar/adicionar dado: workflow `milestone-canvas` (ou /canvas-milestone M8)
// Eixo primário = topologia interna do pacote (verbos → pipeline [treino ⫽ diário] + motores → transversais → config/gates);
// milestone = overlay de ÊNFASE (delta aceso, resto apagado). Arestas só em foco. Curadoria explícita abaixo (repo/docs vencem o extrator).
'use strict';
const fs = require('node:fs');
const path = require('node:path');

const DATA_DIR = path.join(__dirname, 'data');
const R = JSON.parse(fs.readFileSync(path.join(DATA_DIR, '_resolve.json'), 'utf8'));
const byNum = (a, b) => parseInt(a.slice(1), 10) - parseInt(b.slice(1), 10);
const ORDER = R.milestones.map((m) => m.id).sort(byNum);
const rank = (id) => { const i = ORDER.indexOf(id); return i < 0 ? 999 : i; };
const raw = fs.readdirSync(DATA_DIR).filter((f) => /^M\d+\.json$/.test(f)).map((f) => JSON.parse(fs.readFileSync(path.join(DATA_DIR, f), 'utf8'))).sort((a, b) => rank(a.id) - rank(b.id));
const base = (id) => id.replace(/^(mod|tool|verb|fn|cfg):/, '');

// ---------- curadoria (correções repo/docs-vencem; sobrevivem a re-extração) ----------
// arestas que o extrator leu como dependência de MÓDULO mas são fluxo de dado / propriedade do verbo (M2-DATAS.md:257-259, :509)
const EDGE_DROP = new Set(['mod:dates|mod:provenance|depends-on', 'mod:dates|mod:runtime|depends-on']);
// padrão M4/M5: o CONSUMIDOR aponta para o gate (combination→honesty_gate 'G2'); o extrator inverteu no M6 (M6-FORECAST.md:77,:194)
const EDGE_FLIP = { 'mod:honesty_gate|mod:reconcile|depends-on': { from: 'mod:reconcile', to: 'mod:honesty_gate', label: 'reach-control k̂ (2a)' } };
const MS_NAME = { M1: 'fundação', M2: 'datas', M3: 'avaliação', M4: 'combina', M5: 'training', M6: 'forecast', M7: 'orquestração', M8: 'bundle' };
const msName = (m) => MS_NAME[m.id] || (m.slug || '').toLowerCase() || m.id;

// ---------- fold determinístico (o mesmo que o workflow validava) ----------
const nodeMap = new Map(), edgeMap = new Map(), deltas = {};
for (const d of raw) {
  deltas[d.id] = { id: d.id, title: d.title || d.id, intent: d.intent || '', status: d.status || '', docPath: d.docPath || '', files: d.files || [], functions: d.functions || [], functionality: d.functionality || [], verbs: d.verbs || [] };
  for (const n of d.nodes || []) { const p = nodeMap.get(n.id); const ms = n.milestone || d.id; if (!p || rank(ms) < rank(p.milestone)) nodeMap.set(n.id, { id: n.id, label: n.label, kind: n.kind, parent: n.parent || '', note: n.note || '', milestone: ms }); }
  for (const e of d.edges || []) {
    let ee = { from: e.from, to: e.to, kind: e.kind, label: e.label || '', milestone: e.milestone || d.id };
    const k0 = ee.from + '|' + ee.to + '|' + ee.kind;
    if (EDGE_DROP.has(k0)) continue;
    if (EDGE_FLIP[k0]) ee = Object.assign(ee, EDGE_FLIP[k0]);
    const k = ee.from + '|' + ee.to + '|' + ee.kind;
    if (!edgeMap.has(k)) edgeMap.set(k, ee);
  }
}
for (const m of R.modules || []) { const b = String(m.file).replace(/^.*\//, '').replace(/\.R$/i, ''); const isTool = m.kind === 'tool' || /scripts\/tools\//.test(String(m.file)); const id = (isTool ? 'tool:' : 'mod:') + b; if (!nodeMap.has(id)) nodeMap.set(id, { id, label: b + '.R', kind: isTool ? 'tool' : m.kind === 'transversal' ? 'transversal' : 'module', parent: '', note: m.responsibility || '', milestone: m.milestone || ORDER[0] }); }
for (const [, n] of nodeMap) if (n.parent && !nodeMap.has(n.parent)) n.parent = '';
const EDGES = [...edgeMap.values()].filter((e) => nodeMap.has(e.from) && nodeMap.has(e.to));
const byId = nodeMap;
const fnsOf = (id) => [...nodeMap.values()].filter((n) => n.kind === 'function' && n.parent === id).map((f) => ({ id: f.id, label: f.label, ms: f.milestone, note: f.note || '' }));

// ---------- ordenação semântica + taglines (CLI-MODULAR-OBJETIVO.md §2/§3, scripts/lib/README.md, M*.md) ----------
const FLOW_TRAIN = ['features', 'training'];                                   // caminho de TREINO (train-models)
const FLOW_DAILY = ['inference', 'build_long_output', 'combination', 'reconcile', 'exports']; // ciclo DIÁRIO (forecast: apply, nunca fit)
const ENGINES = ['forecast', 'backtest', 'morning'];                                        // motores de verbo (compõem o fluxo)
const PIPE_META = {
  features: { st: 'FE-fachada', ref: 'módulo 2', tag: '1 assinatura sobre os 5 plugins; transforms de alinhamento' },
  training: { st: 'treino por modelo×área', ref: 'módulo 3', tag: 'corte honesto por modelo; artefato content-addressed' },
  inference: { st: 'inferência do artefato', ref: 'módulo 4', tag: 'params lidos do artefato congelado; nunca re-fit' },
  build_long_output: { st: 'painel LONG 8-col', ref: 'módulo 4 · semente', tag: 'monta o painel; junta y_true honesto por instante' },
  combination: { st: 'ensemble convexo', ref: 'módulo 5', tag: 'FISTA/simplex em R-base; leak #1 corrigido' },
  reconcile: { st: 'hierárquico via k̂', ref: 'módulo 6', tag: 'região→subsistema→SIN; topologia fonte-única' },
  exports: { st: 'parquet + manifest', ref: 'módulo 7', tag: 'byte-copy por nível + manifest.json; nunca S3' },
  forecast: { st: 'motor do verbo forecast', ref: 'ciclo diário', tag: 'infer→LONG→combina-apply→reconcile→export (apply, nunca fit)' },
  backtest: { st: 'motor do verbo backtest', ref: 'pra-trás · usa orchestrate.R', tag: 'walk-forward pra-trás; combina honesta por-mês' },
  morning: { st: 'motor do verbo morning', ref: 'runbook diário · usa forecast.R', tag: 'status → validate-deck → score(ontem) → re-fit harness → forecast --pins → report; nunca promove' },
};
const TRANS_ORDER = ['runtime', 'topology', 'provenance', 'registry', 'dates', 'validation', 'evaluation', 'honesty_gate', 'reporting', 'orchestrate', 'cache', 'verify_repro'];
const TRANS_META = {
  runtime: { st: 'runtime-guard', ref: 'módulo 13', tag: 'seta e assere TZ/LC/BLAS 1-thread; roda primeiro' },
  topology: { st: 'fonte única', ref: 'módulo 8', tag: 'carrega e valida config/topology.yaml; membros e model_spec' },
  provenance: { st: 'run-manifest', ref: 'módulo 12', tag: 'version content-addressed, run-manifest, freeze/thaw' },
  registry: { st: 'contrato (ADR-0010) · ciclo de artefato', ref: 'fronteira de tiers (M1) + ciclo (M5)', tag: 'ls · show · promote-atômico · rollback · pin · gc' },
  dates: { st: 'as 3 datas (§4.1)', ref: 'forecast-origin · corte-de-treino · corte-intradiário', tag: 'os 3 cortes a partir de --as-of; corte honesto por modelo' },
  validation: { st: 'deck · gate-0', ref: 'módulo 1 (pipeline no OBJETIVO) · consumido por todos', tag: 'formato, completude flat-48, sanidade física; validate-deck' },
  evaluation: { st: 'métrica fonte-única', ref: 'módulo 9', tag: 'WMAPE/MAPE/MAE/RMSE/BIAS/peak + skill-score' },
  honesty_gate: { st: 'honestidade falsificável', ref: 'ADR-0008', tag: 'gate FORBIDDEN/CONTROL in-process (|Δ|≤1e-5)' },
  reporting: { st: 'HTML + PDF', ref: 'módulo 10', tag: 'forecast --report diagnóstico · backtest --report acurácia' },
  orchestrate: { st: 'ledger local', ref: 'módulo 11', tag: 'matriz membro×folha×origem; plan/--resume/--keep-going' },
  cache: { st: 'content-addressed', ref: 'módulo 14', tag: 'reuso que honra o corte (class-aware)' },
  verify_repro: { st: 'reprodutibilidade (QA)', ref: 'verbo verificar · ADR-0017', tag: 'sha256 → identidade (run-manifest) → |Δ y_pred| por trilho vs referência; exit 9' },
};
const VERB_GROUPS = [
  { id: 'pipeline', label: 'Pipeline', verbs: ['validate-deck', 'train-models', 'infer-model', 'forecast', 'backtest', 'morning'] },
  { id: 'avaliacao', label: 'Avaliação', verbs: ['score', 'check-coherence', 'verificar'] },
  { id: 'composicao', label: 'Composição (estágio isolado)', verbs: ['features', 'combine', 'reconcile', 'export'] },
  { id: 'operacao', label: 'Operação / introspecção', verbs: ['status', 'doctor', 'plan', 'registry', 'models', 'areas', 'describe'] },
  { id: 'alias', label: 'Aliases', verbs: ['treinar', 'prever'] },
];
const VERB_BLURB = { // curtos, aparados ao que o milestone-dono entrega
  status: 'deck mais recente usável + idade/versão do bundle + resumo do doctor',
  doctor: 'pré-voo de runtime (R/renv, arrow); assere TZ/LC/BLAS 1-thread',
  models: 'introspecção: modelos/membros da topologia (fonte única)',
  areas: 'introspecção: folhas → subsistemas → SIN (fonte única)',
  describe: 'descreve um modelo: horizonte, janela e os 3 cortes (--as-of/--model)',
  'validate-deck': 'gate-0: formato + completude + sanidade do deck ANTES de gastar compute',
  score: 'pontua previsão publicada vs y_true fresco (review D+1): acurácia + skill',
  'check-coherence': 'SIN = Σsubsistemas · subsistema = Σregiões·k̂ em faixa física',
  combine: 'combina em isolado sobre painéis existentes (train/apply/full)',
  reconcile: 'reconcilia em isolado (k̂ honesto, topologia fonte-única)',
  export: 'exporta em isolado: byte-copy + manifest.json',
  features: 'materializa a feature_table pela FE-fachada, sem treinar/inferir',
  'train-models': 'treina modelos×áreas (default: todos); corte honesto POR modelo',
  'infer-model': 'infere UM modelo×área com params lidos do artefato',
  registry: 'ciclo de vida: ls · show · promote-atômico · rollback · pin · gc',
  forecast: 'ciclo diário: infer → painel LONG → combina-apply → reconcile → export; --report diagnóstico',
  backtest: 'treino→infer pra-trás dia-a-dia; combina honesta por-mês; --report acurácia; --resume/--corte',
  plan: 'materializa a matriz membro×folha×origem + custo estimado, sem disparar',
  treinar: 'alias → train-models (aviso de depreciação)',
  prever: 'alias → forecast (aviso de depreciação)',
  verificar: 'reprodutibilidade: sha256 → identidade → |Δ y_pred| por trilho vs referência (QA, não acurácia); exit 9',
  morning: 'runbook diário: status → validate-deck → score(ontem) → re-fit harness → forecast --pins → report; nunca promove',
};
const PLANNED_VERBS = ORDER.includes('M8') ? [] : [ // o OBJETIVO lista; os docs mandam p/ M8 (M1:321, M7:75)
  { name: 'verificar', note: 'reprodutibilidade sha256 + |Δ y_pred| vs referência (QA, não acurácia)' },
  { name: 'publish', note: 'elege/congela o bundle oficial do dia' },
  { name: 'morning', note: 'runbook: status → validate-deck → score(ontem) → forecast → report' },
];

// ---------- view-model ----------
const handlerOf = {};
for (const f of [...nodeMap.values()].filter((n) => n.kind === 'function' && /^cmd_/.test(base(n.id)))) { const v = 'verb:' + base(f.id).replace(/^cmd_/, '').replace(/_/g, '-'); if (byId.has(v)) handlerOf[v] = { label: f.label, ms: f.milestone }; }
const mkMod = (name, kind, meta) => { const id = 'mod:' + name; const n = byId.get(id); if (!n) return null; const m = meta[name] || {}; return { id, label: n.label, kind, ms: n.milestone, st: m.st || '', ref: m.ref || '', tag: m.tag || n.note || '', note: n.note || '', fns: fnsOf(id) }; };
const flowTrain = FLOW_TRAIN.map((m) => mkMod(m, 'module', PIPE_META)).filter(Boolean);
const flowDaily = FLOW_DAILY.map((m) => mkMod(m, 'module', PIPE_META)).filter(Boolean);
const engines = ENGINES.map((m) => mkMod(m, 'module', PIPE_META)).filter(Boolean);
const transversal = TRANS_ORDER.map((m) => mkMod(m, 'transversal', TRANS_META)).filter(Boolean);
const tools = [...nodeMap.values()].filter((n) => n.kind === 'tool').sort((a, b) => a.label.localeCompare(b.label)).map((n) => ({ id: n.id, label: n.label, kind: 'tool', ms: n.milestone, st: 'ferramenta · scripts/tools/', ref: 'fora do runtime (tier tools/, CLAUDE.md)', tag: n.note || '', note: n.note || '', path: 'scripts/tools/' + n.label, fns: fnsOf(n.id) })); // harness A/B / inspeção: NÃO são módulos do pacote
for (const n of [...nodeMap.values()].filter((n) => n.kind === 'module' || n.kind === 'transversal')) { // segurança: módulo novo no README sem ordem manual
  const all = [...flowTrain, ...flowDaily, ...engines, ...transversal]; if (!all.some((x) => x.id === n.id)) (n.kind === 'module' ? engines : transversal).push(mkMod(base(n.id), n.kind, {}));
}
let step = 0; for (const m of [...flowTrain, ...flowDaily]) m.step = ++step;
const verbGroups = VERB_GROUPS.map((g) => ({ id: g.id, label: g.label, verbs: g.verbs.map((v) => byId.get('verb:' + v)).filter(Boolean).map((n) => ({ id: n.id, label: n.label, kind: 'verb', ms: n.milestone, blurb: VERB_BLURB[base(n.id)] || n.note || '', note: n.note || '', handler: handlerOf[n.id] ? handlerOf[n.id].label : '', handlerMs: handlerOf[n.id] ? handlerOf[n.id].ms : '' })) }));
for (const n of [...nodeMap.values()].filter((n) => n.kind === 'verb')) if (!verbGroups.some((g) => g.verbs.some((v) => v.id === n.id))) verbGroups[3].verbs.push({ id: n.id, label: n.label, kind: 'verb', ms: n.milestone, blurb: n.note || '', note: '', handler: '', handlerMs: '' });
if (PLANNED_VERBS.length) verbGroups.push({ id: 'planned', label: 'Planejados (M8)', verbs: PLANNED_VERBS.map((v) => ({ id: 'verb:' + v.name, label: v.name, kind: 'verb', ms: 'M8', blurb: v.note, note: '', handler: '', handlerMs: '', planned: true })) });

// config & gates + fachada (arquivos não-lib que os milestones criam/ativam) — derivado dos deltas
const cfgMap = new Map();
const CFG_BLURB = {
  'scripts/prevcarga': 'fachada docopt (R-only): dispatcher fino dos verbos; cresce a cada milestone',
  'config/topology.yaml': 'árvore SIN (folhas → subsistemas + perdas) + 8 membros — fonte única (ADR-0001)',
  'models/<plugin>/run.yaml': 'contrato por-modelo: horizon/history/protocol/seed/params (ADR-0021) ×5 plugins',
  'models/<plugin>/tests/run_all_gates.R': 'gates falsificáveis por-modelo ×5 (M5 cria · M6 ativa sub-gates · M7 ativa harness RF/HW)',
};
for (const m of ORDER) for (const f of (deltas[m] || {}).files || []) {
  if (!/^(config\/|models\/|scripts\/prevcarga$)/.test(f.path)) continue;
  let key = f.path; if (/run_all_gates\.R$/.test(key)) key = 'models/<plugin>/tests/run_all_gates.R'; if (/run\.yaml$/.test(key)) key = 'models/<plugin>/run.yaml';
  const e = cfgMap.get(key) || { id: 'cfg:' + key, label: key.replace(/^models\/<plugin>\//, ''), path: key, kind: 'config', ms: m, touches: [], blurb: CFG_BLURB[key] || '' };
  if (!e.touches.some((t) => t.m === m)) e.touches.push({ m, action: f.action, note: f.note || '' });
  cfgMap.set(key, e);
}
const config = [...cfgMap.values()].sort((a, b) => (a.path === 'scripts/prevcarga' ? -1 : b.path === 'scripts/prevcarga' ? 1 : 0));

const VM = {
  order: ORDER,
  milestones: ORDER.map((id) => { const m = R.milestones.find((x) => x.id === id) || { id }; const d = deltas[id] || {}; return { id, name: msName(m), title: d.title || m.title || id, status: /VERIFY-READY/i.test(d.status || '') ? 'VERIFY-READY' : (m.status || d.status || ''), statusFull: d.status || m.status || '' }; }),
  verbGroups, flowTrain, flowDaily, engines, transversal, tools, config, edges: EDGES.map((e) => ({ from: e.from, to: e.to, kind: e.kind, label: e.label, ms: e.milestone })), deltas,
  totals: { verbs: [...nodeMap.values()].filter((n) => n.kind === 'verb').length, stages: flowTrain.length + flowDaily.length, engines: engines.length, transversal: transversal.length, tools: tools.length, functions: [...nodeMap.values()].filter((n) => n.kind === 'function').length, edges: EDGES.length, depends: EDGES.filter((e) => e.kind === 'depends-on').length, config: config.length },
};
const JSON_SAFE = JSON.stringify(VM).replace(/<\//g, '<\\/');

// ---------- HTML ----------
const html = `<!doctype html>
<html lang="pt-BR">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>prevcarga — topologia por milestone</title>
<meta name="description" content="Topologia interna do pacote R prevcarga (verbos → pipeline → transversais → config) com o delta de cada milestone em destaque.">
<style>
:root{
  color-scheme:light;
  --page:#f9f9f7; --surface:#fcfcfb; --ink:#0b0b0b; --ink-2:#52514e; --muted:#6f6e69; --grid:#e1e0d9; --axis:#c3c2b7; --ring:rgba(11,11,11,.10);
  --edge-faint:#c3c2b7; --edge-op:.26;
  --c-mod:#2a78d6; --c-verb:#eb6834; --c-trans:#1baf7a; --c-cfg:#898781;
  --font:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif; --mono:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
}
@media (prefers-color-scheme:dark){ :root:not([data-theme="light"]){ color-scheme:dark; --page:#0d0d0d; --surface:#1a1a19; --ink:#f0efec; --ink-2:#c3c2b7; --muted:#898781; --grid:#2c2c2a; --axis:#383835; --ring:rgba(255,255,255,.10); --edge-faint:#8a8984; --edge-op:.4; --c-mod:#3987e5; --c-verb:#d95926; --c-trans:#199e70; } }
:root[data-theme="dark"]{ color-scheme:dark; --page:#0d0d0d; --surface:#1a1a19; --ink:#f0efec; --ink-2:#c3c2b7; --muted:#898781; --grid:#2c2c2a; --axis:#383835; --ring:rgba(255,255,255,.10); --edge-faint:#8a8984; --edge-op:.4; --c-mod:#3987e5; --c-verb:#d95926; --c-trans:#199e70; }
*{box-sizing:border-box}
html,body{height:100%}
body{margin:0;background:var(--page);color:var(--ink);font:13px/1.45 var(--font);-webkit-font-smoothing:antialiased}
button,input{font:inherit;color:inherit}
button{cursor:pointer;background:none;border:1px solid var(--ring);border-radius:6px;padding:4px 10px}
button:hover{border-color:var(--axis)}
button:focus-visible,[tabindex]:focus-visible{outline:2px solid var(--ink);outline-offset:2px}
code{font-family:var(--mono);font-size:.95em}
#app{display:grid;grid-template-rows:auto auto 1fr;height:100vh}
/* ---- header ---- */
header{display:flex;flex-wrap:wrap;align-items:center;gap:10px 16px;padding:9px 16px;border-bottom:1px solid var(--grid);background:var(--surface)}
header h1{font-size:15px;font-weight:600;margin:0;letter-spacing:-.01em}
header h1 small{font-weight:400;color:var(--ink-2);margin-left:6px}
.seg{display:inline-flex;border:1px solid var(--ring);border-radius:8px;overflow:hidden}
.seg button{border:0;border-radius:0;padding:5px 11px;color:var(--ink-2);background:var(--surface)}
.seg button+button{border-left:1px solid var(--ring)}
.seg button[aria-pressed="true"]{background:var(--surface);background:color-mix(in oklab,var(--ink) 9%,var(--surface));color:var(--ink);font-weight:600;box-shadow:inset 0 -2px 0 var(--ink)}
.ms-steps button{display:flex;flex-direction:column;align-items:center;line-height:1.1;padding:4px 9px;min-width:46px}
.ms-steps button .k{font-weight:600;font-variant-numeric:tabular-nums}
.ms-steps button .n{font-size:9.5px;color:var(--muted);letter-spacing:.02em;margin-top:1px}
.ms-steps button[aria-pressed="true"] .n{color:var(--ink-2)}
.legend{display:flex;gap:13px;align-items:center;color:var(--ink-2);font-size:12px;margin-left:auto;flex-wrap:wrap}
.legend .sw{display:inline-block;width:10px;height:10px;border-radius:3px;margin-right:5px;vertical-align:-1px}
.legend .ln{display:inline-block;width:18px;height:0;border-top:3px solid var(--ink);margin-right:5px;vertical-align:3px}
.legend .ln.dep{border-top:1.5px dashed var(--ink-2)}
.legend .rg{display:inline-block;width:10px;height:10px;border:2px solid var(--ink);border-radius:3px;margin-right:5px;vertical-align:-1px}
#search{border:1px solid var(--ring);border-radius:6px;padding:5px 9px;background:var(--surface);min-width:190px}
.sub{display:flex;gap:14px;align-items:center;padding:6px 16px;border-bottom:1px solid var(--grid);background:var(--surface);color:var(--ink-2);font-size:12px}
/* ---- main ---- */
main{display:grid;grid-template-columns:1fr 340px;min-height:0}
#boardwrap{overflow:auto;position:relative;background:var(--page)}
#view-matrix,#view-table{display:none;padding:18px 20px;overflow:auto}
#view-graph{display:none;position:relative;height:100%;overflow:hidden;background:var(--page)}
#gsvg{width:100%;height:100%;display:block;cursor:grab;touch-action:none}
#gsvg.panning{cursor:grabbing}
.gctl{position:absolute;top:10px;left:12px;display:flex;gap:8px;flex-wrap:wrap;z-index:2;font-size:12px;align-items:center}
.gctl .seg button,.gctl>button{padding:4px 9px;background:var(--surface)}
.ghint{color:var(--muted);font-size:11px;background:var(--page);padding:2px 6px;border-radius:4px}
.ge{stroke:var(--edge-faint);stroke-width:1;opacity:.6;transition:opacity .15s}
.ge.contains{stroke-width:.7;opacity:.55}
.ge.composes{stroke-width:1.6}
.ge.depends{stroke-dasharray:4 3}
.ge.past{opacity:.16}
.ge.is-delta{stroke:var(--ink-2);opacity:.95;stroke-width:1.6}
.ge.hi{stroke:var(--ink);opacity:1;stroke-width:2.2}
.ge.hi.contains{stroke-width:1.2}
.ge.dim{opacity:.05}
.ge.hidden{display:none}
.gn{fill:var(--k);stroke:var(--surface);stroke-width:1.5;cursor:pointer;transition:opacity .15s}
.gn.function{opacity:.8}
.gn.is-past{opacity:.28}
.gn.is-future{opacity:.12;stroke-dasharray:2 2}
.gn.is-dim{opacity:.09}
.gn.is-delta{stroke:var(--ink);stroke-width:3;opacity:1}
.gn.is-touch{stroke:var(--ink);stroke-width:2;stroke-dasharray:3 2;opacity:1}
.gn.is-focus{stroke:var(--ink);stroke-width:3.5;opacity:1}
.gn.is-nb{opacity:1}
.gl{font:11px var(--mono);fill:var(--ink);paint-order:stroke;stroke:var(--page);stroke-width:3px;stroke-linejoin:round;pointer-events:none}
.gl.verb{font-weight:600}
.gl.function{font-size:9.5px;fill:var(--ink-2);display:none}
#gsvg.zoomed .gl.function,.gl.function.on{display:block}
.gl.off{opacity:.22}
.gmk path{fill:var(--edge-faint)} .gmk-mid path{fill:var(--ink-2)} .gmk-ink path{fill:var(--ink)}
.ge{fill:none}
.gr{fill:var(--k);fill-opacity:.05;stroke:var(--k);stroke-opacity:.5;stroke-width:1.2;pointer-events:all;cursor:default}
.gr.sub{fill-opacity:.04;stroke-opacity:.35;stroke-dasharray:3 3}
.gn.ring{fill:var(--k);fill-opacity:.10;stroke:var(--k);stroke-opacity:.9;stroke-width:1.4}
.gn.ring.is-delta,.gn.ring.is-focus{stroke:var(--ink)}
.gn.ring.is-past{opacity:.35} .gn.ring.is-dim{opacity:.1}
.grl{font:600 10px var(--font);letter-spacing:.08em;text-transform:uppercase;fill:var(--ink-2);text-anchor:middle;pointer-events:none;paint-order:stroke;stroke:var(--page);stroke-width:3px}
.grl.sub{font-size:8.5px;fill:var(--muted);letter-spacing:.06em}
.gl.ringlab{text-anchor:middle;font-weight:600;font-size:9.5px}
#gsvg.pack .gl.verb{font-weight:500;font-size:9.5px}
#gsvg.pack .gl.config{font-size:9.5px}
.gclab{font:700 11px var(--font);letter-spacing:.1em;text-transform:uppercase;fill:var(--muted);text-anchor:middle;pointer-events:none;paint-order:stroke;stroke:var(--page);stroke-width:4px}
.gzoom{position:absolute;right:12px;bottom:12px;display:flex;flex-direction:column;gap:4px;z-index:2}
.gzoom button{width:30px;height:30px;padding:0;background:var(--surface);font-size:15px;line-height:1}
@media (prefers-color-scheme:dark){ :root:not([data-theme="light"]) .gn:not(.ring){filter:drop-shadow(0 0 3px var(--k))} :root:not([data-theme="light"]) .gn.ring,:root:not([data-theme="light"]) .gr{filter:drop-shadow(0 0 7px var(--k))} :root:not([data-theme="light"]) .ge.hi,:root:not([data-theme="light"]) .ge.is-delta{filter:drop-shadow(0 0 2px var(--ink-2))} }
:root[data-theme="dark"] .gn:not(.ring){filter:drop-shadow(0 0 3px var(--k))} :root[data-theme="dark"] .gn.ring,:root[data-theme="dark"] .gr{filter:drop-shadow(0 0 7px var(--k))} :root[data-theme="dark"] .ge.hi,:root[data-theme="dark"] .ge.is-delta{filter:drop-shadow(0 0 2px var(--ink-2))}
:root[data-theme="light"] .gn,:root[data-theme="light"] .gr,:root[data-theme="light"] .ge{filter:none}
.board{position:relative;min-width:980px;padding:18px 18px 30px;display:flex;flex-direction:column;gap:56px}
.band{position:relative}
.band-h{display:flex;align-items:baseline;gap:8px;margin:0 0 10px 0;position:relative;z-index:3;flex-wrap:wrap}
.band-h h2,.band-h .cap{background:var(--page);padding:2px 6px;border-radius:4px}
.band-h h2{font-size:11px;letter-spacing:.08em;text-transform:uppercase;color:var(--ink-2);margin:0;font-weight:700}
.band-h h2 .sw{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:7px;vertical-align:0}
.band-h .cap{color:var(--muted);font-size:11.5px}
.band-h .cap code{color:var(--ink-2);font-size:11px}
.groups{display:flex;gap:10px 28px;flex-wrap:wrap;align-items:flex-start}
.group>.gl{font-size:10.5px;color:var(--muted);text-transform:uppercase;letter-spacing:.06em;margin:0 0 6px 2px;position:relative;z-index:3}
.group>.gl span{background:var(--page);padding:1px 4px;border-radius:3px}
.cards{display:flex;gap:12px;flex-wrap:wrap;align-items:flex-start}
.flowrow{display:flex;gap:0 14px;align-items:flex-start;flex-wrap:nowrap}
.flow{display:flex;flex-direction:column}
.flow .cards{gap:0;flex-wrap:nowrap;align-items:stretch}
.flow.train{flex:none}
.flow.train .card{flex:0 0 150px}
.flow.daily{flex:1 1 0;min-width:0}
.flow.daily .card{flex:1 1 0;min-width:128px;max-width:196px}
.flowsep{align-self:center;width:26px;text-align:center;color:var(--muted);font-size:18px;line-height:1;margin-top:22px;user-select:none}
.engines{display:flex;gap:12px;justify-content:flex-end;align-items:center;margin-top:14px;flex-wrap:wrap}
.engines .ol{font-size:10.5px;color:var(--muted);text-transform:uppercase;letter-spacing:.06em;margin-right:4px;max-width:260px;text-align:right}
.chev{flex:none;align-self:center;width:22px;text-align:center;color:var(--c-mod);font-size:20px;line-height:1;user-select:none;font-weight:700}
/* ---- card ---- */
.card{position:relative;z-index:0;background:var(--surface);border:1px solid var(--ring);border-left:4px solid var(--k);border-radius:8px;width:168px;padding:8px 9px 9px 10px;transition:opacity .18s,filter .18s,box-shadow .18s;cursor:default}
.card.verb{width:auto;padding:6px 9px 6px 9px}
.card.verb .ch{gap:8px;align-items:center}
.card.verb .nm{white-space:nowrap;font-size:11.5px;padding-right:0}
.card.verb .bl,.card.verb .hnd,.card.verb .st{display:none}
.card.verb .badge{position:static}
.card.verb.planned{border-style:dashed;opacity:.45;filter:saturate(0)}
.card.config{width:236px}
.card.tool{border-left-style:dashed}
.legend .sw.tool{background:transparent;border:1.5px dashed var(--c-cfg);box-sizing:border-box}
.row .mono.rm{text-decoration:line-through;opacity:.7}
.card:hover{box-shadow:0 1px 0 var(--ring),0 6px 18px -8px rgba(0,0,0,.35)}
.card .ch{display:flex;align-items:flex-start;gap:6px}
.card .num{flex:none;font:700 10px/1 var(--font);color:var(--surface);background:var(--c-mod);border-radius:4px;width:16px;height:16px;display:inline-flex;align-items:center;justify-content:center;margin-top:1px}
.card .nm{font-family:var(--mono);font-size:11.5px;font-weight:600;color:var(--ink);letter-spacing:-.01em;overflow-wrap:normal;word-break:normal;line-height:1.25;padding-right:30px}
.card.verb .nm::before{content:"$ ";color:var(--muted);font-weight:400}
.badge{position:absolute;top:7px;right:7px;font:600 9.5px/1 var(--font);letter-spacing:.03em;color:var(--ink-2);border:1px solid var(--ring);border-radius:4px;padding:3px 5px;background:var(--surface);background:color-mix(in oklab,var(--k) 9%,var(--surface))}
.card .st{font-size:10.5px;color:var(--muted);margin-top:3px}
.card .bl{font-size:11px;color:var(--ink-2);margin-top:4px;line-height:1.35;display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden}
.card.open .bl{display:block}
.fns{display:flex;flex-wrap:wrap;gap:4px;margin-top:7px}
.fn{font:10.5px/1.3 var(--mono);color:var(--ink-2);background:var(--surface);background:color-mix(in oklab,var(--ink) 4%,var(--surface));border:1px solid var(--ring);border-radius:4px;padding:1px 5px;white-space:nowrap;max-width:100%;overflow:hidden;text-overflow:ellipsis}
.fn.hit{border-color:var(--ink);color:var(--ink);font-weight:600}
button.fn.more{cursor:pointer;color:var(--muted);border-style:dashed;padding:1px 6px}
button.fn.more:hover{color:var(--ink)}
.card:not(.open) .fn.hid{display:none}
.touch{display:flex;gap:4px;flex-wrap:wrap;margin-top:7px}
.touch span{font:600 9.5px/1 var(--font);padding:3px 5px;border-radius:4px;border:1px solid var(--ring);color:var(--ink-2);background:var(--surface);background:color-mix(in oklab,var(--ink) 4%,var(--surface))}
.touch span i{font-style:normal;color:var(--muted);font-weight:400;margin-left:3px}
/* ---- estados ---- */
.card.is-delta{box-shadow:0 0 0 2px var(--ink);z-index:2}
.card.is-touch{outline:2px dashed var(--ink);outline-offset:1px;z-index:2}
.card.is-past{opacity:.42;filter:saturate(.25)}
.card.is-future{opacity:.16;border-style:dashed;filter:saturate(0)}
.card.is-dim{opacity:.14;filter:saturate(0)}
.card.is-nb{opacity:1!important;filter:none!important;z-index:2}
.card.is-focus{opacity:1!important;filter:none!important;border-style:solid;box-shadow:0 0 0 2px var(--ink),0 8px 24px -8px rgba(0,0,0,.4);z-index:2}
/* ---- edges ---- */
#edges{position:absolute;inset:0;width:100%;height:100%;pointer-events:none;overflow:visible;z-index:0}
.board.focusing #edges{z-index:1}
#edges path.e{fill:none;stroke:var(--edge-faint);stroke-width:1.1;opacity:0;transition:opacity .15s}
#edges path.e.depends{stroke-dasharray:5 3}
#edges path.e.show{opacity:var(--edge-op)}
#edges path.e.is-delta{stroke:var(--ink-2);opacity:.6;stroke-width:1.3}
#edges path.e.hi{stroke:var(--ink);opacity:1}
#edges path.e.hi.composes{stroke-width:3}
#edges path.e.hi.depends{stroke-width:1.6}
#edges path.hit{fill:none;stroke:transparent;stroke-width:14;pointer-events:none}
#edges path.hit.on{pointer-events:stroke}
#edges .mk-faint path{fill:var(--edge-faint)} #edges .mk-mid path{fill:var(--ink-2)} #edges .mk-ink path{fill:var(--ink)}
/* ---- aside ---- */
aside{border-left:1px solid var(--grid);background:var(--surface);overflow:auto;padding:16px 16px 28px}
aside h2{font-size:15px;margin:0 0 4px;letter-spacing:-.01em}
aside h2 code,aside .intent code{font-size:.92em;background:var(--page);padding:0 3px;border-radius:3px}
aside .intent{color:var(--ink-2);font-size:12px;margin:0 0 12px}
aside .status{display:inline-block;font-size:11px;color:var(--ink-2);border:1px solid var(--ring);border-radius:5px;padding:2px 7px;margin:0 0 12px}
aside .status::before{content:"✓ ";font-weight:700}
aside h3{font-size:10.5px;text-transform:uppercase;letter-spacing:.07em;color:var(--muted);margin:16px 0 6px;padding-bottom:4px;border-bottom:1px solid var(--grid)}
.tiles{display:grid;grid-template-columns:repeat(4,1fr);gap:8px}
.tile{border:1px solid var(--ring);border-radius:8px;padding:8px 9px;background:var(--page)}
.tile .v{font-size:22px;font-weight:600;letter-spacing:-.02em;line-height:1.1}
.tile .v small{font-size:11px;font-weight:500;color:var(--muted);margin-left:3px}
.tile .l{font-size:10.5px;color:var(--ink-2);margin-top:2px}
.row{display:flex;gap:7px;align-items:baseline;margin:3px 0;font-size:12px}
.act{font:700 9px/1 var(--font);letter-spacing:.04em;padding:3px 5px;border-radius:4px;flex:none;border:1px solid var(--ring);color:var(--ink-2)}
.act.cond{border-style:dashed}
.mono{font-family:var(--mono);font-size:11px;color:var(--ink)}
.mono.k{color:var(--ink-2)}
.mono.fn{font-size:11px;background:none;border:0;padding:0;white-space:normal}
.cond-note{color:var(--muted);font-size:10.5px}
aside ul{margin:4px 0 0;padding-left:16px}
aside li{margin:3px 0;font-size:12px;color:var(--ink-2)}
.bars{display:grid;grid-template-columns:auto 1fr 30px;gap:6px 8px;align-items:center;font-size:11.5px}
.bars .lb{color:var(--ink-2);font-variant-numeric:tabular-nums;border:0;padding:0;text-align:left;background:none}
.bars .lb b{font-weight:600;color:var(--ink)} .bars .lb span{color:var(--muted);margin-left:5px}
.bars .tr{height:16px;position:relative}
.bars .tr i{position:absolute;left:0;top:0;height:16px;background:var(--ink-2);border-radius:0 4px 4px 0;display:block}
.bars .vv{color:var(--ink-2);font-variant-numeric:tabular-nums;text-align:right}
.detail{border:1px solid var(--ring);border-radius:8px;padding:10px 11px;margin:0 0 14px;background:var(--page);position:relative}
.detail .x{position:absolute;right:8px;top:8px;border:0;color:var(--muted);padding:2px 6px}
.detail .kd{font-size:10.5px;color:var(--muted);text-transform:uppercase;letter-spacing:.06em}
.detail .kd .sw{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:5px;vertical-align:-1px}
.detail .nm{font-family:var(--mono);font-size:13px;font-weight:600;margin:3px 0 4px}
.detail p{margin:4px 0;color:var(--ink-2);font-size:12px}
.more-files{border:0;padding:0;color:var(--muted);font-size:11.5px;text-decoration:underline dotted;margin-top:4px}
.hint{color:var(--muted);font-size:11.5px;margin:8px 0 0}
kbd{font:11px var(--mono);border:1px solid var(--ring);border-bottom-width:2px;border-radius:4px;padding:0 4px;color:var(--ink-2)}
/* ---- tooltip ---- */
.tip{position:fixed;z-index:50;pointer-events:none;background:var(--ink);color:var(--surface);border-radius:6px;padding:7px 9px;font-size:11.5px;line-height:1.35;max-width:300px;box-shadow:0 8px 24px -8px rgba(0,0,0,.5);display:none}
.tip b{font-weight:600}
.tip .mt{opacity:.72;margin-top:3px}
/* ---- matriz ---- */
.mx{border-collapse:separate;border-spacing:0;font:11px var(--mono)}
.mx th{font-weight:500;color:var(--ink-2);text-align:left;padding:3px 6px;white-space:nowrap}
.mx thead th{writing-mode:vertical-rl;transform:rotate(180deg);text-align:left;vertical-align:bottom;padding:6px 0;height:150px}
.mx td{width:22px;height:22px;text-align:center;border-top:1px solid var(--grid);border-left:1px solid var(--grid);padding:0}
.mx td button{width:22px;height:22px;border:0;border-radius:0;padding:0;display:block}
.mx td button i{display:block;width:12px;height:12px;margin:5px;border-radius:3px;background:var(--ink-2)}
.mx td button:hover{background:var(--grid)}
.mx tr.is-off th,.mx tr.is-off td button i{opacity:.18}
.mx .diag{background:var(--grid)}
.mx tr th.rk{border-left:3px solid var(--k);padding-left:7px}
.mx thead th.ck{border-bottom:3px solid var(--k)}
.mxnote{color:var(--ink-2);font-size:12px;margin:0 0 12px;max-width:760px}
/* ---- tabela ---- */
.tbl{border-collapse:collapse;font-size:12px;min-width:720px;margin-bottom:22px}
.tbl th{text-align:left;color:var(--ink-2);font-weight:600;border-bottom:1px solid var(--axis);padding:6px 8px;white-space:nowrap}
.tbl td{border-bottom:1px solid var(--grid);padding:5px 8px;vertical-align:top}
.tbl td.m{font-family:var(--mono);font-size:11.5px;white-space:nowrap}
.tbl tr.is-off td{opacity:.35}
.tbl caption{text-align:left;font-weight:600;color:var(--ink);padding:0 0 8px;font-size:13px}
@media (max-width:1000px){ main{grid-template-columns:1fr} aside{border-left:0;border-top:1px solid var(--grid)} #app{grid-template-rows:auto auto auto auto;height:auto} .tiles{grid-template-columns:repeat(2,1fr)} }
@media print{ aside,header .legend,#search{display:none} .card{break-inside:avoid} }
</style>
</head>
<body>
<div id="app">
  <header>
    <h1>prevcarga <small>topologia interna do pacote · M1→${ORDER[ORDER.length - 1]}</small></h1>
    <div class="seg ms-steps" id="steps" role="group" aria-label="Milestone"></div>
    <div class="seg" id="mode" role="group" aria-label="Modo do milestone"><button data-v="delta" aria-pressed="true">Delta</button><button data-v="acc" aria-pressed="false">Acumulado</button></div>
    <div class="seg" id="emode" role="group" aria-label="Arestas"><button data-v="focus" aria-pressed="true">Arestas: foco</button><button data-v="all" aria-pressed="false">todas</button><button data-v="none" aria-pressed="false">nenhuma</button></div>
    <input id="search" type="search" placeholder="filtrar função/módulo/verbo…" aria-label="Filtrar">
    <div class="legend" aria-hidden="true">
      <span><span class="sw" style="background:var(--c-verb)"></span>verbo CLI</span>
      <span><span class="sw" style="background:var(--c-mod)"></span>módulo de pipeline</span>
      <span><span class="sw" style="background:var(--c-trans)"></span>transversal</span>
      <span><span class="sw" style="background:var(--c-cfg)"></span>config/gates</span>
      <span><span class="sw tool"></span>ferramenta scripts/tools/ (fora do runtime)</span>
      <span><span class="ln"></span>compõe</span>
      <span><span class="ln dep"></span>depende de</span>
      <span><span class="rg"></span>entra neste milestone</span>
    </div>
    <button id="theme" aria-label="Tema: sistema">◐ tema</button>
  </header>
  <div class="sub">
    <div class="seg" id="tabs" role="group" aria-label="Vista"><button data-v="map" aria-pressed="true">Mapa</button><button data-v="graph" aria-pressed="false">Grafo</button><button data-v="matrix" aria-pressed="false">Matriz de dependências</button><button data-v="table" aria-pressed="false">Tabela</button></div>
    <span id="subtxt"></span>
  </div>
  <main>
    <div id="boardwrap">
      <div class="board" id="board">
        <svg id="edges" aria-hidden="true">
          <defs>
            <marker id="mk-faint" class="mk-faint" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
            <marker id="mk-mid" class="mk-mid" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
            <marker id="mk-ink" class="mk-ink" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
          </defs>
          <g id="eg"></g>
        </svg>
      </div>
      <div id="view-graph">
        <div class="gctl">
          <div class="seg" id="gfn" role="group" aria-label="Funções no grafo"><button data-v="1" aria-pressed="true">Funções: mostrar</button><button data-v="0" aria-pressed="false">ocultar</button></div>
          <div class="seg" id="glay" role="group" aria-label="Layout do grafo"><button data-v="pack" aria-pressed="true">Layout: bolhas</button><button data-v="layers" aria-pressed="false">camadas</button><button data-v="free" aria-pressed="false">livre</button></div>
          <button id="greset" type="button">↻ reiniciar</button><button id="gfit" type="button">⤢ ajustar</button>
          <span class="ghint">roda = zoom · arrastar fundo = pan · hover foca vizinhos · clique abre o detalhe no painel · Esc solta</span>
        </div>
        <svg id="gsvg" aria-label="Grafo da topologia interna"><defs>
          <marker id="gmk" class="gmk" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
          <marker id="gmk-mid" class="gmk-mid" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
          <marker id="gmk-ink" class="gmk-ink" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0.5 L9 5 L0 9.5 z"/></marker>
        </defs><g id="g-rings"></g><g id="g-edges"></g><g id="g-nodes"></g><g id="g-labels"></g></svg>
        <div class="gzoom"><button id="gzin" type="button" aria-label="aproximar">+</button><button id="gzout" type="button" aria-label="afastar">−</button><button id="gzfit" type="button" aria-label="ajustar à tela">⤢</button></div>
      </div>
      <div id="view-matrix"></div>
      <div id="view-table"></div>
    </div>
    <aside id="aside"></aside>
  </main>
</div>
<div class="tip" id="tip" role="tooltip"></div>
<script id="data" type="application/json">${JSON_SAFE}</script>
<script>
(function(){
'use strict';
var VM = JSON.parse(document.getElementById('data').textContent);
var ORDER = VM.order; var rank = function(m){ var i = ORDER.indexOf(m); return i < 0 ? 999 : i; };
var MSN = {}; VM.milestones.forEach(function(m){ MSN[m.id] = m; });
var KCOL = { module:'var(--c-mod)', verb:'var(--c-verb)', transversal:'var(--c-trans)', config:'var(--c-cfg)', tool:'var(--c-cfg)' };
var KNAME = { module:'módulo de pipeline', verb:'verbo CLI', transversal:'transversal', config:'config / gates', tool:'ferramenta (scripts/tools/, fora do runtime)' };
var BAND = {}, NODES = {}, cards = {};
var state = { sel:'ALL', mode:'delta', emode:'focus', tab:'map', pin:null, hover:null, q:'', g:'pack', fn:true };
var touchedBy = {}; ORDER.forEach(function(m){ touchedBy[m] = {}; ((VM.deltas[m]||{}).verbs||[]).forEach(function(v){ touchedBy[m][v.name] = true; }); });
var isCode = function(p){ return /^(scripts\\/|models\\/|config\\/|tests\\/)/.test(p); };
var isCond = function(f){ return /s[oó] se|condicional/i.test(f.note||''); };

// ---------- helpers ----------
function el(tag, cls, text){ var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
function rich(node, text){ String(text==null?'':text).split('\`').forEach(function(p,i){ if (!p) return; if (i%2===1){ var c = document.createElement('code'); c.textContent = p; node.appendChild(c); } else node.appendChild(document.createTextNode(p)); }); return node; }
function sw(color){ var s = el('span','sw'); s.style.background = color; return s; }
function msOf(id){ return NODES[id] ? NODES[id].ms : ''; }
function readHash(){ var h = location.hash.replace(/^#/,''); if (!h) return; h.split('&').forEach(function(kv){ var p = kv.split('='); var k = decodeURIComponent(p[0]||''), v = decodeURIComponent(p[1]||''); if (k==='m' && (v==='ALL'||ORDER.indexOf(v)>=0)) state.sel = v; if (k==='mode' && (v==='delta'||v==='acc')) state.mode = v; if (k==='edges' && /^(focus|all|none)$/.test(v)) state.emode = v; if (k==='tab' && /^(map|graph|matrix|table)$/.test(v)) state.tab = v; if (k==='g' && /^(pack|layers|free)$/.test(v)) state.g = v; if (k==='fn' && /^(0|1)$/.test(v)) state.fn = v==='1'; if (k==='pin') state.pin = v; if (k==='theme' && /^(light|dark)$/.test(v)) document.documentElement.setAttribute('data-theme', v); }); }
function writeHash(){ var parts = ['m='+state.sel,'mode='+state.mode,'edges='+state.emode,'tab='+state.tab]; if (state.tab==='graph'){ parts.push('g='+state.g); parts.push('fn='+(state.fn?'1':'0')); } if (state.pin) parts.push('pin='+encodeURIComponent(state.pin)); var t = document.documentElement.getAttribute('data-theme'); if (t) parts.push('theme='+t); history.replaceState(null,'','#'+parts.join('&')); }
function inDelta(n, m){ if (n.planned) return false; if (n.kind==='config') return n.touches.some(function(t){ return t.m===m; }); return n.ms===m; }
function isTouched(n, m){ return n.kind==='verb' && !n.planned && n.ms!==m && !!touchedBy[m] && !!touchedBy[m][n.label]; }

// ---------- cards ----------
var board = document.getElementById('board');
function setOpen(c, on){ c.classList.toggle('open', on); var mo = c.querySelector('.more'); if (mo) mo.textContent = on ? 'menos' : '+'+mo.dataset.n; }
function mkCard(n, kind){
  var c = el('div','card '+kind+(n.planned?' planned':'')); c.dataset.id = n.id; c.style.setProperty('--k', KCOL[kind]); c.tabIndex = 0; c.setAttribute('role','button'); c.setAttribute('aria-pressed','false');
  NODES[n.id] = n; n.kind = kind;
  var h = el('div','ch'); if (n.step) h.appendChild(el('span','num', String(n.step)));
  var nm = el('span','nm'); var disp = (kind==='module'||kind==='transversal'||kind==='tool') ? n.label.replace(/\\.R$/,'') : n.label; disp.split('_').forEach(function(part,i,arr){ nm.appendChild(document.createTextNode(part + (i<arr.length-1?'_':''))); if (i<arr.length-1) nm.appendChild(document.createElement('wbr')); });
  nm.title = kind==='tool' ? 'scripts/tools/'+n.label+' · ferramenta fora do runtime (tier tools/)' : (kind==='module'||kind==='transversal') ? 'scripts/lib/'+n.label + (n.ref? ' · '+n.ref+' do CLI-MODULAR-OBJETIVO':'') : (n.planned ? n.label+' · planejado (M8), ainda não expandido' : n.label);
  h.appendChild(nm); h.appendChild(el('span','badge', n.ms)); c.appendChild(h);
  if (n.st) c.appendChild(el('div','st', n.st));
  var bl = n.tag || n.blurb; if (bl) c.appendChild(el('div','bl', bl));
  if (n.fns && n.fns.length){ var fw = el('div','fns'); var MAX = 5; n.fns.forEach(function(f,i){ var ch = el('span','fn'+(i>=MAX?' hid':''), f.label.replace(/^prevcarga_/,'')); ch.title = f.label + (f.note? ' — '+f.note:'') + (f.ms? ' · '+f.ms:''); ch.dataset.fn = f.label.toLowerCase(); fw.appendChild(ch); });
    if (n.fns.length>MAX){ var mo = el('button','fn more', '+'+(n.fns.length-MAX)); mo.type='button'; mo.dataset.n = String(n.fns.length-MAX); mo.setAttribute('aria-label','mostrar todas as funções'); mo.addEventListener('click', function(ev){ ev.stopPropagation(); setOpen(c, !c.classList.contains('open')); delete c.dataset.auto; layoutEdges(); }); mo.addEventListener('keydown', function(ev){ ev.stopPropagation(); }); fw.appendChild(mo); }
    c.appendChild(fw); }
  if (kind==='config'){ var tw = el('div','touch'); n.touches.forEach(function(t){ var s = el('span', null, t.m); var i = el('i', null, t.action.toLowerCase()); s.appendChild(i); s.title = t.note || ''; tw.appendChild(s); }); c.appendChild(tw); }
  c.addEventListener('mouseenter', function(ev){ hoverNode(n.id, ev); });
  c.addEventListener('mousemove', function(ev){ if (ev.target.closest('.fn')) { hideTip(); } else if (tip.style.display!=='block') { showTip(ev, tipNode(n)); } else moveTip(ev); });
  c.addEventListener('mouseleave', function(){ hoverNode(null); });
  c.addEventListener('focus', function(){ hoverNode(n.id, null); });
  c.addEventListener('blur', function(){ hoverNode(null); });
  c.addEventListener('click', function(ev){ ev.stopPropagation(); pinNode(state.pin===n.id ? null : n.id); });
  c.addEventListener('keydown', function(ev){ if (ev.key==='Enter'||ev.key===' '){ ev.preventDefault(); pinNode(state.pin===n.id ? null : n.id); } });
  cards[n.id] = c; return c;
}
function band(title, color, capNode, bodyNode, idx){ var b = el('section','band'); var h = el('div','band-h'); var h2 = el('h2'); h2.appendChild(sw(color)); h2.appendChild(document.createTextNode(title)); h.appendChild(h2); if (capNode) h.appendChild(capNode); b.appendChild(h); b.appendChild(bodyNode); b.dataset.band = idx; return b; }
function cap(text){ return el('span','cap', text); }
function groupLabel(text){ var g = el('div','gl'); g.appendChild(el('span', null, text)); return g; }

// faixa 1 — verbos
(function(){
  var g = el('div','groups');
  VM.verbGroups.forEach(function(grp){ if (!grp.verbs.length) return; var gd = el('div','group'); gd.appendChild(groupLabel(grp.label)); var cs = el('div','cards'); grp.verbs.forEach(function(v){ BAND[v.id]=0; cs.appendChild(mkCard(v,'verb')); }); gd.appendChild(cs); g.appendChild(gd); });
  board.appendChild(band('Verbos CLI', KCOL.verb, cap(VM.totals.verbs+' verbos · prevcarga <verbo> · agrupados por papel · fachada docopt scripts/prevcarga'), g, 0));
})();
// faixa 2 — pipeline: treino ⫽ diário (etapas 1…7) + motores dos verbos
(function(){
  var wrap = el('div'); var row = el('div','flowrow');
  function flow(list, label, extraCls){ var f = el('div','flow'+(extraCls?' '+extraCls:'')); f.appendChild(groupLabel(label)); var cs = el('div','cards'); list.forEach(function(m,i){ BAND[m.id]=1; if (i>0) cs.appendChild(el('span','chev','›')); cs.appendChild(mkCard(m,'module')); }); f.appendChild(cs); return f; }
  row.appendChild(flow(VM.flowTrain, 'treino · train-models', 'train'));
  var sep = el('span','flowsep','⫽'); sep.title = 'o ciclo diário (forecast) NÃO treina: aplica pesos/artefatos congelados'; row.appendChild(sep);
  row.appendChild(flow(VM.flowDaily, 'ciclo diário · forecast (apply, nunca fit)', 'daily'));
  wrap.appendChild(row);
  if (VM.engines.length){ var en = el('div','engines'); en.appendChild(el('span','ol','motores dos verbos '+VM.engines.map(function(m){ return m.label.replace(/\\.R$/,''); }).join(' / ')+' — chamam as etapas acima ↑')); VM.engines.forEach(function(m){ BAND[m.id]=1; en.appendChild(mkCard(m,'module')); }); wrap.appendChild(en); }
  board.appendChild(band('Módulos de pipeline', KCOL.module, cap('etapas 1…'+VM.totals.stages+' em ordem (› = próximo estágio); chips = funções (prefixo prevcarga_ omitido)'), wrap, 1));
})();
// faixa 3 — transversais
(function(){ var cs = el('div','cards'); VM.transversal.forEach(function(m){ BAND[m.id]=2; cs.appendChild(mkCard(m,'transversal')); }); board.appendChild(band('Transversais', KCOL.transversal, cap('fonte única, determinismo, proveniência, datas, avaliação, honestidade — consumidos por todos'), cs, 2)); })();
// faixa 4 — config, fachada & gates
(function(){ if (!VM.config.length && !(VM.tools||[]).length) return; var cs = el('div','cards'); VM.config.forEach(function(m){ BAND[m.id]=3; m.touches = m.touches||[]; cs.appendChild(mkCard(m,'config')); }); (VM.tools||[]).forEach(function(m){ BAND[m.id]=3; cs.appendChild(mkCard(m,'tool')); }); board.appendChild(band('Config, fachada, gates & ferramentas', KCOL.config, cap('arquivos fora de scripts/lib — config/gates que os milestones criam/ativam/estendem (quem toca o quê) + ferramentas scripts/tools/ (harness A/B, inspeção; fora do runtime)'), cs, 3)); })();

// ---------- edges ----------
var svg = document.getElementById('edges'), eg = document.getElementById('eg');
var EDGES = [];
VM.edges.forEach(function(e){ if (!cards[e.from] || !cards[e.to]) return; var p = document.createElementNS('http://www.w3.org/2000/svg','path'); p.setAttribute('class','e '+(e.kind==='composes'?'composes':'depends')); p.setAttribute('marker-end','url(#mk-faint)'); var hit = document.createElementNS('http://www.w3.org/2000/svg','path'); hit.setAttribute('class','hit'); eg.appendChild(p); eg.appendChild(hit); var rec = {e:e, vis:p, hit:hit}; EDGES.push(rec);
  hit.addEventListener('mouseenter', function(ev){ p.classList.add('hi'); p.setAttribute('marker-end','url(#mk-ink)'); showTip(ev, tipEdge(e)); });
  hit.addEventListener('mousemove', moveTip);
  hit.addEventListener('mouseleave', function(){ applyEdges(); hideTip(); });
  hit.addEventListener('click', function(ev){ ev.stopPropagation(); pinNode(e.from); });
});
function rectOf(id){ var r = cards[id].getBoundingClientRect(), b = board.getBoundingClientRect(); return { l:r.left-b.left+board.scrollLeft, t:r.top-b.top+board.scrollTop, w:r.width, h:r.height }; }
var clamp = function(v,lo,hi){ return Math.max(lo, Math.min(hi, v)); };
function layoutEdges(){
  if (state.tab!=='map') return;
  svg.setAttribute('width', board.scrollWidth); svg.setAttribute('height', board.scrollHeight);
  var rects = {}, rowKeys = {};
  Object.keys(cards).forEach(function(id){ var r = rectOf(id); rects[id] = r; var k = BAND[id]+':'+Math.round(r.t/12); var rw = rowKeys[k] || (rowKeys[k] = {band:BAND[id], top:r.t, bottom:r.t+r.h, ids:[]}); rw.ids.push(id); rw.top = Math.min(rw.top, r.t); rw.bottom = Math.max(rw.bottom, r.t+r.h); });
  var rowList = Object.keys(rowKeys).map(function(k){ return rowKeys[k]; }).sort(function(a,b){ return a.top-b.top; });
  var firstTop = {}, rowOf = {}; rowList.forEach(function(rw){ if (!(rw.band in firstTop)) firstTop[rw.band] = rw.top; rw.first = (firstTop[rw.band]===rw.top); rw.lane = 0; rw.ids.forEach(function(id){ rowOf[id] = rw; }); });
  // leque: o ponto de ancoragem desloca em direção ao outro extremo (até 30% da largura) para as setas não se empilharem no centro
  EDGES.forEach(function(rec){ var a = rects[rec.e.from], b = rects[rec.e.to], ra = rowOf[rec.e.from], rb = rowOf[rec.e.to]; var acx = a.l + a.w/2, bcx = b.l + b.w/2; var t = clamp((bcx-acx)/520, -1, 1); var ax = acx + t*a.w*0.3, bx = bcx - t*b.w*0.3, d;
    if (rb.top > ra.top + 4){ var y1 = a.t + a.h, y2 = b.t; var dy = Math.max(24, (y2-y1)*0.5); d = 'M'+ax+' '+y1+' C'+ax+' '+(y1+dy)+' '+bx+' '+(y2-dy)+' '+bx+' '+(y2-2); }
    else if (rb.top < ra.top - 4){ var y1b = a.t, y2b = b.t + b.h; var dyb = Math.max(24, (y1b-y2b)*0.5); d = 'M'+ax+' '+y1b+' C'+ax+' '+(y1b-dyb)+' '+bx+' '+(y2b+dyb)+' '+bx+' '+(y2b+2); }
    else { ra.lane++; var off = 14 + (ra.lane%5)*6; if (ra.first){ var yc = ra.top - off; d = 'M'+ax+' '+a.t+' C'+ax+' '+(yc-10)+' '+bx+' '+(yc-10)+' '+bx+' '+(b.t-2); } else { var yc2 = ra.bottom + off; d = 'M'+ax+' '+(a.t+a.h)+' C'+ax+' '+(yc2+10)+' '+bx+' '+(yc2+10)+' '+bx+' '+(b.t+b.h+2); } }
    rec.vis.setAttribute('d', d); rec.hit.setAttribute('d', d); });
}
function edgeVisible(e){ var m = state.sel;
  if (state.emode==='none') return false;
  if (m!=='ALL' && state.mode==='acc' && (rank(msOf(e.from)) > rank(m) || rank(msOf(e.to)) > rank(m))) return false;
  if (m!=='ALL' && e.ms===m) return true;
  return state.emode==='all'; }
function applyEdges(){
  var focus = focusId();
  EDGES.forEach(function(rec){ var e = rec.e; var base = edgeVisible(e); var isDelta = state.sel!=='ALL' && e.ms===state.sel;
    var hi = false;
    if (focus && (e.from===focus || e.to===focus)){ var other = e.from===focus ? e.to : e.from; hi = (state.sel==='ALL' || state.mode!=='acc' || rank(msOf(other))<=rank(state.sel)); }
    rec.vis.classList.toggle('show', base && !focus);
    rec.vis.classList.toggle('is-delta', base && isDelta && !focus);
    rec.vis.classList.toggle('hi', hi);
    rec.vis.setAttribute('marker-end', hi ? 'url(#mk-ink)' : (base && isDelta && !focus) ? 'url(#mk-mid)' : 'url(#mk-faint)');
    rec.hit.classList.toggle('on', (base && !focus) || hi);
  });
  board.classList.toggle('focusing', !!focus || state.sel!=='ALL');
}

// ---------- estados ----------
function focusId(){ if (state.pin && cards[state.pin]) return state.pin; if (state.pin && !cards[state.pin]) state.pin = null; return state.hover; }
function applyNodes(){
  var focus = focusId(); var nb = {}; if (focus){ nb[focus]=1; VM.edges.forEach(function(e){ if (e.from===focus) nb[e.to]=1; if (e.to===focus) nb[e.from]=1; }); }
  var q = state.q;
  Object.keys(cards).forEach(function(id){ var c = cards[id], n = NODES[id]; c.classList.remove('is-delta','is-touch','is-past','is-future','is-dim','is-focus','is-nb'); c.setAttribute('aria-pressed', String(state.pin===id));
    if (n.planned) c.classList.add('is-future');
    else if (state.sel!=='ALL'){ var del = inDelta(n, state.sel), tch = isTouched(n, state.sel), cmp = rank(n.ms) - rank(state.sel);
      if (state.mode==='delta'){ if (del) c.classList.add('is-delta'); else if (tch) c.classList.add('is-touch'); else c.classList.add('is-past'); }
      else { if (cmp>0 && !del) c.classList.add('is-future'); else if (del) c.classList.add('is-delta'); else if (tch) c.classList.add('is-touch'); } }
    if (q){ var hay = (n.label+' '+(n.tag||'')+' '+(n.blurb||'')+' '+(n.fns||[]).map(function(f){return f.label;}).join(' ')+' '+(n.handler||'')).toLowerCase(); var hit = hay.indexOf(q)>=0; if (!hit) c.classList.add('is-dim'); else c.classList.add('is-nb');
      var any = false; c.querySelectorAll('.fn:not(.more)').forEach(function(ch){ var h2 = !!ch.dataset.fn && ch.dataset.fn.indexOf(q)>=0; ch.classList.toggle('hit', h2); if (h2) any = true; }); if (any && !c.classList.contains('open')){ setOpen(c, true); c.dataset.auto='1'; } }
    else { c.querySelectorAll('.fn.hit').forEach(function(ch){ ch.classList.remove('hit'); }); if (c.dataset.auto){ setOpen(c, false); delete c.dataset.auto; } }
    if (focus){ if (id===focus) c.classList.add('is-focus'); else if (nb[id]) c.classList.add('is-nb'); else c.classList.add('is-dim'); }
  });
  applyEdges(); applyTableState(); applyGraphState();
}
function hoverNode(id, ev){ state.hover = id; applyNodes(); if (id && ev) showTip(ev, tipNode(NODES[id])); if (!id) hideTip(); }
function pinNode(id){ state.pin = (id && cards[id]) ? id : null; applyNodes(); renderAside(); writeHash(); }
function setSel(m){ state.sel = m; document.querySelectorAll('#steps button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===m)); }); applyNodes(); renderAside(); renderMatrixState(); writeHash(); }
function setSeg(segId, key, v){ state[key] = v; document.querySelectorAll('#'+segId+' button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===v)); }); }

// ---------- tooltip ----------
var tip = document.getElementById('tip');
function tipNode(n){ var f = document.createDocumentFragment(); f.appendChild(el('b', null, n.label)); f.appendChild(document.createTextNode(' · '+KNAME[n.kind]+(n.planned? ' · planejado (M8)' : ' · entra em '+n.ms+(MSN[n.ms]? ' ('+MSN[n.ms].name+')':'')))); var bl = n.tag||n.blurb; if (bl) f.appendChild(el('div','mt', bl)); if (n.handler) f.appendChild(el('div','mt', 'handler '+n.handler)); var deg = VM.edges.filter(function(e){ return e.from===n.id||e.to===n.id; }).length; if (deg) f.appendChild(el('div','mt', deg+' ligaç'+(deg>1?'ões':'ão')+' · clique para fixar')); return f; }
function tipEdge(e){ var f = document.createDocumentFragment(); f.appendChild(el('b', null, NODES[e.from].label+' → '+NODES[e.to].label)); f.appendChild(el('div','mt', (e.kind==='composes'?'compõe':'depende de')+(e.label? ' · '+e.label:'')+' · '+e.ms)); return f; }
function showTip(ev, frag){ tip.textContent=''; tip.appendChild(frag); tip.style.display='block'; moveTip(ev); }
function moveTip(ev){ if (!ev || tip.style.display!=='block') return; var x = ev.clientX+14, y = ev.clientY+16; var r = tip.getBoundingClientRect(); if (x + r.width > innerWidth-8) x = ev.clientX - r.width - 10; if (y + r.height > innerHeight-8) y = ev.clientY - r.height - 10; tip.style.left = x+'px'; tip.style.top = y+'px'; }
function hideTip(){ tip.style.display='none'; }

// ---------- aside ----------
var aside = document.getElementById('aside');
function tile(v, l, small){ var t = el('div','tile'); var vv = el('div','v', String(v)); if (small) vv.appendChild(el('small', null, small)); t.appendChild(vv); t.appendChild(el('div','l', l)); return t; }
function fileRow(f){ var r = el('div','row'); var cond = isCond(f); var a = el('span','act'+(cond?' cond':''), f.action+(cond?'?':'')); a.title = cond ? 'condicional: '+(f.note||'') : (f.note||''); r.appendChild(a); var mn = el('span','mono', f.path); mn.title = f.note||''; r.appendChild(mn); if (cond) r.appendChild(el('span','cond-note','(condicional — ato do dono)')); return r; }
function renderAside(){
  aside.textContent='';
  if (state.pin && NODES[state.pin]) aside.appendChild(detailOf(NODES[state.pin]));
  if (state.sel==='ALL'){
    aside.appendChild(el('h2', null, 'Topologia completa'));
    aside.appendChild(el('p','intent','Verbos da CLI compõem módulos de pipeline, que dependem dos transversais. Escolha um milestone na barra para acender o que entra nele; passe o mouse num nó para ver suas ligações.'));
    var tl = el('div','tiles'); tl.appendChild(tile(VM.totals.verbs,'verbos CLI')); tl.appendChild(tile(VM.totals.stages, 'etapas de pipeline', '+'+VM.totals.engines+' motores')); tl.appendChild(tile(VM.totals.transversal,'transversais')); tl.appendChild(tile(VM.totals.functions,'funções (incl. handlers)')); aside.appendChild(tl);
    aside.appendChild(el('h3', null, 'O que entra por milestone'));
    var all = Object.values(NODES).filter(function(n){ return !n.planned; }); var counts = {}, maxc = 1;
    ORDER.forEach(function(m){ var c = all.filter(function(n){ return n.kind!=='config' && n.ms===m; }).length; all.forEach(function(n){ (n.fns||[]).forEach(function(f){ if (f.ms===m) c++; }); if (n.kind==='verb' && n.handler && n.handlerMs===m) c++; }); counts[m]=c; maxc = Math.max(maxc, c); });
    var bars = el('div','bars'); ORDER.forEach(function(m){ var lb = el('button','lb'); lb.appendChild(el('b', null, m)); lb.appendChild(el('span', null, MSN[m] ? MSN[m].name : '')); lb.title = MSN[m] ? MSN[m].title : m; lb.addEventListener('click', function(){ setSel(m); }); bars.appendChild(lb); var tr = el('div','tr'); var i = el('i'); i.style.width = Math.round(100*counts[m]/maxc)+'%'; tr.appendChild(i); bars.appendChild(tr); bars.appendChild(el('div','vv', String(counts[m]))); }); aside.appendChild(bars);
    aside.appendChild(el('p','hint','contagem = módulos + verbos + funções (incl. handlers) que o milestone introduz; config/gates à parte'));
    aside.appendChild(el('h3', null, 'Como ler'));
    var ul = el('ul'); ['Faixas = camadas: verbos → pipeline (treino ⫽ diário, etapas 1…'+VM.totals.stages+') → transversais → config. Setas descem quando um verbo compõe um módulo e quando um módulo depende de um transversal.','Delta: só os nós do milestone acendem (anel); verbos só editados ganham anel tracejado. Acumulado: o que ainda não existe fica fantasma.','Arestas: foco = só do nó sob o mouse/fixado (sólida = compõe, tracejada = depende de); todas = as '+VM.totals.edges+' em cinza claro.','Grafo = visão codegraph (nós + arestas, funções como satélites dos módulos): zoom na roda, pan e arrastar nós; o milestone e o filtro valem lá também.','Matriz = quem depende de quem (linha → coluna). Tabela = tudo em texto, com o mesmo filtro e milestone.'].forEach(function(s){ ul.appendChild(el('li', null, s)); }); aside.appendChild(ul);
    return;
  }
  var d = VM.deltas[state.sel] || {}; var m = state.sel; var MS = MSN[m] || {};
  aside.appendChild(rich(el('h2'), d.title || m));
  if (MS.status){ var sp = el('span','status', MS.status); sp.title = MS.statusFull || ''; aside.appendChild(sp); }
  if (d.intent) aside.appendChild(rich(el('p','intent'), d.intent));
  var newMods = Object.values(NODES).filter(function(n){ return (n.kind==='module'||n.kind==='transversal') && n.ms===m; }).length; var newTools = Object.values(NODES).filter(function(n){ return n.kind==='tool' && n.ms===m; }).length;
  var newVerbs = Object.values(NODES).filter(function(n){ return n.kind==='verb' && !n.planned && n.ms===m; }).length; var edVerbs = Object.values(NODES).filter(function(n){ return isTouched(n, m); }).length;
  var files = (d.files||[]); var condN = files.filter(isCond).length; var newFns = (d.functions||[]).filter(function(f){ return !f.edited; }).length;
  var tl2 = el('div','tiles'); tl2.appendChild(tile(files.length-condN,'arquivos tocados', condN? '+'+condN+' cond.':'')); tl2.appendChild(tile(newFns,'funções novas')); tl2.appendChild(tile(newMods,'módulos novos', newTools? '+'+newTools+' ferramentas':'')); tl2.appendChild(tile(newVerbs,'verbos novos', edVerbs? '+'+edVerbs+' editados':'')); aside.appendChild(tl2);
  if ((d.functionality||[]).length){ aside.appendChild(el('h3', null, 'O que cobre')); var ul2 = el('ul'); d.functionality.forEach(function(x){ ul2.appendChild(rich(el('li'), x)); }); aside.appendChild(ul2); }
  if ((d.functions||[]).length){ aside.appendChild(el('h3', null, 'Funções que entram')); var byMod = {}; d.functions.forEach(function(fn){ var k = fn.module || '(solta)'; (byMod[k]=byMod[k]||[]).push(fn); }); Object.keys(byMod).forEach(function(k){ var r = el('div','row'); r.appendChild(el('span','mono k', k==='prevcarga' ? 'scripts/prevcarga (fachada)' : k+'.R')); aside.appendChild(r); byMod[k].forEach(function(fn){ var r2 = el('div','row'); r2.style.marginLeft='12px'; var s = el('span','mono fn', fn.signature || (fn.name+'()')); s.title = fn.note||''; r2.appendChild(s); if (fn.edited) r2.appendChild(el('span','cond-note','(edita — criada antes)')); aside.appendChild(r2); }); }); }
  if ((d.verbs||[]).length){ aside.appendChild(el('h3', null, 'Verbos CLI')); d.verbs.forEach(function(v){ var n = NODES['verb:'+v.name]; var isNew = n && n.ms===m; var act = (/^REMOV/i.test(v.action||'') || (!n && /^REMOVIDO|aposentad/i.test(v.note||''))) ? 'REMOVE' : (isNew ? 'NOVO' : 'EDITA'); var r = el('div','row'); r.appendChild(el('span','act'+(act==='NOVO'?'':' cond'), act)); var s = el('span','mono'+(act==='REMOVE'?' rm':''),'prevcarga '+v.name); s.title = v.note||''; r.appendChild(s); aside.appendChild(r); }); }
  if (files.length){ aside.appendChild(el('h3', null, 'Arquivos que entram')); var PRI = {CRIAR:1, ATIVAR:2, ESTENDER:3, EDITAR:4}; var pri = function(a){ return a in PRI ? PRI[a] : 9; }; var sorted = files.slice().sort(function(a,b){ return pri(a.action)-pri(b.action); }); var code = sorted.filter(function(f){ return isCode(f.path); }), other = sorted.filter(function(f){ return !isCode(f.path); });
    code.forEach(function(f){ aside.appendChild(fileRow(f)); });
    if (other.length){ var box = el('div'); box.style.display='none'; other.forEach(function(f){ box.appendChild(fileRow(f)); }); var tg = el('button','more-files','+'+other.length+' docs/tooling (ESTADO-ATUAL, README, workflows…)'); tg.type='button'; tg.addEventListener('click', function(){ var on = box.style.display==='none'; box.style.display = on?'block':'none'; tg.textContent = (on?'− ':'+'+other.length+' ')+'docs/tooling'; }); aside.appendChild(tg); aside.appendChild(box); } }
}
function detailOf(n){ var dd = el('div','detail'); var x = el('button','x','×'); x.title='fechar'; x.setAttribute('aria-label','fechar detalhe'); x.addEventListener('click', function(){ pinNode(null); }); dd.appendChild(x); var kd = el('div','kd'); kd.appendChild(sw(KCOL[n.kind])); kd.appendChild(document.createTextNode(KNAME[n.kind]+(n.planned?' · planejado (M8)':' · entra em '+n.ms+(MSN[n.ms]? ' · '+MSN[n.ms].name:''))+(n.st? ' · '+n.st:''))); dd.appendChild(kd); dd.appendChild(el('div','nm', n.label)); if (n.ref) dd.appendChild(el('p', null, n.ref)); if (n.tag) dd.appendChild(el('p', null, n.tag)); if (n.note && n.note!==n.tag) dd.appendChild(rich(el('p'), n.note)); if (!n.tag && n.blurb) dd.appendChild(el('p', null, n.blurb));
  if (n.fns && n.fns.length){ dd.appendChild(el('h3', null, 'Funções ('+n.fns.length+')')); n.fns.forEach(function(f){ var r = el('div','row'); var s = el('span','mono fn', f.label); s.title = f.note||''; r.appendChild(s); if (f.ms && f.ms!==n.ms) r.appendChild(el('span','act', f.ms)); dd.appendChild(r); if (f.note){ var p = rich(el('p'), f.note); p.style.marginLeft='10px'; p.style.fontSize='11px'; dd.appendChild(p); } }); }
  if (n.handler){ dd.appendChild(el('h3', null, 'Handler na fachada')); var r0 = el('div','row'); r0.appendChild(el('span','mono fn', n.handler)); if (n.handlerMs) r0.appendChild(el('span','act', n.handlerMs)); dd.appendChild(r0); }
  if (n.touches){ dd.appendChild(el('h3', null, 'Quem toca')); n.touches.forEach(function(t){ var r = el('div','row'); r.appendChild(el('span','act', t.action)); r.appendChild(el('span','mono k', t.m)); if (t.note) r.appendChild(el('span', null, t.note)); dd.appendChild(r); }); }
  var out = VM.edges.filter(function(e){ return e.from===n.id; }), inn = VM.edges.filter(function(e){ return e.to===n.id; });
  if (out.length){ dd.appendChild(el('h3', null, (n.kind==='verb'?'Compõe':'Depende de')+' ('+out.length+')')); out.forEach(function(e){ var r = el('div','row'); var b = el('button','mono', NODES[e.to].label); b.style.cssText='padding:1px 6px'; b.addEventListener('click', function(){ pinNode(e.to); }); r.appendChild(b); if (e.label) r.appendChild(el('span', null, e.label)); dd.appendChild(r); }); }
  if (inn.length){ dd.appendChild(el('h3', null, 'Usado por ('+inn.length+')')); inn.forEach(function(e){ var r = el('div','row'); var b = el('button','mono', NODES[e.from].label); b.style.cssText='padding:1px 6px'; b.addEventListener('click', function(){ pinNode(e.from); }); r.appendChild(b); r.appendChild(el('span', null, (e.kind==='composes'?'compõe':'depende')+(e.label? ' · '+e.label:''))); dd.appendChild(r); }); }
  return dd; }

// ---------- matriz ----------
var mx = document.getElementById('view-matrix'); var mxRows = [];
function renderMatrix(){ mx.textContent=''; var mods = VM.flowTrain.concat(VM.flowDaily, VM.engines, VM.transversal, VM.tools||[]); var dep = {}; VM.edges.filter(function(e){ return e.kind==='depends-on'; }).forEach(function(e){ dep[e.from+'|'+e.to] = e; });
  mx.appendChild(el('p','mxnote','Linha depende de coluna ('+VM.totals.depends+' arestas depende-de). Linhas = módulos de pipeline (azul), transversais (verde) e ferramentas scripts/tools/ (cinza, fora do runtime), na ordem do mapa. Passe o mouse numa célula para ver o motivo; clique para fixar o módulo no painel.'));
  var t = el('table','mx'); var th = el('thead'); var hr = el('tr'); hr.appendChild(el('th')); mods.forEach(function(m){ var c = el('th','ck'); c.scope='col'; c.style.setProperty('--k', KCOL[m.kind]); c.appendChild(el('span', null, m.label.replace(/\\.R$/,''))); hr.appendChild(c); }); th.appendChild(hr); t.appendChild(th);
  var tb = el('tbody'); mxRows = []; mods.forEach(function(a){ var tr = el('tr'); tr.dataset.id = a.id; var rh = el('th','rk', a.label); rh.scope='row'; rh.style.setProperty('--k', KCOL[a.kind]); tr.appendChild(rh); mods.forEach(function(b){ var td = el('td'); if (a.id===b.id) td.className='diag'; var e = dep[a.id+'|'+b.id]; if (e){ var bt = el('button'); bt.type='button'; bt.setAttribute('aria-label', a.label+' depende de '+b.label+(e.label? ': '+e.label:'')); bt.appendChild(el('i')); bt.addEventListener('mouseenter', function(ev){ showTip(ev, tipEdge(e)); }); bt.addEventListener('mousemove', moveTip); bt.addEventListener('mouseleave', hideTip); bt.addEventListener('click', function(){ pinNode(a.id); setTab('map'); }); td.appendChild(bt); } tr.appendChild(td); }); tb.appendChild(tr); mxRows.push({tr:tr, n:a}); }); t.appendChild(tb); mx.appendChild(t); renderMatrixState(); }
function offRule(ms){ return state.sel!=='ALL' && (state.mode==='delta' ? ms!==state.sel : rank(ms)>rank(state.sel)); }
function renderMatrixState(){ mxRows.forEach(function(r){ r.tr.classList.toggle('is-off', offRule(r.n.ms)); }); }

// ---------- tabela ----------
var tblRows = [];
function renderTable(){ var v = document.getElementById('view-table'); v.textContent=''; tblRows = [];
  function table(caption, heads){ var t = el('table','tbl'); t.appendChild(el('caption', null, caption)); var th = el('thead'); var h = el('tr'); heads.forEach(function(x){ var c = el('th', null, x); c.scope='col'; h.appendChild(c); }); th.appendChild(h); t.appendChild(th); var tb = el('tbody'); t.appendChild(tb); return {t:t, tb:tb}; }
  function row(tb, cells, ms){ var tr = el('tr'); tr.dataset.ms = ms||''; tr.dataset.hay = cells.join(' ').toLowerCase(); cells.forEach(function(c,i){ tr.appendChild(el('td', i===1?'m':null, c||'')); }); tb.appendChild(tr); tblRows.push(tr); }
  var t1 = table('Nós ('+Object.keys(NODES).length+', incluindo '+VM.totals.functions+' funções)', ['tipo','nome','milestone','faixa / módulo','descrição']);
  VM.verbGroups.forEach(function(g){ g.verbs.forEach(function(n){ row(t1.tb, [KNAME.verb+(n.planned?' (planejado)':''), n.label, n.ms, g.label, n.blurb], n.planned?'':n.ms); if (n.handler) row(t1.tb, ['função (handler)', n.handler, n.handlerMs||n.ms, 'scripts/prevcarga · '+n.label, ''], n.handlerMs||n.ms); }); });
  VM.flowTrain.concat(VM.flowDaily, VM.engines, VM.transversal, VM.tools||[]).forEach(function(n){ row(t1.tb, [KNAME[n.kind], n.label, n.ms, (n.step? 'etapa '+n.step+' · ':'')+(n.st||'')+(n.ref? ' ('+n.ref+')':''), n.tag||n.note], n.ms); n.fns.forEach(function(f){ row(t1.tb, ['função', f.label, f.ms, n.label, f.note], f.ms); }); });
  VM.config.forEach(function(n){ row(t1.tb, [KNAME.config, n.path, n.touches.map(function(t){ return t.m+' '+t.action.toLowerCase(); }).join(', '), 'config/fachada/gates', n.blurb], n.ms); });
  v.appendChild(t1.t);
  var t2 = table('Arestas ('+VM.edges.length+': '+VM.totals.depends+' depende-de + '+(VM.edges.length-VM.totals.depends)+' compõe)', ['de','relação','para','motivo','milestone']);
  VM.edges.forEach(function(e){ row(t2.tb, [NODES[e.from].label, e.kind==='composes'?'compõe':'depende de', NODES[e.to].label, e.label, e.ms], e.ms); });
  v.appendChild(t2.t); applyTableState(); }
function applyTableState(){ var q = state.q; tblRows.forEach(function(tr){ tr.hidden = !!q && tr.dataset.hay.indexOf(q)<0; tr.classList.toggle('is-off', !!tr.dataset.ms && offRule(tr.dataset.ms)); }); }


// ---------- grafo (codegraph) ----------
var NS = 'http://www.w3.org/2000/svg';
var gview = document.getElementById('view-graph'), gsvg = document.getElementById('gsvg'), gRings = document.getElementById('g-rings'), gEdges = document.getElementById('g-edges'), gNodes = document.getElementById('g-nodes'), gLabels = document.getElementById('g-labels');
var GW = 1600, GH = 1000;
var G = { nodes:[], edges:[], rings:[], byId:{}, adj:{}, deg:{}, pos:{}, built:false, vb:{x:0,y:0,w:GW,h:GH}, drag:null, pan:null };
function hash32(str){ var h = 2166136261; for (var i=0;i<str.length;i++){ h ^= str.charCodeAt(i); h = Math.imul(h, 16777619)>>>0; } return h>>>0; }
function gRadius(nd){ if (nd.R) return nd.R; if (nd.kind==='function') return 3.6; var deg = G.deg[nd.id]||0; var base = nd.kind==='module'?9:nd.kind==='transversal'?8.5:nd.kind==='tool'?8:6.5; return base + Math.min(5, deg*0.45); }
function gKcol(nd){ if (nd.kind!=='function') return KCOL[nd.kind]; var par = G.byId[nd.parent]; return KCOL[par ? par.kind : 'module']; }
var isPack = function(){ return state.g==='pack'; };
function buildGraphData(){
  var nodes = [], edges = [];
  Object.keys(NODES).forEach(function(id){ var n = NODES[id]; nodes.push({ id:id, kind:n.kind, label:n.label, ms:n.ms, ref:n, planned:!!n.planned }); });
  VM.edges.forEach(function(e){ edges.push({ from:e.from, to:e.to, kind:e.kind, label:e.label, ms:e.ms }); });
  if (state.fn){ Object.keys(NODES).forEach(function(id){ var n = NODES[id];
    (n.fns||[]).forEach(function(f){ nodes.push({ id:f.id, kind:'function', label:f.label.replace(/^prevcarga_/,''), full:f.label, ms:f.ms, parent:id, note:f.note||'' }); if (!isPack()) edges.push({ from:id, to:f.id, kind:'contains', ms:f.ms }); });
    if (n.handler && !isPack()){ var hid = 'fn:'+n.handler.replace(/\(\)$/,''); nodes.push({ id:hid, kind:'function', label:n.handler, full:n.handler, ms:n.handlerMs||n.ms, parent:id, note:'handler da fachada' }); edges.push({ from:id, to:hid, kind:'contains', ms:n.handlerMs||n.ms }); } }); }
  G.nodes = nodes; G.edges = edges; G.byId = {}; G.adj = {}; G.deg = {}; G.rings = [];
  nodes.forEach(function(nd){ G.byId[nd.id] = nd; });
  edges.forEach(function(e){ if (e.kind!=='contains'){ G.deg[e.from]=(G.deg[e.from]||0)+1; G.deg[e.to]=(G.deg[e.to]||0)+1; } (G.adj[e.from]=G.adj[e.from]||[]).push(e.to); (G.adj[e.to]=G.adj[e.to]||[]).push(e.from); });
}
// ---- layout 1: bolhas (circle packing hierárquico: grupo → módulo(anel) → funções(pontos)) ----
function packLayout(){
  function leaf(id, r){ return { id:id, r:r, leaf:true }; }
  function modNode(m){ var fns = state.fn ? (NODES[m.id].fns||[]) : []; if (!fns.length) return { id:m.id, r: 9 + Math.min(5,(G.deg[m.id]||0)*0.45), leaf:true }; return { id:m.id, label:m.label, kind:m.kind, pad:3.5, labelRoom:4, children: fns.map(function(f){ return leaf(f.id, 3.6); }), minR: 16 }; }
  var groups = [
    { id:'grp:verb', label:'Verbos CLI', kind:'verb', pad:18, labelRoom:18, children: VM.verbGroups.map(function(g){ return { id:'sub:'+g.id, label:g.label, kind:'verb', sub:true, pad:12, labelRoom:12, children: g.verbs.map(function(v){ return leaf(v.id, 6); }) }; }) },
    { id:'grp:pipe', label:'Pipeline', kind:'module', pad:26, labelRoom:18, children: VM.flowTrain.concat(VM.flowDaily, VM.engines).map(modNode) },
    { id:'grp:trans', label:'Transversais', kind:'transversal', pad:26, labelRoom:18, children: VM.transversal.map(modNode) },
    { id:'grp:cfg', label:'Config, gates & tools', kind:'config', pad:(VM.tools||[]).length?26:14, labelRoom:(VM.tools||[]).length?20:16, children: (VM.tools||[]).map(function(m){ var n = modNode(m); if (n.leaf){ n.r = 12; } else { n.minR = 24; n.labelRoom = 10; n.pad = 5; } return n; }).concat(VM.config.map(function(c){ return leaf(c.id, 6); })) },
  ];
  var root = { id:'root', pad:56, children: groups };
  function pack(node){ if (node.leaf){ node.R = node.r; return; } var PAD = node.pad != null ? node.pad : 4; node.children.forEach(pack); var ch = node.children.slice().sort(function(a,b){ return b.R-a.R; }); var n = ch.length; var golden = Math.PI*(3-Math.sqrt(5)); var area = 0; ch.forEach(function(c){ area += Math.PI*c.R*c.R; }); var scale = Math.sqrt(area/Math.PI)*1.25;
    ch.forEach(function(c,i){ if (n===1){ c.x = 0; c.y = 0; return; } var t = i*golden + (hash32(c.id)%7)*0.01; var rr = scale*Math.sqrt((i+0.6)/n); c.x = Math.cos(t)*rr; c.y = Math.sin(t)*rr; });
    for (var it=0; it<300; it++){ var moved = false; for (var i=0;i<n;i++){ for (var j=i+1;j<n;j++){ var a = ch[i], b = ch[j]; var dx = b.x-a.x, dy = b.y-a.y; var d = Math.sqrt(dx*dx+dy*dy)||0.01; var min = a.R + b.R + PAD; if (d < min){ var push = (min-d) + 0.05; var ux = dx/d, uy = dy/d; var wa = b.R/(a.R+b.R), wb = a.R/(a.R+b.R); a.x -= ux*push*wa; a.y -= uy*push*wa; b.x += ux*push*wb; b.y += uy*push*wb; moved = true; } } } ch.forEach(function(c){ c.x *= 0.975; c.y *= 0.975; }); if (!moved && it > 60) break; }
    var R = 0; ch.forEach(function(c){ R = Math.max(R, Math.sqrt(c.x*c.x+c.y*c.y) + c.R); }); node.R = Math.max(node.minR||0, R + PAD*1.4 + (node.labelRoom||0)); }
  pack(root);
  G.pos = {}; G.rings = [];
  function place(node, ax, ay, level){ if (node.leaf){ G.pos[node.id] = { x:ax, y:ay, vx:0, vy:0, fixed:true }; var nd0 = G.byId[node.id]; if (nd0) nd0.R = node.r; return; }
    var owner = G.byId[node.id];
    if (node.id!=='root' && !owner) G.rings.push({ id:node.id, label:node.label||'', kind:node.kind, x:ax, y:ay, R:node.R, sub:!!node.sub, level:level });
    if (owner){ G.pos[node.id] = { x:ax, y:ay, vx:0, vy:0, fixed:true }; owner.R = node.R; owner.ring = true; }
    node.children.forEach(function(c){ place(c, ax + c.x, ay + c.y, level+1); }); }
  place(root, GW/2, GH/2, 0);
}
// ---- layouts 2/3: força-dirigido (camadas por faixa | livre) ----
var YT = { verb:0.06, module:0.40, transversal:0.72, config:0.92, tool:0.92 };
function seedPositions(){ G.nodes.forEach(function(nd){ nd.R = undefined; nd.ring = false; if (G.pos[nd.id]){ G.pos[nd.id].fixed = false; return; } var h = hash32(nd.id); var par = nd.parent && G.pos[nd.parent]; var x, y; if (par){ var ang = (h%360)*Math.PI/180; x = par.x + Math.cos(ang)*30; y = par.y + Math.sin(ang)*30; } else { x = 90 + (h % (GW-180)); y = ((YT[nd.kind]!=null?YT[nd.kind]:0.5)*GH) + (((h>>>12)%140)-70); } G.pos[nd.id] = {x:x, y:y, vx:0, vy:0}; }); }
function layoutGraph(iters){
  var nodes = G.nodes, n = nodes.length, pos = G.pos, layers = state.g==='layers';
  for (var it=0; it<iters; it++){
    var alpha = 1 - it/iters, cool = 0.25 + 0.75*alpha;
    for (var i=0;i<n;i++){ var A = nodes[i], a = pos[A.id]; for (var j=i+1;j<n;j++){ var B = nodes[j], b = pos[B.id]; var dx = a.x-b.x, dy = a.y-b.y; var d2 = dx*dx+dy*dy+1; var bothBig = A.kind!=='function' && B.kind!=='function'; var K = bothBig ? 14000 : (A.kind==='function' && B.kind==='function' ? 260 : 1300); if (!bothBig && d2 > 40000) continue; var f = Math.min(16, K/d2) * cool; var d = Math.sqrt(d2); a.vx += dx/d*f; a.vy += dy/d*f; b.vx -= dx/d*f; b.vy -= dy/d*f; } }
    for (var k=0;k<G.edges.length;k++){ var e = G.edges[k], pa = pos[e.from], pb = pos[e.to]; if (!pa||!pb) continue; var ex = pb.x-pa.x, ey = pb.y-pa.y; var ed = Math.sqrt(ex*ex+ey*ey)+0.01; var L = e.kind==='contains' ? 30 : e.kind==='composes' ? 140 : 160; var str = e.kind==='contains' ? 0.10 : 0.025; var s = str*(ed-L)*cool; pa.vx += ex/ed*s; pa.vy += ey/ed*s; pb.vx -= ex/ed*s; pb.vy -= ey/ed*s; }
    for (var m=0;m<n;m++){ var nd = nodes[m], p = pos[nd.id]; if (nd.kind==='function') continue; var ty = layers ? (YT[nd.kind]!=null?YT[nd.kind]:0.5)*GH : GH/2; p.vy += (ty-p.y)*(layers?0.075:0.006)*cool; p.vx += (GW/2-p.x)*(layers?0.002:0.005)*cool; }
    for (var q2=0;q2<n;q2++){ var p2 = pos[nodes[q2].id]; if (p2.fixed){ p2.vx = p2.vy = 0; continue; } var lim = 28*cool+2; p2.x += Math.max(-lim, Math.min(lim, p2.vx)); p2.y += Math.max(-lim, Math.min(lim, p2.vy)); p2.vx *= 0.45; p2.vy *= 0.45; }
  }
}
function renderGraph(){
  buildGraphData(); gsvg.classList.toggle('pack', isPack());
  if (isPack()) packLayout(); else { seedPositions(); layoutGraph(360); }
  gRings.textContent=''; gEdges.textContent=''; gNodes.textContent=''; gLabels.textContent='';
  G.rings.forEach(function(r){ var c = document.createElementNS(NS,'circle'); c.setAttribute('class','gr'+(r.sub?' sub':'')); c.setAttribute('cx', r.x); c.setAttribute('cy', r.y); c.setAttribute('r', r.R); c.style.setProperty('--k', KCOL[r.kind]); gRings.appendChild(c); c.addEventListener('pointerenter', function(ev){ var f = document.createDocumentFragment(); f.appendChild(el('b', null, r.label)); f.appendChild(document.createTextNode(' · grupo')); showTip(ev, f); }); c.addEventListener('pointermove', moveTip); c.addEventListener('pointerleave', hideTip);
    var t = document.createElementNS(NS,'text'); t.setAttribute('class','grl'+(r.sub?' sub':'')); t.setAttribute('x', r.x); t.setAttribute('y', r.y - r.R + (r.sub?10:13)); t.textContent = r.label; gLabels.appendChild(t); });
  G.edges.forEach(function(e){ var pth = document.createElementNS(NS,'path'); pth.setAttribute('class','ge '+e.kind); if (e.kind!=='contains') pth.setAttribute('marker-end','url(#gmk)'); gEdges.appendChild(pth); e.el = pth; });
  G.nodes.forEach(function(nd){ var c = document.createElementNS(NS,'circle'); c.setAttribute('class','gn '+nd.kind+(nd.ring?' ring':'')); c.style.setProperty('--k', gKcol(nd)); c.dataset.id = nd.id; gNodes.appendChild(c); nd.el = c;
    var t = document.createElementNS(NS,'text'); t.setAttribute('class','gl '+nd.kind+(nd.ring?' ringlab':'')); t.textContent = nd.kind==='function' ? nd.label : nd.kind==='verb' ? '$ '+nd.label : nd.kind==='config' ? nd.label : nd.label.replace(/\.R$/,''); gLabels.appendChild(t); nd.tl = t;
    c.addEventListener('pointerenter', function(ev){ if (G.drag||G.pan) return; state.hover = nd.id; applyGraphState(); showTip(ev, gTip(nd)); });
    c.addEventListener('pointermove', moveTip);
    c.addEventListener('pointerleave', function(){ if (G.drag) return; state.hover = null; applyGraphState(); hideTip(); });
    c.addEventListener('pointerdown', function(ev){ ev.stopPropagation(); ev.preventDefault(); var p = svgPoint(ev), ps = G.pos[nd.id]; G.drag = { id:nd.id, dx:ps.x-p.x, dy:ps.y-p.y, x0:ev.clientX, y0:ev.clientY, moved:false, lock:isPack() }; if (!isPack()) ps.fixed = true; gsvg.setPointerCapture(ev.pointerId); });
  });
  // rótulos de cluster (centróide por tipo) nos layouts de força
  if (!isPack()){ var cen = {}; G.nodes.forEach(function(nd){ if (nd.kind==='function'||nd.planned) return; var p = G.pos[nd.id]; var c = cen[nd.kind] = cen[nd.kind] || {x:0,y:0,n:0,miny:1e9}; c.x += p.x; c.y += p.y; c.n++; c.miny = Math.min(c.miny, p.y); }); var LAB = { verb:'verbos CLI', module:'módulos de pipeline', transversal:'transversais', config:'config & gates', tool:'ferramentas (tools/)' }; Object.keys(cen).forEach(function(k){ var c = cen[k]; var t = document.createElementNS(NS,'text'); t.setAttribute('class','gclab'); t.setAttribute('x', c.x/c.n); t.setAttribute('y', c.miny - 22); t.textContent = LAB[k] || k; gLabels.appendChild(t); }); }
  G.built = true; positionGraph(); applyGraphState();
}
function edgePath(a, b, ra, rb, idx){ var dx = b.x-a.x, dy = b.y-a.y, d = Math.sqrt(dx*dx+dy*dy)+0.01; var ux = dx/d, uy = dy/d; var x1 = a.x+ux*ra, y1 = a.y+uy*ra, x2 = b.x-ux*rb, y2 = b.y-uy*rb; var bend = Math.min(60, d*0.16) * ((idx%2)?1:-1); var mx = (x1+x2)/2 - uy*bend, my = (y1+y2)/2 + ux*bend; return 'M'+x1.toFixed(1)+' '+y1.toFixed(1)+' Q'+mx.toFixed(1)+' '+my.toFixed(1)+' '+x2.toFixed(1)+' '+y2.toFixed(1); }
function positionGraph(){
  G.edges.forEach(function(e, i){ var a = G.pos[e.from], b = G.pos[e.to]; if (!a||!b) return; var rb = gRadius(G.byId[e.to]) + (e.kind==='contains'?1:5), ra = gRadius(G.byId[e.from]) + 1; e.el.setAttribute('d', edgePath(a, b, ra, rb, hash32(e.from+e.to))); });
  G.nodes.forEach(function(nd){ var p = G.pos[nd.id], r = gRadius(nd); nd.el.setAttribute('cx', p.x); nd.el.setAttribute('cy', p.y); nd.el.setAttribute('r', r); if (nd.ring){ nd.tl.setAttribute('x', p.x); nd.tl.setAttribute('y', r >= 24 ? p.y - r + 11 : p.y + r + 10); } else { nd.tl.setAttribute('x', p.x + r + 4); nd.tl.setAttribute('y', p.y + 3.5); } });
}
function gMs(id){ var nd = G.byId[id]; return nd ? nd.ms : ''; }
function applyGraphState(){ if (!G.built || state.tab!=='graph') return;
  var focus = (state.pin && cards[state.pin]) ? state.pin : state.hover; var nb = {}; if (focus){ nb[focus]=1; (G.adj[focus]||[]).forEach(function(x){ nb[x]=1; }); var fnd = G.byId[focus]; if (fnd && fnd.parent) nb[fnd.parent]=1; G.nodes.forEach(function(nd){ if (nd.parent===focus) nb[nd.id]=1; }); }
  var q = state.q, m = state.sel;
  G.nodes.forEach(function(nd){ var cls = ['gn', nd.kind]; if (nd.ring) cls.push('ring');
    if (nd.planned) cls.push('is-future');
    else if (m!=='ALL'){ var del = nd.kind==='function' ? nd.ms===m : inDelta(nd.ref, m); var tch = nd.ref ? isTouched(nd.ref, m) : false; var cmp = rank(nd.ms)-rank(m);
      if (state.mode==='delta') cls.push(del ? 'is-delta' : tch ? 'is-touch' : 'is-past'); else { if (cmp>0 && !del) cls.push('is-future'); else if (del) cls.push('is-delta'); else if (tch) cls.push('is-touch'); } }
    if (q){ var hay = ((nd.full||nd.label)+' '+(nd.ref ? (nd.ref.tag||'')+' '+(nd.ref.blurb||'') : (nd.note||''))).toLowerCase(); cls.push(hay.indexOf(q)>=0 ? 'is-nb' : 'is-dim'); }
    if (focus) cls.push(nd.id===focus ? 'is-focus' : nb[nd.id] ? 'is-nb' : 'is-dim');
    nd.el.setAttribute('class', cls.join(' '));
    var on = cls.indexOf('is-focus')>=0 || cls.indexOf('is-nb')>=0 || cls.indexOf('is-delta')>=0 || cls.indexOf('is-touch')>=0; var off = !on && (cls.indexOf('is-dim')>=0 || cls.indexOf('is-future')>=0 || cls.indexOf('is-past')>=0);
    nd.tl.setAttribute('class', 'gl '+nd.kind+(nd.ring?' ringlab':'')+(on?' on':'')+(off?' off':'')); });
  G.edges.forEach(function(e){ var cls = ['ge', e.kind]; var mk = 'url(#gmk)';
    if (focus){ var inc = e.from===focus || e.to===focus; cls.push(inc ? 'hi' : 'dim'); if (inc) mk = 'url(#gmk-ink)'; }
    else { if (state.emode==='none' && e.kind!=='contains') cls.push('hidden');
      if (m!=='ALL'){ if (state.mode==='acc' && (rank(gMs(e.from))>rank(m) || rank(gMs(e.to))>rank(m))) cls.push('hidden'); else if (e.ms===m){ cls.push('is-delta'); mk = 'url(#gmk-mid)'; } else if (state.mode==='delta') cls.push('past'); } }
    e.el.setAttribute('class', cls.join(' ')); if (e.kind!=='contains') e.el.setAttribute('marker-end', mk); });
}
function gTip(nd){ if (nd.kind!=='function') return tipNode(nd.ref); var f = document.createDocumentFragment(); f.appendChild(el('b', null, nd.full)); var par = G.byId[nd.parent]; f.appendChild(document.createTextNode(' · função'+(par? ' de '+par.label:'')+' · entra em '+nd.ms)); if (nd.note) f.appendChild(el('div','mt', nd.note)); f.appendChild(el('div','mt','clique abre o detalhe do módulo-pai')); return f; }
// ---- viewport / zoom / pan / drag ----
function setVB(){ var r = gsvg.getBoundingClientRect(); if (r.width>0) G.vb.h = G.vb.w * r.height / r.width; gsvg.setAttribute('viewBox', G.vb.x+' '+G.vb.y+' '+G.vb.w+' '+G.vb.h); gsvg.classList.toggle('zoomed', !!G.fitW && G.vb.w < G.fitW*0.55); }
function svgPoint(ev){ var r = gsvg.getBoundingClientRect(); return { x: G.vb.x + (ev.clientX-r.left)/r.width*G.vb.w, y: G.vb.y + (ev.clientY-r.top)/r.height*G.vb.h }; }
function zoomBy(f){ var cx = G.vb.x + G.vb.w/2, cy = G.vb.y + G.vb.h/2; G.vb.w *= f; G.vb.h *= f; G.vb.x = cx - G.vb.w/2; G.vb.y = cy - G.vb.h/2; setVB(); }
function fitGraph(){ if (!G.built) return; var xs = [], ys = []; G.nodes.forEach(function(nd){ if (nd.planned) return; var p = G.pos[nd.id], r = gRadius(nd); xs.push(p.x-r, p.x+r); ys.push(p.y-r, p.y+r); }); G.rings.forEach(function(r){ xs.push(r.x-r.R, r.x+r.R); ys.push(r.y-r.R, r.y+r.R); }); var minx = Math.min.apply(null,xs)-50, maxx = Math.max.apply(null,xs)+120, miny = Math.min.apply(null,ys)-(isPack()?75:95), maxy = Math.max.apply(null,ys)+30; var r = gsvg.getBoundingClientRect(); var w = maxx-minx, h = maxy-miny; var asp = r.width>0 ? r.height/r.width : 0.6; if (h/w > asp){ w = h/asp; } G.vb = { x: minx - (w-(maxx-minx))/2, y: miny, w: w, h: w*asp }; G.fitW = w; setVB(); }
gsvg.addEventListener('wheel', function(ev){ ev.preventDefault(); var pt = svgPoint(ev); var f = ev.deltaY>0 ? 1.12 : 1/1.12; G.vb.x = pt.x - (pt.x-G.vb.x)*f; G.vb.y = pt.y - (pt.y-G.vb.y)*f; G.vb.w *= f; G.vb.h *= f; setVB(); }, {passive:false});
gsvg.addEventListener('pointerdown', function(ev){ if (ev.target.tagName==='circle' && ev.target.classList.contains('gn')) return; G.pan = { x0:ev.clientX, y0:ev.clientY, vx:G.vb.x, vy:G.vb.y, moved:false }; gsvg.classList.add('panning'); gsvg.setPointerCapture(ev.pointerId); });
gsvg.addEventListener('pointermove', function(ev){ var r = gsvg.getBoundingClientRect(); if (G.drag){ if (Math.abs(ev.clientX-G.drag.x0)+Math.abs(ev.clientY-G.drag.y0) > 4) G.drag.moved = true; if (!G.drag.lock){ var p = svgPoint(ev), ps = G.pos[G.drag.id]; ps.x = p.x + G.drag.dx; ps.y = p.y + G.drag.dy; positionGraph(); } hideTip(); } else if (G.pan){ var sx = G.vb.w/r.width, sy = G.vb.h/r.height; G.vb.x = G.pan.vx - (ev.clientX-G.pan.x0)*sx; G.vb.y = G.pan.vy - (ev.clientY-G.pan.y0)*sy; if (Math.abs(ev.clientX-G.pan.x0)+Math.abs(ev.clientY-G.pan.y0) > 4) G.pan.moved = true; setVB(); } });
function gEndPointer(ev){ if (G.drag){ var d = G.drag; G.drag = null; if (!d.lock) G.pos[d.id].fixed = true; if (!d.moved || d.lock){ var nd = G.byId[d.id]; var target = nd.kind==='function' ? nd.parent : nd.id; pinNode(state.pin===target ? null : target); } } else if (G.pan){ var pn = G.pan; G.pan = null; gsvg.classList.remove('panning'); if (!pn.moved && state.pin) pinNode(null); } }
gsvg.addEventListener('pointerup', gEndPointer); gsvg.addEventListener('pointercancel', gEndPointer);
function rebuildGraph(){ G.built = false; renderGraph(); fitGraph(); writeHash(); }
document.getElementById('gfn').addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (!b) return; setSeg('gfn','fn', b.dataset.v); state.fn = b.dataset.v==='1'; rebuildGraph(); });
document.getElementById('glay').addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (!b) return; setSeg('glay','g', b.dataset.v); rebuildGraph(); });
document.getElementById('greset').addEventListener('click', function(){ G.pos = {}; rebuildGraph(); });
document.getElementById('gfit').addEventListener('click', fitGraph);
document.getElementById('gzin').addEventListener('click', function(){ zoomBy(1/1.25); });
document.getElementById('gzout').addEventListener('click', function(){ zoomBy(1.25); });
document.getElementById('gzfit').addEventListener('click', fitGraph);
addEventListener('resize', function(){ if (state.tab==='graph') setVB(); });

// ---------- tabs / controles ----------
function setTab(v){ state.tab = v; setSeg('tabs','tab',v); board.style.display = v==='map' ? '' : 'none'; gview.style.display = v==='graph' ? 'block' : 'none'; mx.style.display = v==='matrix' ? 'block' : 'none'; document.getElementById('view-table').style.display = v==='table' ? 'block' : 'none'; document.getElementById('subtxt').textContent = v==='map' ? 'passe o mouse num nó para ver suas ligações · clique para fixar · Esc solta · +N expande as funções' : v==='graph' ? 'visão codegraph: nós = verbos/módulos/funções, arestas = compõe/depende/contém · o milestone e o filtro valem aqui também' : v==='matrix' ? 'linha depende de coluna · hover = motivo · clique fixa o módulo' : 'o equivalente em texto do mapa — responde ao filtro e ao milestone'; if (v==='map') layoutEdges(); if (v==='graph'){ if (!G.built) renderGraph(); fitGraph(); applyGraphState(); } writeHash(); }
function themeLabel(){ var t = document.documentElement.getAttribute('data-theme'); document.getElementById('theme').setAttribute('aria-label', 'Tema: '+(t==='dark'?'escuro':t==='light'?'claro':'sistema')+' (clique para alternar)'); }
(function(){ var st = document.getElementById('steps'); function btn(v, k, n, title){ var b = el('button'); b.dataset.v = v; b.appendChild(el('span','k', k)); b.appendChild(el('span','n', n)); if (title) b.title = title; return b; } st.appendChild(btn('ALL','Tudo','M1→M'+ORDER.length,'topologia completa')); VM.milestones.forEach(function(m){ st.appendChild(btn(m.id, m.id, m.name, m.title)); }); st.addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (b) setSel(b.dataset.v); }); })();
document.getElementById('mode').addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (!b) return; setSeg('mode','mode',b.dataset.v); applyNodes(); renderAside(); renderMatrixState(); writeHash(); });
document.getElementById('emode').addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (!b) return; setSeg('emode','emode',b.dataset.v); applyEdges(); writeHash(); });
document.getElementById('tabs').addEventListener('click', function(ev){ var b = ev.target.closest('button'); if (b) setTab(b.dataset.v); });
document.getElementById('search').addEventListener('input', function(ev){ state.q = ev.target.value.trim().toLowerCase(); applyNodes(); layoutEdges(); });
document.getElementById('theme').addEventListener('click', function(){ var r = document.documentElement; var cur = r.getAttribute('data-theme'); var sysDark = matchMedia('(prefers-color-scheme: dark)').matches; var next = cur==='dark' ? 'light' : cur==='light' ? '' : (sysDark ? 'light' : 'dark'); if (next) r.setAttribute('data-theme', next); else r.removeAttribute('data-theme'); try { localStorage.setItem('prevcarga-canvas-theme', next); } catch(e){} themeLabel(); writeHash(); });
board.addEventListener('click', function(){ if (state.pin) pinNode(null); });
document.addEventListener('keydown', function(ev){ if (ev.key==='Escape'){ pinNode(null); hideTip(); } });
addEventListener('resize', layoutEdges);
addEventListener('hashchange', function(){ readHash(); document.querySelectorAll('#mode button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===state.mode)); }); document.querySelectorAll('#emode button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===state.emode)); }); setTab(state.tab); setSel(state.sel); themeLabel(); });
if (window.ResizeObserver) new ResizeObserver(function(){ layoutEdges(); }).observe(board);

// ---------- boot ----------
try { var saved = localStorage.getItem('prevcarga-canvas-theme'); if (saved) document.documentElement.setAttribute('data-theme', saved); } catch(e){}
readHash(); if (state.pin && !cards[state.pin]) state.pin = null;
document.querySelectorAll('#mode button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===state.mode)); });
document.querySelectorAll('#emode button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===state.emode)); });
document.querySelectorAll('#gfn button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===(state.fn?'1':'0'))); }); document.querySelectorAll('#glay button').forEach(function(b){ b.setAttribute('aria-pressed', String(b.dataset.v===state.g)); });
themeLabel(); renderMatrix(); renderTable(); setTab(state.tab); setSel(state.sel); layoutEdges(); requestAnimationFrame(layoutEdges); setTimeout(layoutEdges, 120);
})();
</script>
</body>
</html>
`;
fs.writeFileSync(path.join(__dirname, 'canvas.html'), html);
console.log('canvas.html gerado: ' + (html.length / 1024).toFixed(0) + ' KB · milestones ' + ORDER.join(',') + ' · verbos ' + VM.totals.verbs + ' · etapas ' + VM.totals.stages + ' + motores ' + VM.totals.engines + ' · transversais ' + VM.totals.transversal + ' · ferramentas ' + VM.totals.tools + ' · funções ' + VM.totals.functions + ' · config ' + VM.totals.config + ' · arestas ' + VM.totals.edges + ' (depende-de ' + VM.totals.depends + ')');
