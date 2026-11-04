import React from 'react';

// FIG-M04 — Os tres cortes de data por modelo (linha do tempo, O = 2026-10-01).
// Fonte: scripts/lib/dates.R (prevcarga_train_cut, prevcarga_first_delivery), models/*/run.yaml
// (split, horizon_days, history_days.train) e models/*/R/register.R (data_lag_days, delivery_days, reads_day_O).
// Eixo em dias relativos a O, com quebra a esquerda para a janela de historico. Cores so por tokens do DS.

const X0 = 250;      // inicio da coluna O-12
const DW = 30;       // largura de um dia
const dx = (d) => X0 + (d + 12) * DW;   // borda esquerda do dia O+d
const AU = 9 / 24;   // available_until do exemplo: 09:00 do dia O

const T = {
  text: { fill: 'var(--text-primary)' },
  sub: { fill: 'var(--text-secondary)' },
  ter: { fill: 'var(--text-tertiary)' },
  grid: { stroke: 'var(--border-subtle)', strokeWidth: 1 },
  train: { fill: 'var(--accent-soft)', stroke: 'var(--accent)', strokeWidth: 1 },
  dayO: { fill: 'var(--highlight-soft)', stroke: 'var(--highlight)', strokeWidth: 1.2 },
  deliv: { fill: 'var(--surface-3)', stroke: 'var(--border-strong)', strokeWidth: 1 },
  mark: { fill: 'var(--text-primary)' },
  cut: { stroke: 'var(--highlight)', strokeWidth: 1.5, strokeDasharray: '4 3' },
};

// last: ultimo dia de dado do treino (relativo a O); to: train_origin; first: 1o dia da janela;
// readsO: le carga do dia O ate available_until; dd: delivery_days.
const LANES = [
  { nome: 'random-forest', meta: 'origin_agnostic · H 9 · lag 0', to: -10, last: -1, first: 'O−375', readsO: false, dd: 9, alvo: true },
  { nome: 'lgbm', meta: 'origin_agnostic · H 1 · lag 2', to: -2, last: -3, first: 'O−732', readsO: true, dd: 2 },
  { nome: 'holt-winters', meta: 'origin_scoped · lag 0', to: 0, last: -1, first: 'O−730', readsO: false, dd: 9 },
  { nome: 'dessem (linear, regdin)', meta: 'origin_scoped · lag 0', to: 0, last: -1, first: 'O−1.825', readsO: true, dd: 9 },
];

function Diamond({ x, y }) {
  return <path d={`M ${x} ${y - 6} L ${x + 6} ${y} L ${x} ${y + 6} L ${x - 6} ${y} Z`} style={T.mark} />;
}

export default function FigM04() {
  const days = Array.from({ length: 22 }, (_, i) => i - 12);
  const top = 70;
  const lh = 58;
  const xAU = dx(0) + AU * DW;
  return (
    <svg viewBox="0 0 960 400" width="100%" role="img" style={{ fontFamily: 'var(--font-sans)', maxWidth: 960 }}
      aria-label="Linha do tempo dos cortes de data por modelo para O = 2026-10-01">
      <defs>
        <pattern id="figm04-hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">
          <line x1="0" y1="0" x2="0" y2="5" style={{ stroke: 'var(--highlight)', strokeWidth: 1.4 }} />
        </pattern>
      </defs>

      <text x="16" y="22" fontSize="12" style={T.sub}>Exemplo: O = 2026-10-01, available_until = O 09:00. Eixo em dias relativos a O.</text>

      {/* eixo de dias */}
      {days.map((d) => (
        <g key={d}>
          <line x1={dx(d)} y1={top - 18} x2={dx(d)} y2={top + LANES.length * lh - 10} style={T.grid} />
          <text x={dx(d) + DW / 2} y={top - 24} fontSize="10" textAnchor="middle"
            style={d === 0 ? { ...T.text, fontWeight: 600 } : T.ter}>
            {d === 0 ? 'O' : d < 0 ? `−${-d}` : `+${d}`}
          </text>
        </g>
      ))}
      <line x1={dx(10)} y1={top - 18} x2={dx(10)} y2={top + LANES.length * lh - 10} style={T.grid} />
      <text x="212" y={top - 24} fontSize="10" textAnchor="middle" style={T.ter}>janela</text>

      {/* quebra do eixo */}
      <path d={`M 234 ${top - 20} l 5 8 l -5 8 M 242 ${top - 20} l 5 8 l -5 8`} style={{ stroke: 'var(--text-tertiary)', fill: 'none' }} />

      {LANES.map((l, i) => {
        const y = top + i * lh;
        const by = y + 8;
        const bh = 18;
        const lastEnd = dx(l.last + 1);
        return (
          <g key={l.nome}>
            <text x="16" y={y + 16} fontSize="12.5" style={{ ...T.text, fontWeight: 600 }}>{l.nome}</text>
            <text x="16" y={y + 32} fontSize="10.5" style={T.sub}>{l.meta}</text>

            {/* dado de treino: da janela ate o fim de last_data_day */}
            <rect x="186" y={by} width={lastEnd - 186} height={bh} rx="2" style={T.train} />
            <text x="192" y={by + 13} fontSize="10" style={T.text}>desde {l.first}</text>
            <line x1={lastEnd} y1={by - 4} x2={lastEnd} y2={by + bh + 4} style={{ stroke: 'var(--accent)', strokeWidth: 2 }} />

            {/* train_origin */}
            <Diamond x={dx(l.to) + DW / 2} y={by + bh + 12} />
            <text x={dx(l.to) + DW / 2 + 9} y={by + bh + 16} fontSize="10" style={T.sub}>
              {`train_origin ${l.to === 0 ? 'O' : `O−${-l.to}`}`}
            </text>

            {/* alvos da ultima amostra do RF: T+1..T+9 */}
            {l.alvo ? (
              <path d={`M ${dx(-9)} ${by - 3} L ${dx(-9)} ${by - 7} L ${dx(0)} ${by - 7} L ${dx(0)} ${by - 3}`}
                style={{ stroke: 'var(--text-tertiary)', fill: 'none' }} />
            ) : null}
            {l.alvo ? <text x={dx(-9) + 4} y={by - 10} fontSize="9.5" style={T.ter}>alvos da amostra T = O−10: T+1..T+9</text> : null}

            {/* leitura do dia O ate available_until */}
            {l.readsO ? (
              <g>
                <rect x={dx(0)} y={by} width={AU * DW} height={bh} style={T.dayO} />
                <rect x={dx(0)} y={by} width={AU * DW} height={bh} fill="url(#figm04-hatch)" />
              </g>
            ) : (
              <text x={dx(0) + DW / 2} y={by + 13} fontSize="9" textAnchor="middle" style={T.ter}>—</text>
            )}

            {/* entrega: de available_until + 30 min ate O+dd 00:00 */}
            <rect x={xAU + 2} y={by} width={dx(l.dd) - xAU - 2} height={bh} rx="2" style={T.deliv} />
            <text x={l.dd > 3 ? dx(l.dd) - 6 : dx(l.dd) + 6} y={by + 13} fontSize="10" textAnchor={l.dd > 3 ? 'end' : 'start'} style={T.text}>
              {`entrega até O+${l.dd - 1}`}
            </text>
          </g>
        );
      })}

      {/* available_until */}
      <line x1={xAU} y1={top - 14} x2={xAU} y2={top + LANES.length * lh - 6} style={T.cut} />
      <text x={xAU + 4} y={top + LANES.length * lh + 6} fontSize="10" style={{ fill: 'var(--highlight)' }}>available_until</text>

      {/* legenda */}
      <g transform={`translate(16 ${top + LANES.length * lh + 26})`}>
        <rect x="0" y="0" width="18" height="11" style={T.train} />
        <text x="24" y="10" fontSize="10.5" style={T.sub}>carga do treino (até last_data_day, traço azul)</text>
        <Diamond x={290} y={5} />
        <text x="300" y="10" fontSize="10.5" style={T.sub}>train_origin</text>
        <rect x="390" y="0" width="18" height="11" style={T.dayO} />
        <rect x="390" y="0" width="18" height="11" fill="url(#figm04-hatch)" />
        <text x="414" y="10" fontSize="10.5" style={T.sub}>carga do dia O lida no infer</text>
        <rect x="600" y="0" width="18" height="11" style={T.deliv} />
        <text x="624" y="10" fontSize="10.5" style={T.sub}>faixa de entrega (desde available_until + 30 min)</text>
      </g>

      {/* derivacao */}
      <g transform={`translate(16 ${top + LANES.length * lh + 52})`}>
        {[['O: --origin ou último dia de carga', 206], ['resolve_cuts', 96], ['train_origin (split, H)', 150], ['last_data_day = O−1−lag', 160], ['available_until (leitor)', 160], ['entrega', 80]].map(([s, w], k, a) => {
          const x = a.slice(0, k).reduce((acc, [, ww]) => acc + ww + 14, 0);
          return (
            <g key={s}>
              <rect x={x} y="0" width={w} height="24" rx="3" style={{ fill: 'var(--surface-2)', stroke: 'var(--border-default)' }} />
              <text x={x + w / 2} y="16" fontSize="10" textAnchor="middle" style={T.text}>{s}</text>
              {k < a.length - 1 ? <text x={x + w + 4} y="16" fontSize="10" style={T.ter}>›</text> : null}
            </g>
          );
        })}
      </g>
    </svg>
  );
}
