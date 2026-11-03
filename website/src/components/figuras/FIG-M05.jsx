import React from 'react';

// FIG-M05 — available_until: derivacao a partir dos slots de carga do dia O e faixa aceita na rodada.
// Fonte: scripts/lib/deck_reader.R (.prevcarga_area_until, prevcarga_available_until, .prevcarga_derive_cut),
// scripts/lib/dates.R (prevcarga_input_cut, prevcarga_first_delivery), scripts/lib/forecast.R (.forecast_gate0).
// Areas ilustrativas (A, B, C, D); cores so por tokens do DS.

const X0 = 120;
const CW = 14;                      // um slot de 30 min
const sx = (k) => X0 + k * CW;      // borda esquerda do slot k (0 = 00:00-00:30)
const XEND = sx(48);

const T = {
  text: { fill: 'var(--text-primary)' },
  sub: { fill: 'var(--text-secondary)' },
  ter: { fill: 'var(--text-tertiary)' },
  on: { fill: 'var(--accent-soft)', stroke: 'var(--accent)', strokeWidth: 0.8 },
  off: { fill: 'var(--surface-2)', stroke: 'var(--border-subtle)', strokeWidth: 0.8 },
  hole: { fill: 'var(--danger-soft)', stroke: 'var(--danger-500)', strokeWidth: 1.2 },
  cut: { stroke: 'var(--highlight)', strokeWidth: 1.6, strokeDasharray: '4 3' },
};

const hhmm = (k) => `${String(Math.floor(k / 2)).padStart(2, '0')}:${k % 2 ? '30' : '00'}`;

// present: slots presentes no dia O (indices 0..47)
const range = (a, b) => Array.from({ length: b - a }, (_, i) => a + i);
const AREAS = [
  { nome: 'área A', present: range(0, 20), until: 20 },
  { nome: 'área B', present: range(0, 19), until: 19 },
  { nome: 'área C', present: range(0, 21), until: 21 },
];
const GAP = { nome: 'área D', present: [...range(0, 12), ...range(13, 16)], gap: 12 };
const MIN = 19;   // minimo entre A, B e C => available_until 09:30

function Row({ y, area, hole }) {
  const set = new Set(area.present);
  return (
    <g>
      <text x="16" y={y + 13} fontSize="12" style={T.text}>{area.nome}</text>
      {Array.from({ length: 48 }, (_, k) => (
        <rect key={k} x={sx(k) + 1} y={y + 1} width={CW - 2} height="16" rx="1.5"
          style={k === hole ? T.hole : set.has(k) ? T.on : T.off} />
      ))}
    </g>
  );
}

export default function FigM05() {
  const top = 56;
  const rh = 24;
  return (
    <svg viewBox="0 0 960 320" width="100%" role="img" style={{ fontFamily: 'var(--font-sans)', maxWidth: 960 }}
      aria-label="Derivação do available_until a partir dos slots de carga do dia O e faixa de corte aceita">
      <text x="16" y="20" fontSize="12" style={T.sub}>Dia O, 48 intervalos de 30 min por área (interval_start). Preenchido = carga presente.</text>

      {/* horas */}
      {Array.from({ length: 9 }, (_, i) => i * 6).map((k) => (
        <g key={k}>
          <line x1={sx(k)} y1={top - 8} x2={sx(k)} y2={top - 3} style={{ stroke: 'var(--text-tertiary)' }} />
          <text x={sx(k)} y={top - 12} fontSize="10" textAnchor="middle" style={T.ter}>{k === 48 ? 'O+1 00:00' : hhmm(k)}</text>
        </g>
      ))}

      {AREAS.map((a, i) => (
        <g key={a.nome}>
          <Row y={top + i * rh} area={a} />
          <text x={XEND + 14} y={top + i * rh + 13} fontSize="11" style={T.sub}>{`até ${hhmm(a.until)}`}</text>
        </g>
      ))}

      {/* minimo global */}
      <line x1={sx(MIN)} y1={top - 4} x2={sx(MIN)} y2={top + 3 * rh + 2} style={T.cut} />
      <text x={sx(MIN) + 4} y={top + 3 * rh + 14} fontSize="11" style={{ fill: 'var(--highlight)' }}>
        mínimo entre as áreas = available_until 09:30 (aviso available_until_spread)
      </text>

      {/* buraco */}
      <Row y={top + 3 * rh + 30} area={GAP} hole={GAP.gap} />
      <text x={XEND + 14} y={top + 3 * rh + 43} fontSize="11" style={{ fill: 'var(--danger-500)' }}>buraco às 06:00</text>
      <text x={X0} y={top + 3 * rh + 62} fontSize="10.5" style={T.sub}>
        Há carga depois do buraco: completude.origin_day_gap (exit 3). O corte nunca encurta por causa do buraco.
      </text>

      {/* derivados */}
      <g transform={`translate(16 ${top + 3 * rh + 82})`}>
        {[
          ['granularidade 30', 'input_cut = 09:30'],
          ['granularidade 60', 'input_cut = 09:00 (hora cheia)'],
          ['primeira entrega', 'interval_end 10:00 (patamar 20)'],
        ].map(([a, b], k) => (
          <g key={a} transform={`translate(${k * 306} 0)`}>
            <rect x="0" y="0" width="298" height="34" rx="3" style={{ fill: 'var(--surface-2)', stroke: 'var(--border-default)' }} />
            <text x="10" y="14" fontSize="10" style={T.ter}>{a}</text>
            <text x="10" y="28" fontSize="11.5" style={T.text}>{b}</text>
          </g>
        ))}
      </g>

      {/* faixa aceita */}
      <g transform={`translate(0 ${top + 3 * rh + 140})`}>
        <text x="16" y="13" fontSize="12" style={T.text}>corte aceito</text>
        <rect x={X0} y="0" width={XEND - X0} height="18" rx="2"
          style={{ fill: 'var(--highlight-soft)', stroke: 'var(--highlight)', strokeWidth: 1 }} />
        <rect x={XEND} y="0" width="120" height="18" rx="2"
          style={{ fill: 'var(--warning-soft)', stroke: 'var(--warning-500)', strokeWidth: 1 }} />
        <circle cx={X0} cy="9" r="4.5" style={{ fill: 'var(--highlight)' }} />
        <text x={X0} y="34" fontSize="10.5" style={T.sub}>O 00:00 aceito: rodada das 00h, 1ª entrega O 00:30</text>
        <text x={X0 + (XEND - X0) / 2 + 60} y="13" fontSize="10.5" textAnchor="middle" style={T.text}>
          qualquer corte intradiário de O, na grade de 30 min
        </text>
        <text x={XEND + 6} y="13" fontSize="10.5" style={{ fill: 'var(--warning-500)' }}>≥ O+1 00:00</text>
        <text x={XEND - 4} y="34" fontSize="10.5" textAnchor="end" style={{ fill: 'var(--warning-500)' }}>
          recusado: completude.forecast_origin_day_complete (exit 3)
        </text>
      </g>
    </svg>
  );
}
