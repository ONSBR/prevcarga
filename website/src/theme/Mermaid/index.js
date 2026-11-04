import React, { useCallback, useEffect, useRef, useState } from 'react';
import Mermaid from '@theme-original/Mermaid';
import styles from './styles.module.css';

// Moldura de zoom para todo diagrama Mermaid: 30% a 600%, arrastar para mover,
// Ctrl+roda para zoom no cursor e tela cheia.
const MIN = 0.3;
const MAX = 6;
const STEPS = [0.3, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4, 6];
const clamp = (z) => Math.min(MAX, Math.max(MIN, z));

function Icon({ d }) {
  return (
    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.75" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
      {d.map((p) => <path key={p} d={p} />)}
    </svg>
  );
}
const I = {
  menos: ['M5 12h14'],
  mais: ['M12 5v14', 'M5 12h14'],
  ajustar: ['M3 7V3h4', 'M21 7V3h-4', 'M3 17v4h4', 'M21 17v4h-4'],
  cheia: ['m21 21-6-6m6 6v-4.8m0 4.8h-4.8', 'M3 16.2V21m0 0h4.8M3 21l6-6', 'M21 7.8V3m0 0h-4.8M21 3l-6 6', 'M3 7.8V3m0 0h4.8M3 3l6 6'],
  sair: ['m15 15 6 6m-6-6v4.8m0-4.8h4.8', 'M9 19.8V15m0 0H4.2M9 15l-6 6', 'M15 4.2V9m0 0h4.8M15 9l6-6', 'M9 4.2V9m0 0H4.2M9 9 3 3'],
};

export default function MermaidZoom(props) {
  const frame = useRef(null);
  const view = useRef(null);
  const base = useRef(null); // largura natural do SVG (100%)
  const drag = useRef(null);
  const scroller = useRef(null);
  const fullRef = useRef(false);
  const [zoom, setZoom] = useState(1);
  const [full, setFull] = useState(false);
  fullRef.current = full;
  const [ready, setReady] = useState(false);
  const [left, setLeft] = useState(0);

  const svgEl = () => view.current?.querySelector('.docusaurus-mermaid-container svg');

  // Aplica a escala mudando a largura real do SVG: o texto continua vetorial e a rolagem do visor faz o pan.
  const apply = useCallback((z) => {
    const svg = svgEl();
    if (!svg || !base.current) return;
    svg.style.maxWidth = 'none';
    svg.style.width = `${base.current * z}px`;
    svg.style.height = 'auto';
  }, []);

  // O Mermaid desenha de forma assincrona: espera o SVG aparecer para medir a largura natural.
  useEffect(() => {
    const el = view.current;
    if (!el) return undefined;
    const measure = () => {
      const svg = svgEl();
      if (!svg) return false;
      const vb = svg.viewBox?.baseVal;
      const natural = vb && vb.width ? vb.width : svg.getBoundingClientRect().width;
      const avail = el.clientWidth - 2;
      base.current = Math.min(natural, avail > 0 ? avail : natural);
      setReady(true);
      apply(1);
      return true;
    };
    if (measure()) return undefined;
    const mo = new MutationObserver(() => { if (measure()) mo.disconnect(); });
    mo.observe(el, { childList: true, subtree: true });
    return () => mo.disconnect();
  }, [apply]);

  // Zoom mantendo fixo o ponto (px, py) do visor.
  const zoomTo = useCallback((next, px, py) => {
    const el = view.current;
    setZoom((cur) => {
      const z = clamp(next(cur));
      if (el) {
        const cx = px ?? el.clientWidth / 2;
        const cy = py ?? el.clientHeight / 2;
        const fx = (el.scrollLeft + cx) / cur;
        const fy = (el.scrollTop + cy) / cur;
        apply(z);
        el.scrollLeft = fx * z - cx;
        el.scrollTop = fy * z - cy;
      }
      return z;
    });
  }, [apply]);

  const step = (dir) => zoomTo((z) => {
    const i = dir > 0 ? STEPS.findIndex((s) => s > z + 1e-6) : STEPS.slice().reverse().findIndex((s) => s < z - 1e-6);
    if (i < 0) return z;
    return dir > 0 ? STEPS[i] : STEPS[STEPS.length - 1 - i];
  });
  const fitRef = useRef(null);
  const fit = () => {
    const el = view.current;
    if (!el || !base.current) return;
    const z = clamp((el.clientWidth - 48) / base.current);
    setZoom(z); apply(z); el.scrollLeft = 0;
  };
  fitRef.current = fit;

  // Roda do mouse tratada aqui, igual em qualquer navegador:
  // Ctrl/Cmd + roda = zoom no cursor; roda vertical = rola o painel (tela cheia) ou a pagina;
  // roda horizontal (ou Shift) = rola a figura. A figura nunca prende a rolagem vertical.
  useEffect(() => {
    const el = view.current;
    if (!el) return undefined;
    const onWheel = (e) => {
      const unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? window.innerHeight : 1;
      let dx = e.deltaX * unit;
      let dy = e.deltaY * unit;
      if (e.ctrlKey || e.metaKey) {
        e.preventDefault();
        const r = el.getBoundingClientRect();
        const f = Math.exp(-dy * 0.0015);
        zoomTo((z) => z * f, e.clientX - r.left, e.clientY - r.top);
        return;
      }
      if (e.shiftKey && !dx) { dx = dy; dy = 0; }
      e.preventDefault();
      if (dx) el.scrollLeft += dx;
      if (dy) {
        const sc = fullRef.current ? scroller.current : null;
        if (sc) sc.scrollTop += dy; else window.scrollBy(0, dy);
      }
    };
    el.addEventListener('wheel', onWheel, { passive: false });
    return () => el.removeEventListener('wheel', onWheel);
  }, [zoomTo]);

  // Arrastar para mover quando o diagrama passa do visor.
  const onDown = (e) => {
    if (e.button !== 0) return;
    const el = view.current;
    drag.current = { x: e.clientX, y: e.clientY, l: el.scrollLeft, t: el.scrollTop };
    el.setPointerCapture?.(e.pointerId);
  };
  const onMove = (e) => {
    const d = drag.current; const el = view.current;
    if (!d) return;
    el.scrollLeft = d.l - (e.clientX - d.x);
    el.scrollTop = d.t - (e.clientY - d.y);
  };
  const onUp = () => { drag.current = null; };

  // Tela cheia dentro da pagina: o painel cobre o corpo do documento (a barra do site e a sidebar
  // continuam visiveis) e rola na vertical como uma pagina; a figura so rola na horizontal. Esc fecha.
  useEffect(() => {
    if (!full) return undefined;
    const measure = () => {
      const sb = document.querySelector('.theme-doc-sidebar-container');
      setLeft(sb ? Math.max(0, sb.getBoundingClientRect().right) : 0);
    };
    measure();
    const onEsc = (e) => { if (e.key === 'Escape') setFull(false); };
    const prev = document.body.style.overflow;
    document.body.style.overflow = 'hidden';
    document.addEventListener('keydown', onEsc);
    window.addEventListener('resize', measure);
    const ro = window.ResizeObserver ? new ResizeObserver(measure) : null;
    const sb = document.querySelector('.theme-doc-sidebar-container');
    if (ro && sb) ro.observe(sb);
    return () => {
      document.body.style.overflow = prev;
      document.removeEventListener('keydown', onEsc);
      window.removeEventListener('resize', measure);
      ro?.disconnect();
    };
  }, [full]);
  // Ao abrir, encaixa pela largura (ate 150%); ao fechar, volta a 100%.
  useEffect(() => {
    if (!ready) return;
    requestAnimationFrame(() => {
      const el = view.current;
      if (!el || !base.current) return;
      const z = full ? clamp(Math.min((el.clientWidth - 48) / base.current, 1.5)) : 1;
      setZoom(z); apply(z); el.scrollLeft = 0;
    });
  }, [full, ready, left, apply]);
  const toggleFull = () => setFull((f) => !f);

  const onKey = (e) => {
    if (e.key === '+' || e.key === '=') { e.preventDefault(); step(1); }
    else if (e.key === '-') { e.preventDefault(); step(-1); }
    else if (e.key === '0') { e.preventDefault(); setZoom(1); apply(1); }
  };

  return (
    <div ref={frame} className={styles.frame} data-full={full ? 'true' : undefined} style={full ? { left } : undefined}>
      <div ref={scroller} className={styles.scroller}>
      <div className={styles.bar} role="toolbar" aria-label="Zoom do diagrama">
        <button type="button" className={styles.btn} onClick={() => step(-1)} disabled={!ready || zoom <= MIN + 1e-6} aria-label="Diminuir zoom" title="Diminuir zoom (−)"><Icon d={I.menos} /></button>
        <input
          className={styles.range} type="range" min={MIN * 100} max={MAX * 100} step={5}
          value={Math.round(zoom * 100)} disabled={!ready} aria-label="Zoom"
          onChange={(e) => zoomTo(() => Number(e.target.value) / 100)}
        />
        <button type="button" className={styles.btn} onClick={() => step(1)} disabled={!ready || zoom >= MAX - 1e-6} aria-label="Aumentar zoom" title="Aumentar zoom (+)"><Icon d={I.mais} /></button>
        <button type="button" className={styles.pct} onClick={() => { setZoom(1); apply(1); }} disabled={!ready} title="Voltar a 100% (0)">{Math.round(zoom * 100)}%</button>
        <span className={styles.sep} />
        <button type="button" className={styles.btn} onClick={fit} disabled={!ready} aria-label="Ajustar à área" title="Ajustar à área"><Icon d={I.ajustar} /></button>
        <button type="button" className={styles.btn} onClick={toggleFull} aria-label={full ? 'Sair da tela cheia' : 'Tela cheia'} title={full ? 'Sair da tela cheia (Esc)' : 'Tela cheia'}><Icon d={full ? I.sair : I.cheia} /></button>
      </div>
      <div
        ref={view} className={styles.view} tabIndex={0} onKeyDown={onKey}
        onPointerDown={onDown} onPointerMove={onMove} onPointerUp={onUp} onPointerCancel={onUp}
        data-zoomed={zoom > 1.001 ? 'true' : undefined}
      >
        <Mermaid {...props} />
      </div>
      </div>
    </div>
  );
}
