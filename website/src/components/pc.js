import React from 'react';
import Link from '@docusaurus/Link';
import styles from './pc.module.css';

// Janela de terminal da CLI (porte do components/cli/Terminal.jsx do DS).
function renderCmd(text) {
  let seenBin = false;
  return String(text).split(/(\s+)/).map((p, i) => {
    if (!p.trim()) return p;
    if (!seenBin) { seenBin = true; return <span key={i} className={styles.bin}>{p}</span>; }
    if (p.startsWith('-')) return <span key={i} className={styles.flag}>{p}</span>;
    return p;
  });
}
function renderOut(text) {
  const m = /^(\[[^\]]+\])(.*)$/s.exec(String(text));
  return m ? <><span className={styles.ts}>{m[1]}</span>{m[2]}</> : text;
}
export function Terminal({ title = 'prevcarga — bash', lines = [] }) {
  return (
    <div className={styles.term}>
      <div className={styles.bar}><div className={styles.dots}><i /><i /><i /></div><div className={styles.title}>{title}</div></div>
      <div className={styles.body}>
        {lines.map((l, i) => {
          const type = l.type || 'out';
          if (type === 'blank') return <span key={i} className={styles.line}> </span>;
          if (type === 'cmd') return <span key={i} className={styles.line} data-type="cmd"><span className={styles.prompt}>❯</span>{renderCmd(l.text)}</span>;
          return <span key={i} className={styles.line} data-type={type}>{type === 'out' || type === 'dim' ? renderOut(l.text) : l.text}</span>;
        })}
      </div>
    </div>
  );
}

export function Tag({ children, mono = true }) {
  return <span className={styles.tag} data-mono={mono ? 'true' : undefined}>{children}</span>;
}

// Pendencia: o que ainda nao esta decidido ou medido. Nunca apresentar como fato.
export function Pendente({ titulo = 'Pendente', children }) {
  return (
    <div className={styles.pendente} role="note">
      <span className={styles.pendenteHead}>{titulo}</span>
      <div className={styles.pendenteBody}>{children}</div>
    </div>
  );
}

// Toda figura entra por aqui: legenda com id, titulo, natureza e as fontes (que tambem estao no frontmatter).
export function Figura({ id, titulo, natureza, fontes = [], adrs = [], children }) {
  return (
    <figure className={styles.figura} id={id ? id.toLowerCase() : undefined}>
      {children}
      <figcaption>
        {id ? <span className={styles.fignum}>{id}</span> : null} {titulo}
        {natureza ? <span className={styles.nat}>{natureza}</span> : null}
        {fontes.length || adrs.length ? <span className={styles.figsrc}>{[...fontes, ...adrs].join(' · ')}</span> : null}
      </figcaption>
    </figure>
  );
}

export function SecaoCard({ overline, titulo, texto, to, itens = [] }) {
  return (
    <Link to={to} className={styles.card}>
      <span className="pc-overline">{overline}</span>
      <span className={styles.cardTitle}>{titulo}</span>
      <span className={styles.cardText}>{texto}</span>
      <ul className={styles.cardList}>{itens.map((t) => <li key={t}>{t}</li>)}</ul>
      <span className={styles.cardCta}>Abrir seção →</span>
    </Link>
  );
}
