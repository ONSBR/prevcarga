import React from 'react';
import Footer from '@theme-original/DocItem/Footer';
import { useDoc } from '@docusaurus/plugin-content-docs/client';
import { Tag } from '@site/src/components/pc';
import styles from '@site/src/components/pc.module.css';

// Rodape "Fontes desta pagina", lido do frontmatter (fontes, adrs, revisado_em, commit_base).
// revisado_em chega como Date quando o YAML le AAAA-MM-DD; mostra em dd/mm/aaaa.
function fmtData(v) {
  const iso = v instanceof Date ? v.toISOString().slice(0, 10) : String(v);
  const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(iso);
  return m ? `${m[3]}/${m[2]}/${m[1]}` : iso;
}

export default function FooterWrapper(props) {
  const { frontMatter } = useDoc();
  const fontes = frontMatter.fontes || [];
  const adrs = frontMatter.adrs || [];
  return (
    <>
      {fontes.length ? (
        <div className={styles.fontes}>
          <span className="pc-overline">Fontes desta página</span>
          <div className={styles.tags}>{fontes.map((f) => <Tag key={f}>{f}</Tag>)}</div>
          {adrs.length ? <><span className="pc-overline" style={{ marginTop: 12 }}>Decisões</span><div className={styles.tags}>{adrs.map((a) => <Tag key={a}>{a}</Tag>)}</div></> : null}
          {frontMatter.revisado_em ? <span className={styles.rev}>Revisado em {fmtData(frontMatter.revisado_em)}{frontMatter.commit_base ? <> · base <code>{String(frontMatter.commit_base).slice(0, 7)}</code></> : null}</span> : null}
        </div>
      ) : null}
      <Footer {...props} />
    </>
  );
}
