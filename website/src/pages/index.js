import React from 'react';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import { SecaoCard, Terminal } from '@site/src/components/pc';
import styles from './index.module.css';

export default function Home() {
  return (
    <Layout title="Documentação" description="Metodologia, arquitetura e execução do PrevCarga">
      <main className={styles.page}>
        <section className={styles.hero}>
          <div className={styles.heroText}>
            <span className="pc-overline">ONS · Documentação técnica</span>
            <div className={styles.lockup}>
              <img className={styles.markDark} src={useBaseUrl('/img/prevcarga-logo-c-dark.svg')} alt="PrevCarga" />
              <img className={styles.markLight} src={useBaseUrl('/img/prevcarga-logo-c-light.svg')} alt="PrevCarga" />
              <h1 className={styles.srOnly}>PrevCarga</h1>
            </div>
            <p className={styles.sub}>Previsão de carga semi-horária das 24 áreas do SIN, de D−1 a D+7, com combinação dos modelos e reconciliação até os subsistemas e o SIN. CLI <code>prevcarga</code> em R, para Linux e Windows.</p>
            <div className={styles.actions}>
              <Link className={styles.btnPrimary} to="/metodologia/visao-geral">Começar pela metodologia</Link>
              <Link className={styles.btnSecondary} to="/execucao/rodada-diaria">Executar a rodada</Link>
            </div>
          </div>
          <div className={styles.heroTerm}>
            <Terminal title="prevcarga — bash" lines={[
              { type: 'cmd', text: 'prevcarga forecast --deck ./deck --origin 2026-11-02' },
              { type: 'out', text: '[08:00:04] validate-deck  24 areas  completude ok' },
              { type: 'out', text: '[08:00:09] plan           available_until 2026-11-02 08:00' },
              { type: 'out', text: '[08:03:41] infer          8 membros x 24 areas' },
              { type: 'out', text: '[08:04:02] combina        24 areas  pesos convexos + intercepto' },
              { type: 'out', text: '[08:04:05] reconcile      4 subsistemas + SIN' },
              { type: 'ok', text: '✓ entrega gravada em ./saida/2026-11-02' },
              { type: 'dim', text: 'tente: prevcarga verificar --saida ./saida/2026-11-02' },
            ]} />
          </div>
        </section>
        <section className={styles.grid}>
          <SecaoCard overline="01" titulo="Metodologia" to="/metodologia/visao-geral"
            texto="O que é previsto, como o tempo é cortado e como cada modelo, a combinação e a reconciliação funcionam."
            itens={['Dados de entrada e os três cortes', 'RF, LGBM, Holt-Winters e DESSEM', 'Combinação e reconciliação']} />
          <SecaoCard overline="02" titulo="Arquitetura" to="/arquitetura/principios"
            texto="Módulos R importáveis, plugins por descoberta, despachante único e as garantias de honestidade."
            itens={['Princípios e tiers do repositório', 'Plugins e despachante', 'Determinismo e proveniência']} />
          <SecaoCard overline="03" titulo="Execução" to="/execucao/inicio-rapido"
            texto="Instalação, rodada operacional, backtest, publicação e verificação, com o catálogo de erros."
            itens={['Rodada operacional passo a passo', 'Backtest e verificação', 'Erros e códigos de saída']} />
        </section>
      </main>
    </Layout>
  );
}
