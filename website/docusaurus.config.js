// Site de documentacao do PrevCarga (Metodologia, Arquitetura, Execucao).
const config = {
  title: 'PrevCarga',
  tagline: 'Modelo de previsão de carga do Sistema Interligado Nacional',
  url: 'https://prevcarga.invalid',
  baseUrl: '/',
  favicon: 'img/favicon.svg',
  onBrokenLinks: 'throw',
  onBrokenAnchors: 'throw',
  i18n: { defaultLocale: 'pt-BR', locales: ['pt-BR'] },
  markdown: { mermaid: true, hooks: { onBrokenMarkdownLinks: 'throw' } },
  themes: [
    '@docusaurus/theme-mermaid',
    ['@easyops-cn/docusaurus-search-local', { hashed: true, language: ['pt'], indexBlog: false, docsRouteBasePath: '/' }],
  ],
  stylesheets: [
    'https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&family=Source+Code+Pro:wght@400;500;600&display=swap',
  ],
  presets: [
    ['classic', {
      docs: { routeBasePath: '/', sidebarPath: './sidebars.js' },
      blog: false,
      theme: { customCss: './src/css/custom.css' },
    }],
  ],
  themeConfig: {
    colorMode: { defaultMode: 'dark', respectPrefersColorScheme: false },
    navbar: {
      logo: { alt: 'PrevCarga', src: 'img/prevcarga-logo-c-light.svg', srcDark: 'img/prevcarga-logo-c-dark.svg' },
      items: [
        { type: 'docSidebar', sidebarId: 'metodologia', label: 'Metodologia', position: 'left' },
        { type: 'docSidebar', sidebarId: 'arquitetura', label: 'Arquitetura', position: 'left' },
        { type: 'docSidebar', sidebarId: 'execucao', label: 'Execução', position: 'left' },
        { type: 'html', position: 'right', value: '<span class="pc-ons"><img class="pc-ons-light" src="/img/logo-ons-preta.png" alt="ONS"/><img class="pc-ons-dark" src="/img/logo-ons-branca.png" alt="ONS"/></span>' },
      ],
    },
    footer: {
      copyright: 'ONS · Operador Nacional do Sistema Elétrico',
    },
    prism: { additionalLanguages: ['r', 'bash', 'yaml', 'powershell'] },
    mermaid: { theme: { light: 'base', dark: 'base' }, options: { themeVariables: { fontFamily: 'Outfit, ui-sans-serif, system-ui, sans-serif', fontSize: '14px' } } },
    docs: { sidebar: { hideable: true } },
  },
};
module.exports = config;
