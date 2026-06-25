# fluxoescolar

> Indicadores de fluxo escolar a partir do painel longitudinal da PNAD Contínua

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R ≥ 4.0](https://img.shields.io/badge/R-%E2%89%A5%204.0-blue.svg)]()

Pacote em R para calcular os cinco indicadores de fluxo escolar brasileiro
— **abandono** (intra-ano), **evasão**, **promoção**, **repetência** e
**não-progressão** (entre-anos) — diretamente a partir do painel
longitudinal de cinco trimestres da Pesquisa Nacional por Amostra de
Domicílios Contínua (PNAD Contínua / PNADC) do IBGE.

A proposta dialoga com a tradição PROFLUXO (Fletcher, Ribeiro e Klein,
1985-2006), mas não é uma extensão daquele modelo: oferece uma família
nova de indicadores construídos por **observação direta** das transições
individuais entre visitas do painel — em vez da inferência cross-sectional
do PROFLUXO.

## Instalação

```r
# Instalar de GitHub (requer devtools/remotes):
remotes::install_github("vitpereira/fluxoescolar")
```

## Uso básico

```r
library(fluxoescolar)
library(data.table)

# 1. Baixar microdados PNADC trimestral do IBGE (cerca de 11 GB)
baixar_pnadc(
    dir_destino = "~/dados/pnadc",
    anos = 2012:2024
)

# 2. Parsear os zips em data.tables (cerca de 1h)
parsear_pnadc(
    dir_zips    = "~/dados/pnadc",
    dir_destino = "~/dados/pnadc_parsed"
)

# 3. Carregar e harmonizar
files <- list.files("~/dados/pnadc_parsed", full.names = TRUE)
df_all <- rbindlist(lapply(files, function(f) {
    harmonizar_pnadc(readRDS(f))
}))

# 4. Construir o painel longitudinal individual
painel <- construir_painel(df_all)

# 5. Calcular indicadores: agregado Brasil por macroetapa-ano
indicadores <- calcular_indicadores(
    painel,
    tipo = "inter",
    desagregacao = c("ano_t", "macroetapa"),
    periodo = c(2012, 2024)
)
print(indicadores)

# 6. Heterogeneidade socioeconomica: por quintil de renda
indicadores_renda <- calcular_indicadores(
    painel,
    tipo = "inter",
    desagregacao = c("macroetapa", "quintil_renda"),
    periodo = c(2018, 2023)
)

# 7. Visualizar
fig1 <- figura_serie_temporal(indicadores)
ggplot2::ggsave("F1_serie_temporal.pdf", fig1, width = 10, height = 7)

fig2 <- figura_heterogeneidade(
    indicadores_renda,
    indicador = "flag_promocao",
    subgrupo = "quintil_renda",
    macroetapa = 3  # EM
)
ggplot2::ggsave("F2_gradiente_renda_EM.pdf", fig2, width = 7, height = 5)
```

## Funções principais

| Função | Descrição |
|---|---|
| `baixar_pnadc()` | Download dos microdados trimestrais do FTP IBGE |
| `parsear_pnadc()` | Parse dos fixed-width em data.tables |
| `harmonizar_pnadc()` | Padroniza variáveis entre versões (2012-2024) |
| `construir_painel()` | Linkagem individual via Ribas-Soares (sexo + idade) |
| `calcular_indicadores()` | Os 5 indicadores por subgrupo |
| `decompor_inep()` | Decomposição R+U+S+C+M vs. INEP |
| `figura_serie_temporal()` | Série temporal por macroetapa |
| `figura_heterogeneidade()` | Barras por subgrupo |

## Definições operacionais dos 5 indicadores

Seja $i$ um indivíduo com série $s$ no ano $t$. Os cinco indicadores são:

- **Abandono** (intra-ano): $i$ tinha `freq=1` na primeira observação do ano $t$ e
  `freq=0` na última observação do mesmo ano. Captura saída intra-ano.

- **Evasão** (entre-anos): $i$ tinha `freq=1` em $t$ e `freq=0` em $t+1$.

- **Promoção**: $i$ avançou para série $s+1$ ou etapa seguinte
  (9° EF → 1° EM; 3° EM → superior).

- **Repetência**: $i$ está na mesma série $s$ em $t+1$.

- **Não-progressão** = Repetência + Evasão = $1 -$ Promoção.

**Identidade contábil:** Promoção + Repetência + Evasão = 1 (entre-anos).

## Painel longitudinal da PNADC

Cada domicílio é entrevistado em **cinco trimestres consecutivos**. Os
identificadores estáveis são:

- Domicílio: `UF + UPA + V1008 + V1014`
- Indivíduo: `+ V2003` (ordem na lista), validado por sexo + idade
  (tolerância 1 ano entre visitas consecutivas)

## Limitações conhecidas

1. **Atrito do painel**: ~15-20% dos indivíduos saem entre visitas. Use
   `construir_painel()` com `link_ok == 1` e considere reponderar por
   probabilidade inversa de atrito (não implementado por padrão).

2. **COVID 2020-2021**: tamanho amostral reduzido em até 40% (entrevistas
   por telefone), aprovação automática em vários sistemas. Trate esse
   período com cautela ou exclua-o.

3. **CadÚnico**: PNADC não tem variável direta. O proxy disponível é
   `renda_dom_pc ≤ 1/2 SM`. O recebimento de BFA só aparece na Visita 5
   (suplemento anual).

## Citação

Se este pacote ajudou seu trabalho, por favor cite:

> Pereira, V. A. (2026). *fluxoescolar: Indicadores de Fluxo Escolar via
> Painel Longitudinal da PNAD Contínua*. R package version 0.1.0.
> https://github.com/vitpereira/fluxoescolar

Para a tradição PROFLUXO em que o método se inspira:

> Ribeiro, S. C. (1991). A pedagogia da repetência. *Estudos Avançados*, 5(12), 7-21.
> Klein, R., & Ribeiro, S. C. (1991). O Censo Educacional e o modelo de fluxo: o problema da repetência. *Revista Brasileira de Estatística*, 52(197/198).

## Licença

MIT © 2026 Vitor Azevedo Pereira

## Reproducibility

Este pacote é o componente computacional do working paper:

> Pereira, V. A. (2026). Indicadores de fluxo escolar via painel longitudinal da PNAD Contínua: três décadas depois do PROFLUXO. Working paper UFRJ-IE.

Os dados, código original em Stata, e o paper LaTeX estão disponíveis em:
https://github.com/vitpereira/Nota_PNAD

## Contribuindo

Pull requests são bem-vindos. Issues, sugestões de melhorias e relatos de
bug devem ser registrados em https://github.com/vitpereira/fluxoescolar/issues.
