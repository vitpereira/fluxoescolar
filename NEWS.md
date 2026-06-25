# fluxoescolar 0.1.0 (2026-06-25)

## Primeira versão pública

* Funções para download (`baixar_pnadc`), parsing (`parsear_pnadc`), e
  harmonização (`harmonizar_pnadc`) dos microdados PNADC trimestral 2012-2024.
* `construir_painel()` linka indivíduos entre as 5 visitas do painel rotativo
  via matching por `hh_id + V2003` com validação por sexo e idade.
* `calcular_indicadores()` produz os cinco indicadores de fluxo escolar
  (abandono, evasão, promoção, repetência, não-progressão) com desagregação
  por etapa, série, sexo, raça, quintil de renda, perfil CadÚnico-proxy,
  rede, e UF.
* `decompor_inep()` decompõe a diferença entre PNADC e INEP em fontes
  R (retorno), U (universo), S (sampling), C (sub-cobertura) e M (residual).
* `figura_serie_temporal()` e `figura_heterogeneidade()` para visualização.
* Testes unitários para `harmonizar_pnadc()` e `construir_painel()`.
* Vignette `getting-started.Rmd` com pipeline completo.
