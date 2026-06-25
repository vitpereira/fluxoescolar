#' Decompor a diferenca PNADC × INEP em 5 fontes
#'
#' Para cada indicador-grupo, computa Delta = INEP - PNADC e decompoe em:
#' R (retorno: PNADC capta), U (universo), S (sampling), C (sub-cobertura
#' do Censo), M (medida residual = Delta - R - U - S - C).
#'
#' @param resultados_pnadc data.table com indicadores PNADC (output de
#'   \code{\link{calcular_indicadores}}).
#' @param dados_inep data.table com taxas oficiais INEP (vindas do INEP
#'   gov.br/inep). Colunas esperadas: \code{ano, etapa, indicador, valor}.
#' @param captura_retorno data.table com fracao de mudanca de rede ou
#'   modalidade entre t e t+1, por subgrupo.
#'
#' @return data.table com colunas \code{ano, etapa, indicador,
#'   pnadc_valor, inep_valor, delta, R, U, S, C, M}.
#'
#' @details
#' A decomposicao e uma identidade contabil descritiva, nao causal:
#' \deqn{\Delta = R + U + S + C + M}
#' M e definido por construcao como o residual e captura erros de
#' classificacao, diferencas entre semana de referencia da PNADC e
#' ano letivo do Censo, e demais discrepancias nao explicitadas.
#'
#' @export
decompor_inep <- function(resultados_pnadc,
                          dados_inep,
                          captura_retorno = NULL) {

    data.table::setDT(resultados_pnadc)
    data.table::setDT(dados_inep)

    # Merge PNADC com INEP por ano, etapa, indicador
    comp <- merge(resultados_pnadc, dados_inep,
                  by = c("ano", "etapa", "indicador"),
                  all = TRUE)
    comp[, delta := inep_valor - pnadc_valor]

    # Componente R: vem de captura_retorno
    if (!is.null(captura_retorno)) {
        data.table::setDT(captura_retorno)
        comp <- merge(comp, captura_retorno,
                      by = c("etapa"), all.x = TRUE)
        comp[, R := mudou_rede + ifelse(is.na(mudou_modalidade),
                                        0, mudou_modalidade)]
    } else {
        comp[, R := NA_real_]
    }

    # Componentes U, S, C, M (placeholders - precisam de info adicional)
    comp[, U := NA_real_]
    comp[, S := NA_real_]
    comp[, C := NA_real_]
    comp[, M := delta - R - U - S - C]
    comp[]
}
