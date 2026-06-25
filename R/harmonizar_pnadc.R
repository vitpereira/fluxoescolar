#' Harmonizar variaveis da PNADC entre versoes
#'
#' Cria variaveis padronizadas a partir dos microdados trimestrais.
#' Trata da mudanca de nomenclatura entre versoes: V3003 → V3003A em 2016,
#' V3001 → V3001A em 2016. Calcula a renda dom per capita a partir de
#' V403312 (rendimento individual do trabalho).
#'
#' @param df data.table ou data.frame de uma rodada da PNADC trimestral
#'   (output de \code{\link{parsear_pnadc}}).
#'
#' @return data.table com colunas harmonizadas:
#' \itemize{
#'   \item \code{hh_id}: identificador domiciliar estavel
#'   \item \code{Ano, Trimestre, visita}: tempo e visita do painel
#'   \item \code{freq_escola}: frequenta escola? (1 sim, 0 nao)
#'   \item \code{sexo, idade, raca}: demografia
#'   \item \code{rede}: rede da escola (1 privada, 2-4 publica)
#'   \item \code{etapa}: curso/etapa (1-9)
#'   \item \code{serie}: ano/serie (1-9 EF, 1-3 EM)
#'   \item \code{etapa_consolid}: codigo consolidado (4=EF iniciais, 5=EF finais,
#'                                10-12=EM, 20-21=EJA)
#'   \item \code{renda_dom_pc}: renda dom. PC calculada
#'   \item \code{peso_v1028}: peso amostral
#' }
#'
#' @details
#' A consolidacao de etapa segue a convencao:
#' \itemize{
#'   \item 30 = Creche/Pre/Classe alfabetizacao
#'   \item 4 = EF Regular anos iniciais (1o-5o)
#'   \item 5 = EF Regular anos finais (6o-9o)
#'   \item 10, 11, 12 = 1o, 2o, 3o EM Regular
#'   \item 20 = EJA Fundamental
#'   \item 21 = EJA Medio
#' }
#'
#' @examples
#' \dontrun{
#' df <- readRDS("pnadc_012023.rds")
#' df_h <- harmonizar_pnadc(df)
#' }
#'
#' @export
harmonizar_pnadc <- function(df) {

    data.table::setDT(df)
    UF <- UPA <- V1008 <- V1014 <- V1016 <- V1028 <- NULL
    V2003 <- V2007 <- V2009 <- V2010 <- NULL
    V3001 <- V3001A <- V3002 <- V3002A <- NULL
    V3003 <- V3003A <- V3006 <- V403312 <- V2001 <- Ano <- Trimestre <- NULL

    # Identificador domiciliar
    df[, hh_id := paste0(
        sprintf("%02d", UF), "_",
        sprintf("%09s", UPA), "_",
        sprintf("%02d", V1008), "_",
        sprintf("%02d", V1014)
    )]

    # Variaveis nucleares
    df[, freq_escola := as.integer(V3002 == 1)]
    df[, sexo := as.integer(V2007)]
    df[, idade := as.integer(V2009)]
    df[, raca := as.integer(V2010)]
    df[, rede := as.integer(V3002A)]
    df[, visita := as.integer(V1016)]
    df[, peso_v1028 := as.numeric(V1028)]

    # Etapa harmonizada (V3003 pre-2016, V3003A pos-2016)
    if ("V3003" %in% names(df) & "V3003A" %in% names(df)) {
        df[, etapa := ifelse(Ano <= 2015,
                             as.integer(V3003),
                             as.integer(V3003A))]
    } else if ("V3003A" %in% names(df)) {
        df[, etapa := as.integer(V3003A)]
    } else if ("V3003" %in% names(df)) {
        df[, etapa := as.integer(V3003)]
    } else {
        df[, etapa := NA_integer_]
    }

    # Serie
    df[, serie := as.integer(V3006)]

    # Le-escreve (V3001 pre-2016, V3001A pos-2016)
    if ("V3001" %in% names(df) & "V3001A" %in% names(df)) {
        df[, le_escreve := ifelse(Ano <= 2015,
                                  as.integer(V3001),
                                  as.integer(V3001A))]
    }

    # Renda dom PC (manual, somando V403312 dentro do domicilio-trimestre)
    df[, renda_trab_ind := ifelse(is.na(V403312), 0, as.numeric(V403312))]
    df[, hh_yr := paste0(hh_id, "_", Ano, "Q", Trimestre)]
    df[, renda_dom_total := sum(renda_trab_ind, na.rm = TRUE), by = hh_yr]
    df[, renda_dom_pc := renda_dom_total / V2001]
    df[, c("hh_yr", "renda_dom_total", "renda_trab_ind") := NULL]

    # Etapa consolidada
    df[, etapa_consolid := NA_integer_]
    df[etapa %in% c(1, 2, 3), etapa_consolid := 30L]
    df[etapa == 4 & serie %in% 1:5,  etapa_consolid := 4L]
    df[etapa == 4 & serie %in% 6:9,  etapa_consolid := 5L]
    df[etapa == 6 & serie == 1, etapa_consolid := 10L]
    df[etapa == 6 & serie == 2, etapa_consolid := 11L]
    df[etapa == 6 & serie == 3, etapa_consolid := 12L]
    df[etapa == 5, etapa_consolid := 20L]
    df[etapa == 7, etapa_consolid := 21L]

    # Filtros
    df <- df[idade >= 4 & idade <= 24 & !is.na(peso_v1028)]

    # Manter so colunas core
    keep_cols <- c("hh_id", "Ano", "Trimestre", "visita",
                   "UF", "UPA", "V1008", "V1014", "V2003",
                   "freq_escola", "sexo", "idade", "raca", "rede",
                   "etapa", "etapa_consolid", "serie",
                   "renda_dom_pc", "peso_v1028")
    keep_cols <- intersect(keep_cols, names(df))
    df[, ..keep_cols]
}
