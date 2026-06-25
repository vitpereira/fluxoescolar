#' Construir painel longitudinal individual da PNADC
#'
#' Liga individuos atraves das cinco visitas do painel da PNADC trimestral
#' usando matching baseado em \code{hh_id + V2003} com validacao por sexo
#' e idade. Indivíduos com sexo inconsistente ou idade incompatível entre
#' visitas sao marcados como "elo quebrado" e excluidos.
#'
#' @param df data.table harmonizado de varios trimestres (output de
#'   \code{\link{harmonizar_pnadc}} aplicado a multiplos anos e
#'   \code{rbind}-eados).
#' @param min_visitas Minimo de visitas necessario para o indivíduo entrar
#'   no painel. Default \code{2}.
#' @param sexo_tol Fracao minima de visitas com sexo consistente. Default
#'   \code{0.8}.
#' @param idade_tol Fracao minima de transicoes com idade compativel.
#'   Default \code{0.8}.
#' @param verbose Logical.
#'
#' @return data.table com coluna adicional \code{person_id} (unico por
#'   pessoa-painel) para individuos linkados, e \code{link_ok} (1/0).
#'   Individuos com \code{link_ok=0} podem ser descartados pelo usuario
#'   se preferir o painel mais restrito.
#'
#' @details
#' Estrategia em 3 camadas inspirada na metodologia Ribas-Soares (2008):
#' \enumerate{
#'   \item Match preliminar por \code{hh_id + V2003}
#'   \item Validacao: \code{V2007} (sexo) estavel em >=80% das visitas;
#'         \code{V2009} (idade) cresce ~0 ou 1 ano entre visitas consecutivas
#'         (tolerancia +/- 1)
#'   \item Reconciliacao: se mismatch persiste, marcar como "elo quebrado"
#' }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' df_all <- rbindlist(list(
#'     harmonizar_pnadc(readRDS("pnadc_012023.rds")),
#'     harmonizar_pnadc(readRDS("pnadc_022023.rds")),
#'     harmonizar_pnadc(readRDS("pnadc_032023.rds"))
#' ))
#' painel <- construir_painel(df_all)
#' painel[link_ok == 1, .N]   # contagem de obs linkadas
#' }
#'
#' @export
construir_painel <- function(df,
                             min_visitas = 2,
                             sexo_tol = 0.8,
                             idade_tol = 0.8,
                             verbose = TRUE) {

    data.table::setDT(df)
    hh_id <- V2003 <- sexo <- idade <- visita <- NULL
    n_visitas <- frac_sexo_ok <- frac_idade_ok <- NULL
    sexo_mode <- sexo_consistent <- delta_idade <- idade_ok <- NULL

    df[, pid_provis := paste0(hh_id, "_", sprintf("%02d", V2003))]
    df[, n_visitas := .N, by = pid_provis]

    # Validacao sexo: mode dentro do pid
    sexo_dt <- df[, .(sexo_mode = data.table::first(
        data.table::frank(table(sexo), ties.method = "first")
    )), by = pid_provis]
    # Mais simples: pegar moda
    df[, sexo_mode := mode_chr(sexo), by = pid_provis]
    df[, sexo_consistent := (sexo == sexo_mode)]
    df[, frac_sexo_ok := mean(sexo_consistent, na.rm = TRUE), by = pid_provis]

    # Validacao idade
    data.table::setorder(df, pid_provis, visita)
    df[, delta_idade := idade - data.table::shift(idade), by = pid_provis]
    df[, idade_ok := delta_idade >= 0 & delta_idade <= 2]
    df[, frac_idade_ok := mean(idade_ok, na.rm = TRUE), by = pid_provis]

    # Link OK
    df[, link_ok := as.integer(
        n_visitas >= min_visitas &
        (is.na(frac_sexo_ok) | frac_sexo_ok >= sexo_tol) &
        (is.na(frac_idade_ok) | frac_idade_ok >= idade_tol)
    )]
    df[is.na(link_ok), link_ok := 0L]
    df[link_ok == 0, link_ok := 0L]

    df[, person_id := ifelse(link_ok == 1, pid_provis, NA_character_)]

    if (verbose) {
        n_link <- df[link_ok == 1, .N]
        n_no <- df[link_ok == 0, .N]
        message(glue::glue("Linkados: {n_link}; Nao-linkados: {n_no}"))
    }

    # Limpar columns auxiliares
    df[, c("sexo_mode", "sexo_consistent", "delta_idade", "idade_ok",
           "frac_sexo_ok", "frac_idade_ok") := NULL]
    df
}

#' @keywords internal
#' @noRd
mode_chr <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(NA)
    tab <- tabulate(match(x, ux))
    ux[which.max(tab)]
}
