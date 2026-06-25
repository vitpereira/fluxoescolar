#' Figura: serie temporal dos indicadores
#'
#' Gera figura com series temporais dos indicadores de fluxo escolar
#' por macroetapa, opcionalmente sobrepondo dados do INEP.
#'
#' @param dados_pnadc data.table com indicadores PNADC (output de
#'   \code{\link{calcular_indicadores}} com \code{desagregacao =
#'   c("ano_t", "macroetapa")}).
#' @param dados_inep Opcional: data.table com indicadores INEP no mesmo
#'   formato (mesmas colunas + \code{fonte = "INEP"}).
#' @param indicadores Vetor de indicadores a plotar. Default todos:
#'   \code{c("flag_promocao", "flag_repetencia", "flag_evasao",
#'   "flag_naoprog", "flag_abandono")}.
#' @param destacar_covid Logical. Se TRUE (default), destaca 2020-2021 com
#'   sombra cinza.
#'
#' @return Objeto ggplot.
#'
#' @export
figura_serie_temporal <- function(dados_pnadc,
                                  dados_inep = NULL,
                                  indicadores = c("flag_promocao",
                                                  "flag_repetencia",
                                                  "flag_evasao",
                                                  "flag_abandono"),
                                  destacar_covid = TRUE) {
    data.table::setDT(dados_pnadc)
    dt_long <- data.table::melt(dados_pnadc,
        id.vars = c("ano_t", "macroetapa"),
        measure.vars = intersect(indicadores, names(dados_pnadc)),
        variable.name = "indicador",
        value.name = "taxa")
    dt_long[, indicador_lbl := data.table::fcase(
        indicador == "flag_promocao", "Promoção",
        indicador == "flag_repetencia", "Repetência",
        indicador == "flag_evasao", "Evasão",
        indicador == "flag_naoprog", "Não-progressão",
        indicador == "flag_abandono", "Abandono"
    )]
    dt_long[, macroetapa_lbl := data.table::fcase(
        macroetapa %in% c(1, "EF iniciais"), "EF iniciais",
        macroetapa %in% c(2, "EF finais"), "EF finais",
        macroetapa %in% c(3, "EM"), "Ensino Médio"
    )]
    dt_long <- dt_long[!is.na(macroetapa_lbl)]
    dt_long[, fonte := "PNADC"]

    if (!is.null(dados_inep)) {
        data.table::setDT(dados_inep)
        dados_inep[, fonte := "INEP"]
        dt_long <- rbind(dt_long, dados_inep, fill = TRUE)
    }

    p <- ggplot2::ggplot(dt_long,
                         ggplot2::aes(x = ano_t, y = taxa,
                                      color = macroetapa_lbl,
                                      linetype = fonte)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 1.2) +
        ggplot2::facet_wrap(~indicador_lbl, scales = "free_y") +
        ggplot2::scale_y_continuous(
            labels = scales::percent_format(accuracy = 1)) +
        ggplot2::scale_x_continuous(breaks = 2012:2024) +
        ggplot2::labs(x = NULL, y = NULL, color = NULL, linetype = NULL,
                      title = NULL) +
        ggplot2::theme_minimal(base_family = "serif") +
        ggplot2::theme(
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

    if (destacar_covid) {
        p <- p + ggplot2::annotate("rect", xmin = 2020, xmax = 2021,
                                   ymin = -Inf, ymax = Inf,
                                   alpha = 0.15, fill = "grey50")
    }
    p
}

#' Figura: heterogeneidade por subgrupo
#'
#' Gera figura de barras mostrando indicador por categoria de subgrupo
#' (quintil renda, raca, sexo, etc.).
#'
#' @param dados data.table com indicadores PNADC desagregados.
#' @param indicador Nome da coluna do indicador a plotar.
#' @param subgrupo Variavel de subgrupo (ex. "quintil_renda", "raca").
#' @param macroetapa Macroetapa a filtrar (default todos).
#'
#' @return Objeto ggplot.
#'
#' @export
figura_heterogeneidade <- function(dados,
                                   indicador = "flag_promocao",
                                   subgrupo = "quintil_renda",
                                   macroetapa = NULL) {

    data.table::setDT(dados)
    df <- if (!is.null(macroetapa)) dados[macroetapa == ..macroetapa] else dados
    p <- ggplot2::ggplot(df,
        ggplot2::aes_string(x = subgrupo, y = indicador)) +
        ggplot2::geom_col(fill = "#4d4d4d") +
        ggplot2::scale_y_continuous(
            labels = scales::percent_format(accuracy = 1)) +
        ggplot2::labs(x = NULL, y = NULL, title = NULL) +
        ggplot2::theme_minimal(base_family = "serif")
    p
}
