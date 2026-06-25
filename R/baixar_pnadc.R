#' Baixar microdados PNADC trimestral do FTP do IBGE
#'
#' Faz download de todos os arquivos .zip da PNAD Continua trimestral entre
#' os anos especificados. Cada trimestre tem cerca de 200 MB compactados.
#' Total para 2012-2024: ~11 GB. Tambem baixa o input SAS e o dicionario.
#'
#' @param dir_destino Caminho da pasta onde salvar os zips. Sera criada se
#'   nao existir. Dentro dela, sera criada uma subpasta \code{docs/} para
#'   o input SAS e dicionario.
#' @param anos Vetor de anos (2012-2024). Default: \code{2012:2024}.
#' @param trimestres Vetor de trimestres (1-4). Default: \code{1:4}.
#' @param skip_existing Logical. Se \code{TRUE} (default), pula arquivos que
#'   ja existam no destino.
#' @param verbose Logical. Se \code{TRUE} (default), imprime progresso.
#'
#' @return Invisivelmente, retorna um data.frame com as colunas
#'   \code{ano}, \code{trimestre}, \code{file}, \code{size_mb}, \code{status}.
#'
#' @details
#' As URLs do IBGE seguem o padrao
#' \code{ftp.ibge.gov.br/.../Trimestral/Microdados/<ANO>/PNADC_<TT><ANO>_<DATE>.zip}.
#' O nome inclui um sufixo de data que varia por trimestre (revisao do IBGE).
#' Esta funcao detecta automaticamente o nome correto via listagem do FTP.
#'
#' @examples
#' \dontrun{
#' log <- baixar_pnadc(dir_destino = "C:/dados/pnadc", anos = 2023:2024)
#' }
#'
#' @export
baixar_pnadc <- function(dir_destino,
                         anos = 2012:2024,
                         trimestres = 1:4,
                         skip_existing = TRUE,
                         verbose = TRUE) {

    fs::dir_create(dir_destino)
    docs_dir <- file.path(dir_destino, "docs")
    fs::dir_create(docs_dir)

    base_url <- paste0(
        "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/",
        "Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/",
        "Trimestral/Microdados"
    )
    docs_url <- paste0(base_url, "/Documentacao/")

    if (verbose) message("Baixando documentacao (input SAS + dicionario)...")
    dict_zip <- file.path(docs_dir, "Dicionario_e_input.zip")
    if (!fs::file_exists(dict_zip) || !skip_existing) {
        utils::download.file(
            paste0(docs_url, "Dicionario_e_input_20221031.zip"),
            destfile = dict_zip,
            mode = "wb",
            quiet = !verbose
        )
        utils::unzip(dict_zip, exdir = docs_dir)
    }

    listar_year <- function(ano) {
        url <- paste0(base_url, "/", ano, "/")
        html <- readLines(url, warn = FALSE)
        links <- regmatches(html, regexpr('PNADC_\\d{6}_\\d{8}\\.zip', html))
        unique(links)
    }

    log_rows <- list()

    for (ano in anos) {
        if (verbose) message(glue::glue("Ano {ano}..."))
        files <- tryCatch(listar_year(ano), error = function(e) character(0))

        for (q in trimestres) {
            padrao <- sprintf("PNADC_%02d%d", q, ano)
            match <- files[grep(padrao, files)]
            if (length(match) == 0) {
                log_rows[[length(log_rows) + 1]] <- data.frame(
                    ano = ano, trimestre = q, file = NA_character_,
                    size_mb = 0, status = "not_found"
                )
                next
            }
            fname <- sort(match, decreasing = TRUE)[1]
            url <- paste0(base_url, "/", ano, "/", fname)
            dest <- file.path(dir_destino, sprintf("PNADC_%02d%d.zip", q, ano))

            if (fs::file_exists(dest) && skip_existing) {
                sz <- file.info(dest)$size / 1e6
                log_rows[[length(log_rows) + 1]] <- data.frame(
                    ano = ano, trimestre = q, file = fname,
                    size_mb = sz, status = "skipped"
                )
                next
            }

            if (verbose) message(glue::glue("  {ano}Q{q}: baixando {fname}"))
            tryCatch({
                utils::download.file(url, destfile = dest, mode = "wb",
                                     quiet = !verbose)
                sz <- file.info(dest)$size / 1e6
                log_rows[[length(log_rows) + 1]] <- data.frame(
                    ano = ano, trimestre = q, file = fname,
                    size_mb = sz, status = "downloaded"
                )
            }, error = function(e) {
                log_rows[[length(log_rows) + 1]] <<- data.frame(
                    ano = ano, trimestre = q, file = fname,
                    size_mb = 0, status = paste0("error: ", conditionMessage(e))
                )
            })
        }
    }

    log_df <- do.call(rbind, log_rows)
    if (verbose) {
        n_ok <- sum(log_df$status %in% c("downloaded", "skipped"))
        message(glue::glue("Concluido: {n_ok}/{nrow(log_df)} arquivos OK"))
    }
    invisible(log_df)
}
