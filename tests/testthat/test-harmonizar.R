test_that("harmonizar_pnadc cria as variaveis nucleares", {
    df_mock <- data.table::data.table(
        Ano = 2023L, Trimestre = 1L,
        UF = c(11L, 11L, 33L),
        UPA = c("000000001", "000000001", "000000002"),
        V1008 = c(1L, 1L, 5L),
        V1014 = c(1L, 1L, 2L),
        V1016 = c(1L, 1L, 1L),
        V1028 = c(100.0, 95.0, 80.0),
        V2001 = c(3L, 3L, 4L),
        V2003 = c(1L, 2L, 1L),
        V2007 = c(1L, 2L, 1L),
        V2009 = c(15L, 12L, 8L),
        V2010 = c(1L, 1L, 2L),
        V3002 = c(1L, 1L, 1L),
        V3002A = c(2L, 2L, 1L),
        V3003A = c(4L, 4L, 4L),
        V3006 = c(8L, 6L, 3L),
        V403312 = c(1500, NA, 2000)
    )
    df_h <- harmonizar_pnadc(df_mock)

    expect_true("hh_id" %in% names(df_h))
    expect_true("freq_escola" %in% names(df_h))
    expect_true("etapa_consolid" %in% names(df_h))
    expect_true("renda_dom_pc" %in% names(df_h))

    # 8o ano EF (etapa=4, serie=8) -> EF finais (5)
    expect_equal(df_h$etapa_consolid[df_h$V2003 == 1 & df_h$idade == 15], 5L)
    # 6o ano EF (etapa=4, serie=6) -> EF finais (5)
    expect_equal(df_h$etapa_consolid[df_h$idade == 12], 5L)
    # 3o ano EF (etapa=4, serie=3) -> EF iniciais (4)
    expect_equal(df_h$etapa_consolid[df_h$idade == 8], 4L)
})

test_that("harmonizar_pnadc filtra idade 4-24", {
    df_mock <- data.table::data.table(
        Ano = 2023L, Trimestre = 1L,
        UF = rep(11L, 3), UPA = rep("000000001", 3),
        V1008 = rep(1L, 3), V1014 = rep(1L, 3),
        V1016 = rep(1L, 3), V1028 = rep(100, 3),
        V2001 = rep(3L, 3), V2003 = 1:3,
        V2007 = c(1L, 2L, 1L), V2009 = c(2L, 15L, 30L),  # 2 (out), 15 (in), 30 (out)
        V2010 = rep(1L, 3),
        V3002 = c(NA, 1L, 2L),
        V3002A = c(NA, 2L, NA),
        V3003A = c(NA, 4L, NA),
        V3006 = c(NA, 8L, NA),
        V403312 = c(NA, NA, 3000)
    )
    df_h <- harmonizar_pnadc(df_mock)
    # Apenas o de 15 anos deve sobrar
    expect_equal(nrow(df_h), 1L)
    expect_equal(df_h$idade[1], 15L)
})
