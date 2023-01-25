rcode2
================
2023-01-25

``` r
# install and load packages ###
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

    ## Warning: package 'pacman' was built under R version 4.1.3

``` r
#Use pacman to load packages I want##

pacman::p_load(pacman, knitr, rmarkdown, rio, naniar, lme4, merTools, jtools, lmerTest, reghelper, gitcreds, dplyr, car, psych, psychTools)

pacman::p_load(pacman, rio, naniar, lme4, merTools, jtools, lmerTest, reghelper, gitcreds, dplyr, car)
```

``` r
#importing data
PHData <-import("C:/Users/Owner/Downloads/Personality and Health MERGED  (1).xlsx")
options(digits=6)
head(PHData)
```

    ##   ID_Number Gender Age Stud_Status Program_Year Year_School Work_Status
    ## 1         1      2  18           1            1          12           3
    ## 2         2      2  18           1            1          12           3
    ## 3         3      2  21           1            3          14           1
    ## 4         4      1  18           1            1          12           1
    ## 5         5      1  19           1            2          13           3
    ## 6         6      1  18           1            1          12           3
    ##   Religion                                            Religion2 Ethnicity
    ## 1        2                                              Baptist         3
    ## 2        2                        A slightly faithful believer.         6
    ## 3        3 I go to church every week for band but not religious         2
    ## 4        7                                                  -99         5
    ## 5        4                                                  -99         6
    ## 6        2                                         Presbyterian         6
    ##   Ethnicity2                                    Ethnicity3 USCitizen T1_EDEQ_1
    ## 1        -99                                           -99         1         1
    ## 2        -99                                           -99         1         6
    ## 3          2                                           -99         2         2
    ## 4        -99 Half white, Half Malaysian (South East Asian)         1         3
    ## 5        -99                                           -99         1         7
    ## 6        -99                                           -99         1         1
    ##   T1_EDEQ_2 T1_EDEQ_3 T1_EDEQ_4 T1_EDEQ_5 T1_EDEQ_6 T1_EDEQ_7 T1_EDEQ_8
    ## 1         1         1         1         1         1         1         1
    ## 2         1         7         7         1         4         2         1
    ## 3         1         1         1         1         1         1         1
    ## 4         1         3         3         1         3         1         1
    ## 5         1         7         1         1         1         1         2
    ## 6         1         1         1         1         1         1         2
    ##   T1_EDEQ_9 T1_EDEQ_10 T1_EDEQ_11 T1_EDEQ_12 T1_EDEQ_13 T1_EDEQ_14 T1_EDEQ_15
    ## 1         1          1          3          1          1          1          1
    ## 2         1          7          4          4          7          5          4
    ## 3         1          7          1          1          1          1          3
    ## 4         1          1          1          1          1          3          1
    ## 5         1          7          1          1          1          1          7
    ## 6         1          4          1          1          2          1          1
    ##   T1_EDEQ_16 T1_EDEQ_17A T1_EDEQ_17B T1_EDEQ_17C T1_EDEQ_18A T1_EDEQ_18B
    ## 1          1           1           0           0           1           0
    ## 2          3           1         -99         -99           2           2
    ## 3          1           1         -99         -99           1         -99
    ## 4          2           2           0           0           1         -99
    ## 5          1           1           0           0           1           0
    ## 6          1           2           2           0           1           0
    ##   T1_EDEQ_19A T1_EDEQ_19B T1_EDEQ_20A T1_EDEQ_20B T1_EDEQ_21A T1_EDEQ_21B
    ## 1           1           0           1           0           1           0
    ## 2           1         -99           1         -99           1         -99
    ## 3           1         -99           1         -99           1         -99
    ## 4           1         -99           1         -99           1         -99
    ## 5           1           0           1           0           1           0
    ## 6           1           0           1           0           1           0
    ##   T1_EDEQ_22A T1_EDEQ_22B T1_EDEQ_23 T1_EDEQ_24 T1_EDEQ_25 T1_EDEQ_26
    ## 1           1           0          1          1          1          2
    ## 2           1         -99          3          1          3          2
    ## 3           1         -99          1          1          2          1
    ## 4           1         -99          1          1          1          1
    ## 5           2           3          1          2          1          1
    ## 6           1           2          1          2          1          3
    ##   T1_EDEQ_27 T1_EDEQ_28 T1_EDEQ_29 T1_EDEQ_30 T1_EDEQ_31 T1_EDEQ_32 T1_EDEQ_33
    ## 1          1          1          1          5          1          1          2
    ## 2          1          4          2          6          3          4          5
    ## 3          2          2          2          2          1          1          2
    ## 4          1          1          2          2          1          1          1
    ## 5          1          2          2          3          1          1          1
    ## 6          1          1          2          3          1          1          1
    ##   T1_EDEQ_34A
    ## 1           2
    ## 2           1
    ## 3           2
    ## 4           1
    ## 5           2
    ## 6           1
    ##                                                                                                                                        T1_EDEQ_34B
    ## 1                                                                                                                                              n/a
    ## 2 About half the time I was very significantly focused on weight, calories, shape, etc, and about half the time I was more involved in-the-moment.
    ## 3                                                                                                                                              -99
    ## 4   The last week consisted of a much more disjointed eating schedule due to insufficient time. Staying busy also cuts down binge eating episodes.
    ## 5                                                                                                                                              -99
    ## 6                                                                                                                    Usually I would work out more
    ##   T1_Height T1_Weight T1_EDI_1 T1_EDI_2 T1_EDI_3 T1_EDI_4 T1_EDI_5 T1_EDI_6
    ## 1        69       145        4        3        1        1        2        2
    ## 2        69       143        4        3        3        4        1        6
    ## 3        64       115        4        2        1        3        2        2
    ## 4        70       175        2        1        1        2        1        3
    ## 5        71       170        4        3        3        5        1        1
    ## 6        70       145        6        2        1        1        3        1
    ##   T1_EDI_7 T1_EDI_8 T1_EDI_9 T1_EDI_10 T1_EDI_11 T1_EDI_12 T1_EDI_13 T1_EDI_14
    ## 1        1        1        5         1         1         5         1         1
    ## 2        6        2        1         6         1         3         5         1
    ## 3        3        2        4         2         2         5         2         1
    ## 4        1        1        6         1         2         3         4         2
    ## 5        5        1        3         1         1         5         3         1
    ## 6        1        1        4         2         3         5         1         1
    ##   T1_EDI_15 T1_EDI_16 T1_EDI_17 T1_EDI_18 T1_EDI_19 T1_EDI_20 T1_EDI_21
    ## 1         6         1         1         1         1         1         5
    ## 2         4         4         2         1         6         1         1
    ## 3         5         2         2         1         1         1         5
    ## 4         2         1         1         1         1         1         2
    ## 5         5         4         1         1         1         1         5
    ## 6         6         2         1         1         1         1         4
    ##   T1_EDI_22 T1_EDI_23 T1_SAAS_1 T1_SAAS_2 T1_SAAS_3 T1_SAAS_4 T1_SAAS_5
    ## 1         1         6         5         1         1         2         1
    ## 2         1         5         3         4         4         4         2
    ## 3         1         5         3         4         4         2         1
    ## 4         3         4         4         3         3         1         1
    ## 5         1         6         4         3         2         2         2
    ## 6         1         4         5         1         1         1         3
    ##   T1_SAAS_6 T1_SAAS_7 T1_SAAS_8 T1_SAAS_9 T1_SAAS_10 T1_SAAS_11 T1_SAAS_12
    ## 1         2         1         4         3          1          1          1
    ## 2         5         4         2         1          3          4          5
    ## 3         2         2         2         2          2          4          1
    ## 4         1         1         2         1          1          1          1
    ## 5         2         2         1         1          2          2          1
    ## 6         2         4         2         1          1          3          2
    ##   T1_SAAS_13 T1_SAAS_14 T1_SAAS_15 T1_SAAS_16 T1_FMPS_1 T1_FMPS_2 T1_FMPS_3
    ## 1          2          1          1          1         4         5         2
    ## 2          5          4          3          5         3         5         1
    ## 3          2          3          1          1         4         2         4
    ## 4          1          2          1          1         1         5         1
    ## 5          1          2          3          3         4         4         1
    ## 6          3          3          2          4         4         2         1
    ##   T1_FMPS_4 T1_FMPS_5 T1_FMPS_6 T1_FMPS_7 T1_FMPS_8 T1_FMPS_9 T1_FMPS_10
    ## 1         3         1         2         5         5         1          1
    ## 2         4         2         5         4         5         3          5
    ## 3         3         4         3         2         3         2          4
    ## 4         2         1         5         5         5         2          4
    ## 5         3         1         5         5         5         3          3
    ## 6         2         1         5         3         4         2          3
    ##   T1_FMPS_11 T1_FMPS_12 T1_FMPS_13 T1_FMPS_14 T1_FMPS_15 T1_FMPS_16 T1_FMPS_17
    ## 1          2          5          1          1          3          5          1
    ## 2          1          4          1          2          2          5          2
    ## 3          3          3          3          4          4          2          5
    ## 4          1          5          1          1          1          5          3
    ## 5          2          5          3          3          3          5          2
    ## 6          2          5          2          3          2          5          2
    ##   T1_FMPS_18 T1_FMPS_19 T1_FMPS_20 T1_FMPS_21 T1_FMPS_22 T1_FMPS_23 T1_FMPS_24
    ## 1          1          4          3          2          1          1          4
    ## 2          3          5          4          4          1          2          4
    ## 3          5          3          5          4          5          3          2
    ## 4          4          4          1          4          1          1          3
    ## 5          3          5          3          1          1          1          4
    ## 6          4          5          4          2          3          2          5
    ##   T1_FMPS_25 T1_FMPS_26 T1_FMPS_27 T1_FMPS_28 T1_FMPS_29 T1_FMPS_30 T1_FMPS_31
    ## 1          1          1          5          2          4          2          5
    ## 2          3          1          5          3          4          4          4
    ## 3          4          5          2          2          3          3          2
    ## 4          1          1          5          5          5          4          5
    ## 5          2          3          5          2          4          5          5
    ## 6          2          2          4          1          2          5          3
    ##   T1_FMPS_32 T1_FMPS_33 T1_FMPS_34 T1_FMPS_35 T1_SSES_1 T1_SSES_2 T1_SSES_3
    ## 1          1          1          1          1         5         1         5
    ## 2          1          5          2          1         3         4         3
    ## 3          2          2          2          5         3         4         3
    ## 4          1          3          2          1         4         1         3
    ## 5          3          4          1          1         5         1         4
    ## 6          1          3          2          3         4         4         4
    ##   T1_SSES_4 T1_SSES_5 T1_SSES_6 T1_SSES_7 T1_SSES_8 T1_SSES_9 T1_SSES_10
    ## 1         1         1         4         1         1         5          1
    ## 2         5         1         4         2         5         4          3
    ## 3         3         3         2         2         5         3          3
    ## 4         2         2         3         1         3         4          1
    ## 5         2         1         5         2         2         5          2
    ## 6         3         2         3         1         2         3          2
    ##   T1_SSES_11 T1_SSES_12 T1_SSES_13 T1_SSES_14 T1_SSES_15 T1_SSES_16 T1_SSES_17
    ## 1          5          5          2          4          1          5          3
    ## 2          3          3          5          5          2          2          5
    ## 3          2          3          3          3          3          2          4
    ## 4          4          3          2          4          1          2          3
    ## 5          4          4          2          5          1          2          3
    ## 6          3          4          5          4          3          2          2
    ##   T1_SSES_18 T1_SSES_19 T1_SSES_20 T1_DEQ_1 T1_DEQ_2 T1_DEQ_3 T1_DEQ_4 T1_DEQ_5
    ## 1          1          1          1        2        2        2        2        1
    ## 2          2          4          4        6        2        5        5        1
    ## 3          2          2          3        5        5        5        4        3
    ## 4          2          2          1        2        1        2        1        1
    ## 5          1          1          1        1        5        5        5        2
    ## 6          2          3          3        3        6        3        5        1
    ##   T1_DEQ_6 T1_DEQ_7 T1_DEQ_8 T1_DEQ_9 T1_SIAS_1 T1_SIAS_2 T1_SIAS_3 T1_SIAS_4
    ## 1        1        1        2        7         1         1         1         1
    ## 2        2        1        5        4         5         3         4         5
    ## 3        5        6        6        3         4         3         5         2
    ## 4        1        1        1        6         2       -99         3         1
    ## 5        1        1        2        7         1         1         2         2
    ## 6        7        5        6        2         3         1         3         4
    ##   T1_SIAS_5 T1_SIAS_6 T1_SIAS_7 T1_SIAS_8 T1_SIAS_9 T1_SIAS_10 T1_SIAS_11
    ## 1         5         1         1         1         5          1          5
    ## 2         1         3         5         4         1          3          1
    ## 3         2         3         3         2         1          2          2
    ## 4         4         1         2         1         2          2          3
    ## 5         2         1         2         1         4          2          4
    ## 6         2         1         2         3         2          2          2
    ##   T1_SIAS_12 T1_SIAS_13 T1_SIAS_14 T1_SIAS_15 T1_SIAS_16 T1_SIAS_17 T1_SIAS_18
    ## 1          1          1          1          1          1          1          1
    ## 2          3          1          5          5          5          4          5
    ## 3          3          3          2          3          3          2          3
    ## 4          3          1          1          3          2          3          3
    ## 5          3          1          1          2          1          1          1
    ## 6          4          4          4          4          4          2          2
    ##   T1_SIAS_19 T1_SIAS_20 T1_SPS_1 T1_SPS_2 T1_SPS_3 T1_SPS_4 T1_SPS_5 T1_SPS_6
    ## 1          1          1        1        1        1        2        1        1
    ## 2          5          1        1        1        1        4        2        5
    ## 3          2          4        4        2        1        2        1        2
    ## 4          2          1        1        1        4        1        2        3
    ## 5          1          1        1        1        1        1        1        1
    ## 6          2          3        2        1        2        1        1        2
    ##   T1_SPS_7 T1_SPS_8 T1_SPS_9 T1_SPS_10 T1_SPS_11 T1_SPS_12 T1_SPS_13 T1_SPS_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        1        1        1         1         1         3         4         2
    ## 3        3        1        1         1         1         1         2         2
    ## 4        1        1        1         1         2         2         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        1        1         1         2         2         1         1
    ##   T1_SPS_15 T1_SPS_16 T1_SPS_17 T1_SPS_18 T1_SPS_19 T1_SPS_20 T1_BFNE_1
    ## 1         1         1         1         1         1         1         1
    ## 2         3         2         2         4         1         5         3
    ## 3         2         2         2         4         1         3         3
    ## 4         2         1         1         3         1         2         2
    ## 5         1         1         1         1         1         2         1
    ## 6         2         1         2         3         1         1         4
    ##   T1_BFNE_2 T1_BFNE_3 T1_BFNE_4 T1_BFNE_5 T1_BFNE_6 T1_BFNE_7 T1_BFNE_8
    ## 1         3         1         3         1         1         2         1
    ## 2         1         3         1         3         4         3         5
    ## 3         2         2         3         3         2         2         1
    ## 4         1         1         2         1         2         2         2
    ## 5         4         1         2         2         1         5         3
    ## 6         4         4         1         3         4         1         2
    ##   T1_BFNE_9 T1_BFNE_10 T1_BFNE_11 T1_BFNE_12 T1_FPES_1 T1_FPES_2 T1_FPES_3
    ## 1         1          3          1          1         1         1         1
    ## 2         4          2          4          5         9         2         1
    ## 3         2          2          4          3         8         8         8
    ## 4         2          3          4          4         4         2         3
    ## 5         2          3          1          1         3         1         1
    ## 6         2          1          3          3         8         6         3
    ##   T1_FPES_4 T1_FPES_5 T1_FPES_6 T1_FPES_7 T1_FPES_8 T1_FPES_9 T1_FPES_10
    ## 1         3         8         8         2         1         1          1
    ## 2         6         7         8        10         5         3          2
    ## 3        10         8        10         1         1         4          1
    ## 4         2         3         5         5         2         7          2
    ## 5         1        10         5         1         4         3          1
    ## 6         4         7         4         6         4         2          4
    ##   T1_CET_1 T1_CET_2 T1_CET_3 T1_CET_4 T1_CET_5 T1_CET_6 T1_CET_7 T1_CET_8
    ## 1        4        3        4        3        3        1        1        2
    ## 2        5        2        5        5        5        3        1        3
    ## 3        5        2        5        1        3        1        1        4
    ## 4        4        2        5        1        3        2        1        4
    ## 5        4        5        2        2        2        2        5        1
    ## 6        4        4        3        2        2        1        3        2
    ##   T1_CET_9 T1_CET_10 T1_CET_11 T1_CET_12 T1_CET_13 T1_CET_14 T1_CET_15
    ## 1        1         1         1         4         3         4         2
    ## 2        1         1         1         3         3         5         1
    ## 3        2         2         1         3         3         4         2
    ## 4        1         1         2         5         3         4         2
    ## 5        3         3         2         4         5         5         2
    ## 6        1         2         1         4         4         4         2
    ##   T1_CET_16 T1_CET_17 T1_CET_18 T1_CET_19 T1_CET_20 T1_CET_21 T1_CET_22
    ## 1         1         4         1         1         1         2         1
    ## 2         1         5         3         2         1         3         3
    ## 3         2         4         1         1         2         2         1
    ## 4         1         5         3         3         1         1         1
    ## 5         3         4         5         4         2         2         3
    ## 6         1         4         2         3         1         1         2
    ##   T1_CET_23 T1_CET_24 T1_CIA_1 T1_CIA_2 T1_CIA_3 T1_CIA_4 T1_CIA_5 T1_CIA_6
    ## 1         1         1        1        1        1        1        1        1
    ## 2         1         1        2        3        2        2        1        2
    ## 3         1         4        1        1        1        1        1        1
    ## 4         1         1        1        2        1        1        1        1
    ## 5         2         4        1        1        1        1        1        1
    ## 6         1         3        1        2        1        1        1        1
    ##   T1_CIA_7 T1_CIA_8 T1_CIA_9 T1_CIA_10 T1_CIA_11 T1_CIA_12 T1_CIA_13 T1_CIA_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        2        2        1         2         2         1         1         2
    ## 3        1        1        1         1         1         1         1         1
    ## 4        1        1        2         1         2         1         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        1        1         1         2         1         1         1
    ##   T1_CIA_15 T1_CIA_16 T1_PANAS_1 T1_PANAS_2 T1_PANAS_3 T1_PANAS_4 T1_PANAS_5
    ## 1         1         1          4          2          5          1          4
    ## 2         2         3          5          3          5          2          4
    ## 3         1         1          5          2          4          1          2
    ## 4         1         1          5          2          4          1          4
    ## 5         1         2          4          2          4          2          4
    ## 6         1         2          5          5          3          5          2
    ##   T1_PANAS_6 T1_PANAS_7 T1_PANAS_8 T1_PANAS_9 T1_PANAS_10 T1_PANAS_11
    ## 1          1          1          2          5           5           2
    ## 2          2          3          2          4           3           1
    ## 3          1          3          1          4           1           2
    ## 4          1          3          1          4           4           2
    ## 5          1          1          1          4           4           2
    ## 6          1          5          1          3           2           3
    ##   T1_PANAS_12 T1_PANAS_13 T1_PANAS_14 T1_PANAS_15 T1_PANAS_16 T1_PANAS_17
    ## 1           4           1           5           2           5           4
    ## 2           5           1           5           5           3           5
    ## 3           3           1           4           4           4           3
    ## 4           2           1           4           3           4           5
    ## 5           5           1           5           2           5           5
    ## 6           4           1           4           5           5           4
    ##   T1_PANAS_18 T1_PANAS_19 T1_PANAS_20 T1_BDI_1 T1_BDI_2 T1_BDI_3 T1_BDI_4
    ## 1           2           4           1        1        1        1        1
    ## 2           4           2           4        1        2        2        1
    ## 3           3           3           3        1        2        3        1
    ## 4           2           5           3        1        1        1        1
    ## 5           1           4           1        1        1        1        1
    ## 6           4           4           5        2        2        1        1
    ##   T1_BDI_5 T1_BDI_6 T1_BDI_7 T1_BDI_8 T1_BDI_9 T1_BDI_10 T1_BDI_11 T1_BDI_12
    ## 1        1        1        1        1        1         1         1         1
    ## 2        1        1        2        1        1         3         2         1
    ## 3        1        2        1        1        1         1         1         1
    ## 4        1        1        2        1        2         1         1         1
    ## 5        1        1        1        1        1         1         2         1
    ## 6        1        1        1        2        2         2         1         1
    ##   T1_BDI_13 T1_BDI_14 T1_BDI_15 T1_BDI_16 T1_BDI_17 T1_BDI_18 T1_BDI_19
    ## 1         1         1         1         1         1         1         1
    ## 2         1         2         5         1         2         2         2
    ## 3         1         1         3         1         2         1         2
    ## 4         1         1         3         1         2         1         1
    ## 5         1         1         3         2         1         1         1
    ## 6         1         1         3         1         1         2         2
    ##   T1_BDI_20 T1_FOF_AE_1 T1_FOF_AE_2 T1_FOF_AE_3 T1_FOF_AE_4 T1_FOF_AE_5
    ## 1         1           1           1           1           1           1
    ## 2         1           5           2           4           5           5
    ## 3         2           1           2           1           1           1
    ## 4         1           1           1           1           1           1
    ## 5         1           1           1           1           1           1
    ## 6         2           1           1           1           1           1
    ##   T1_FOF_AE_6 T1_FOF_AE_7 T1_FOF_AE_8 T1_FOF_FAB_1 T1_FOF_FAB_2 T1_FOF_FAB_3
    ## 1           1           1           1            1            1            1
    ## 2           5           2           6            7            1            1
    ## 3           2           1           1            1            1            1
    ## 4           1           1           1            1            1            2
    ## 5           1           1           1            2            7            1
    ## 6           1           1           1            1            2            1
    ##   T1_FOF_FAB_4 T1_FOF_FAB_5 T1_FOF_FAB_6 T1_FOF_FAB_7 T1_FOF_FAB_8 T1_FOF_FC_1
    ## 1            1            1            3            1            1           1
    ## 2            7            1            7            1            7           5
    ## 3            3            1            1            1            2           2
    ## 4            7            1            4            1            4           1
    ## 5            7            4            7            1            7           1
    ## 6            3            1            5            1            1           2
    ##   T1_FOF_FC_2 T1_FOF_FC_3 T1_FOF_FC_4 T1_FOF_FC_5 T1_FOF_FC_6 T1_FOF_FC_7
    ## 1           1           1           1           1           1           1
    ## 2           1           2           5           5           5           3
    ## 3           1           1           1           2           1           2
    ## 4           1           1           1           2           1           1
    ## 5           1           1           1           1           1           1
    ## 6           1           1           2           1           1           1
    ##   T1_FOF_FC_8 T1_FOF_FC_9 T1_FOFS_1 T1_FOFS_2 T1_FOFS_3 T1_FOFS_4 T1_FOFS_5
    ## 1           1           1       -99       -99       -99       -99       -99
    ## 2           6           1        25        30        10         0        70
    ## 3           1           1         5       -99       -99       -99        25
    ## 4           1           1        50         0         0         0        60
    ## 5           1           1        20       -99       -99       -99        15
    ## 6           1           1         3       -99       -99       -99         7
    ##   T1_FOFS_6 T1_FOFS_7 T1_FOFS_8 T1_FOFS_9
    ## 1       -99       -99       -99       -99
    ## 2        10        15         0        10
    ## 3       -99       -99       -99       -99
    ## 4         5         0         0         0
    ## 5       -99       -99       -99       -99
    ## 6       -99       -99       -99       -99
    ##                                                                                               T1_FOFS10
    ## 1                                                                                                    no
    ## 2 Pastries, espeically large ones, beef hamburgers, bacon, and pretty much anything that is deep fried.
    ## 3                                                                                                   -99
    ## 4                                                                                                   -99
    ## 5                                                                                             Junk food
    ## 6                                                                                                    No
    ##   T1_CES_1 T1_CES_2 T1_CES_3 T1_CES_4 T1_CES_5 T1_CES_6 T1_CES_7 T1_CES_8
    ## 1        1        1        1        1        1        1        1        1
    ## 2        3        1        4        2        2        5        2        2
    ## 3        6        4        7        7        4        3        5        4
    ## 4        2        3        9        7        7        1        2        4
    ## 5        3        1        1        3        1        1        3        2
    ## 6        8        2        5        3        5        1        1        4
    ##   T1_CES_9 T1_CES_10 T1_CES_11 T1_CES_12 T1_MINI_1 T1_MINI_2 T1_MINI_3
    ## 1        1         1         1         1         5         5         4
    ## 2        5         1         9         4         1         5         1
    ## 3        7         3         2         8         1         4         2
    ## 4        1         4         4         7         1         4         4
    ## 5        1         1         1         4         4         2         4
    ## 6        6         2         1         2         4         4         2
    ##   T1_MINI_4 T1_MINI_5 T1_MINI_6 T1_MINI_7 T1_MINI_8 T1_MINI_9 T1_MINI_10
    ## 1         2         3         2         1         2         4          4
    ## 2         1         2         4         1         1         2          4
    ## 3         2         4         4         3         4         2          3
    ## 4         2         3         4         1       -99         4          2
    ## 5         2         4         4         2         1         4          2
    ## 6         2         3         2         1         4         4          2
    ##   T1_MINI_11 T1_MINI_12 T1_MINI_13 T1_MINI_14 T1_MINI_15 T1_MINI_16 T1_MINI_17
    ## 1          5          5          5          2          1          1          1
    ## 2          1          3          5          2          1          4          1
    ## 3          2          3          4          4          3          4          4
    ## 4          1          4          5          2          2          5          1
    ## 5          4          2          4          1          1          1          1
    ## 6          4          4          4          1          2          2          1
    ##   T1_MINI_18 T1_MINI_19 T1_MINI_20 T1_MINI_21 T1_MINI_22 T1_MINI_23 T1_MINI_24
    ## 1          1          3          2          4          4          4          4
    ## 2          1          4          4          3          2          5          4
    ## 3          4          3          2          3          3          2          3
    ## 4          2          4          3          4          5          4          5
    ## 5          1          5          1          4          2          5          4
    ## 6          1          2          3          5          5          4          3
    ##   T1_MINI_25 T1_MINI_26 T1_MINI_27 T1_MINI_28 T1_MINI_29 T1_MINI_30 T1_MINI_31
    ## 1          4          5          3          2          2          2          5
    ## 2        -99        -99        -99        -99        -99        -99        -99
    ## 3          4          3          3          2          4          2          3
    ## 4          5          4          1          4          1          1          5
    ## 5          1          5          2          4          2          1          5
    ## 6          5          4          2          2          4          4          5
    ##   T1_MINI_32 T1_MINI_33 T1_MINI_34 T1_MINI_35 T1_MINI_36 T1_MINI_37 T1_MINI_38
    ## 1          1          1          4          1          2          1          5
    ## 2        -99          1          2          1          4          3          5
    ## 3          3          2          4          2          5          4          3
    ## 4          1          1          5          1          2          1          4
    ## 5          4          1          2          1          1          2          5
    ## 6          2          1          5          4          2          1          4
    ##   T1_MINI_39 T1_MINI_40 T1_MINI_41 T1_MINI_42 T1_MINI_43 T1_MINI_44 T1_MINI_45
    ## 1          3          5          4          5          4          5          5
    ## 2          2          5          2          5          2          4          3
    ## 3          3          5          3          2          4          4          3
    ## 4          5          5          1          4          4          4          4
    ## 5          4          4          3          5          2          5          5
    ## 6          3          4          2          4          5          4          5
    ##   T1_MINI_46 T1_MINI_47 T1_IU_1 T1_IU_2 T1_IU_3 T1_IU_4 T1_IU_5 T1_IU_6 T1_IU_7
    ## 1          5          1       3       1       1       1       1       1       1
    ## 2          4          1       2       1       1       1       5       5       4
    ## 3          4          3       5       3       3       1       2       5       3
    ## 4          4          1       3       1       1       1       1       1       1
    ## 5          4          2       1       1       1       1       1       1       1
    ## 6          4          4       4       1       1       3       1       2       2
    ##   T1_IU_8 T1_IU_9 T1_IU_10 T1_IU_11 T1_IU_12 T1_IU_13 T1_IU_14 T1_IU_15
    ## 1       1       1        1        1        1        1        1        1
    ## 2       5       1        5        2        1        1        2        3
    ## 3       5       3        4        3        2        3        2        3
    ## 4       3       1        3        2        1        1        1        1
    ## 5       2       1        5        1        1        1        1        1
    ## 6       3       1        2        1        1        1        2        2
    ##   T1_IU_16 T1_IU_17 T1_IU_18 T1_IU_19 T1_IU_20 T1_IU_21 T1_IU_22 T1_IU_23
    ## 1        1        1        1        1        1        2        1        1
    ## 2        4        1        5        3        1        3        4        1
    ## 3        4        3        5        2        3        4        5        1
    ## 4        1        1        3        2        3        5        1        1
    ## 5        1        1        1        1        1        2        1        1
    ## 6        4        2        1        1        2        1        1        2
    ##   T1_IU_24 T1_IU_25 T1_IU_26 T1_IU_27 T1_SSGS_1 T1_SSGS_2 T1_SSGS_3 T1_SSGS_4
    ## 1        1        1        1        1         5         1         1         5
    ## 2        1        1        3        3         3         1         2         4
    ## 3        1        1        3        2         2         3         3         3
    ## 4        1        2        1        1         4         1         1         4
    ## 5        2        1        1        1         5         1         1         5
    ## 6        1        1        3        3         3         1         1         4
    ##   T1_SSGS_5 T1_SSGS_6 T1_SSGS_7 T1_SSGS_8 T1_SSGS_9 T1_SSGS_10 T1_SSGS_11
    ## 1         1         1         5         1         1          5          1
    ## 2         1         2         4         1         2          2          1
    ## 3         3         4         2         1         1          1          3
    ## 4         1         3         4         1         1          4          1
    ## 5         1         1         5         1         1          5          1
    ## 6         2         1         5         1         1          4          1
    ##   T1_SSGS_12 T1_SSGS_13 T1_SSGS_14 T1_SSGS_15 T1_SAFE_1 T1_SAFE_2 T1_SAFE_3
    ## 1          1          5          1          1         2         2         2
    ## 2          1          4          2          4         4         4         4
    ## 3          1          3          2          1         1         4         2
    ## 4          4          5          1          1         2         4         4
    ## 5          1          5          1          1         2         1         4
    ## 6          4          3          2          2         1         2         2
    ##   T1_SAFE_4 T1_SAFE_5 T1_SAFE_6 T1_SAFE_7 T1_SAFE_8 T1_SAFE_9 T1_SAFE_10
    ## 1         2         1         1         1         1         1          1
    ## 2         3         1         2         2         3         3          2
    ## 3         3         2         4         1         2         2          2
    ## 4         5         2         1         4         2         2          3
    ## 5         1         1         1         1         2         2          1
    ## 6         2         2         2         1         1         1          2
    ##   T1_SAFE_11 T1_SAFE_12 T1_SAFE_13 T1_SAFE_14 T1_SAFE_15 T1_SAFE_16 T1_SAFE_17
    ## 1          1          1          2          1          1          1          1
    ## 2          1          2          1          3          2          2          1
    ## 3          1          1          2          3          1          1          1
    ## 4          1          1          1          2          3          2          1
    ## 5          1          1          1          2          1          1          1
    ## 6          1          1          1          1          1          1          1
    ##   T1_SAFE_18 T1_SAFE_19 T1_SAFE_20 T1_SAFE_21 T1_SAFE_22 T1_SAFE_23 T1_SAFE_24
    ## 1          1          1          1          1          1          1          1
    ## 2          5          1          2          1          2          1          2
    ## 3          2          2          1          1          2          1          1
    ## 4          3          3          3          2          2          1          1
    ## 5          2          1          1          1          1          1          1
    ## 6          3          2          1          2          1          1          1
    ##   T1_SAFE_25 T1_SAFE_26 T1_SAFE_27 T1_SAFE_28 T1_SAFE_29 T1_SAFE_30 T1_SAFE_31
    ## 1          1          1          1          1          1          1          1
    ## 2          3          1          2          2          2          1          1
    ## 3          2          1          2          3          1          1          2
    ## 4          1          1          2          3          2          2          1
    ## 5          1          1          1          1          1          1          1
    ## 6          2          2          1          2          1          1          1
    ##   T1_SAFE_32 T1_SADS_1 T1_SADS_2 T1_SADS_3 T1_SADS_4 T1_SADS_5 T1_SADS_6
    ## 1          1         1         2         2         1         2         1
    ## 2          4         2         1         2         2         1         2
    ## 3          4         2         1         2         2         2         2
    ## 4          4         2         1         2         1         2         1
    ## 5          2         1         2         1         1         2         1
    ## 6          2         2         2         2         1         2         1
    ##   T1_SADS_7 T1_SADS_8 T1_SADS_9 T1_SADS_10 T1_SADS_11 T1_SADS_12 T1_SADS_13
    ## 1         1         2         1          2          2          1          2
    ## 2         2         1         2          1          1          2          1
    ## 3         2         1         1          2          1          2          1
    ## 4         1         2         1          2          1          1          2
    ## 5         1         2         1          2          2          1          2
    ## 6         1         2         1          2          1          1          2
    ##   T1_SADS_14 T1_SADS_15 T1_SADS_16 T1_SADS_17 T1_SADS_18 T1_SADS_19 T1_SADS_20
    ## 1          2          1          2          1          2          1          2
    ## 2          1          2          2          1          1          1          1
    ## 3          1          2          1          1          1          2          2
    ## 4          1          2          2          1          1          1          2
    ## 5          2          1          2          1          2          1          2
    ## 6          1          2          1          1          1          1          2
    ##   T1_SADS_21 T1_SADS_22 T1_SADS_23 T1_SADS_24 T1_SADS_25 T1_SADS_26 T1_SADS_27
    ## 1          2          1          2          2          1          2          1
    ## 2          1          2          1          1          1          2          1
    ## 3          1          1          2          2          1          2          1
    ## 4          1          1          1          2          1          2          1
    ## 5          2          1          2          2          1          2          1
    ## 6          2          1          2          2          1          2          1
    ##   T1_SADS_28 T2_EDEQ_1 T2_EDEQ_2 T2_EDEQ_3 T2_EDEQ_4 T2_EDEQ_5 T2_EDEQ_6
    ## 1          1         1         1         1         1         1         1
    ## 2          2         7         1         6         7         2         6
    ## 3          1         1         1         1         1         1         1
    ## 4          2         3         1         1         3         1         3
    ## 5          1         3         1         3         7         1         1
    ## 6          1         1         1         1         1         1         1
    ##   T2_EDEQ_7 T2_EDEQ_8 T2_EDEQ_9 T2_EDEQ_10 T2_EDEQ_11 T2_EDEQ_12 T2_EDEQ_13
    ## 1         1         1         1          1          1          1          1
    ## 2         2         2         1          7          2          3          7
    ## 3         1         1         1          1          1          1          1
    ## 4         2         2         1          1          1          1          3
    ## 5         1         1         1          3          1          1          1
    ## 6         1         2         1          2          1          1          1
    ##   T2_EDEQ_14 T2_EDEQ_15 T2_EDEQ_16 T2_EDEQ_17A T2_EDEQ_17B T2_EDEQ_17C
    ## 1          1          1          1           1         -99         -99
    ## 2          6          6          5           2           1           1
    ## 3          1          1          1           1         -99         -99
    ## 4          4          2          2           2           1           1
    ## 5          3          3          2           1           0           0
    ## 6          1          1          1           2           0           0
    ##   T2_EDEQ_18A T2_EDEQ_18B T2_EDEQ_19A T2_EDEQ_19B T2_EDEQ_20A T2_EDEQ_20B
    ## 1           1         -99           1         -99           1         -99
    ## 2           1         -99           1         -99           1         -99
    ## 3           1         -99           1         -99           1         -99
    ## 4           1         -99           1         -99           1         -99
    ## 5           1           0           1           0           1           0
    ## 6           1           0           1           0           1           0
    ##   T2_EDEQ_21A T2_EDEQ_21B T2_EDEQ_22A T2_EDEQ_22B T2_EDEQ_23 T2_EDEQ_24
    ## 1           1         -99           1         -99          1          1
    ## 2           1         -99           1         -99          2          1
    ## 3           1         -99           1         -99          1          1
    ## 4           1         -99           1         -99          2          2
    ## 5           1           0           2           2          1          1
    ## 6           1           0           2           4          1          1
    ##   T2_EDEQ_25 T2_EDEQ_26 T2_EDEQ_27 T2_EDEQ_28 T2_EDEQ_29 T2_EDEQ_30 T2_EDEQ_31
    ## 1          1          1          1          1          1          1          1
    ## 2          5          3          2          6          3          6          5
    ## 3          1          1          1          1          2          1          1
    ## 4          1          1          2          2          3          2          2
    ## 5          1          1          1          3          3          4          2
    ## 6          3          4          1          1          1          3          1
    ##   T2_EDEQ_32 T2_EDEQ_33 T2_EDEQ_34A
    ## 1          1          1           2
    ## 2          4          5           2
    ## 3          1          2           2
    ## 4          3          3           2
    ## 5          1          1           1
    ## 6          1          1           2
    ##                                                                                                                         T2_EDEQ_34B
    ## 1                                                                                                                               -99
    ## 2                                                                                                                               -99
    ## 3                                                                                                                               -99
    ## 4                                                                                                                               -99
    ## 5 I lost 15-20 pounds earlier in the year, and am trying to lost a little more.  So earlier in the year, I felt worse about my body
    ## 6                                                                                                                               -99
    ##   T2_Height T2_Weight T2_EDI_1 T2_EDI_2 T2_EDI_3 T2_EDI_4 T2_EDI_5 T2_EDI_6
    ## 1        69       145        4        4        2        2        2        2
    ## 2        69       145        3        2        4        3        1        6
    ## 3        64       115        5        2        2        2        3        1
    ## 4        70       173        2        1        1        3        2        3
    ## 5        71       173        5        3        3        3        2        2
    ## 6        70       145        5        2        1        1        3        1
    ##   T2_EDI_7 T2_EDI_8 T2_EDI_9 T2_EDI_10 T2_EDI_11 T2_EDI_12 T2_EDI_13 T2_EDI_14
    ## 1        1        1        5         1         1         5         2         1
    ## 2        5        2        2         5         1         4         5         1
    ## 3        3        1        5         2         1         5         2         1
    ## 4        3        2        3         3         3         4         4         2
    ## 5        5        2        4         2         2         5         4         1
    ## 6        1        1        4         2         3         1         2         1
    ##   T2_EDI_15 T2_EDI_16 T2_EDI_17 T2_EDI_18 T2_EDI_19 T2_EDI_20 T2_EDI_21
    ## 1         6         1         1         1         1         1         5
    ## 2         5         4         1         1         5         1         1
    ## 3         4         2         1         1         1         1         5
    ## 4         2         1         1         1         2         2         3
    ## 5         5         3         1         2         1         1         5
    ## 6         6         1         1         1         1         1         5
    ##   T2_EDI_22 T2_EDI_23 T2_SAAS_1 T2_SAAS_2 T2_SAAS_3 T2_SAAS_4 T2_SAAS_5
    ## 1         1         6         5         1         1         1         1
    ## 2         1         5         3         5         4         5         4
    ## 3         1         5         3         4         4         3         2
    ## 4         3         4         3         2         2         1         1
    ## 5         2         5         4         2         2         2         1
    ## 6         1         5         5         1         2         2         3
    ##   T2_SAAS_6 T2_SAAS_7 T2_SAAS_8 T2_SAAS_9 T2_SAAS_10 T2_SAAS_11 T2_SAAS_12
    ## 1         1         1         2         2          1          1          1
    ## 2         4         4         3         2          4          4          5
    ## 3         2         3         2         2          2          4          2
    ## 4         1         2         1         1          1          3          1
    ## 5         1         2         1         1          1          2          2
    ## 6         2         3         2         2          1          2          1
    ##   T2_SAAS_13 T2_SAAS_14 T2_SAAS_15 T2_SAAS_16 T2_FMPS_1 T2_FMPS_2 T2_FMPS_3
    ## 1          1          1          1          1         4         5         1
    ## 2          5          5          5          5         3         5         1
    ## 3          2          5          2          3         5         2         4
    ## 4          1          2          1          1         1         5         1
    ## 5          1          2          1          2         1         4         1
    ## 6          3          4          1          3         4         2         2
    ##   T2_FMPS_4 T2_FMPS_5 T2_FMPS_6 T2_FMPS_7 T2_FMPS_8 T2_FMPS_9 T2_FMPS_10
    ## 1         1         1         2         3         5         1          1
    ## 2         3         1         4         3         5         3          4
    ## 3         3         4         4         1         3         3          4
    ## 4         2         1         4         5         5         1          2
    ## 5         2         1         5         5         5         3          5
    ## 6         3         1         5         4         4         2          4
    ##   T2_FMPS_11 T2_FMPS_12 T2_FMPS_13 T2_FMPS_14 T2_FMPS_15 T2_FMPS_16 T2_FMPS_17
    ## 1          1          5          1          1          2          5          1
    ## 2          2          4          2          1          3          4          1
    ## 3          5          3          2          3          5          3          5
    ## 4          1          4          1          1          1          5          3
    ## 5          2          5          2          2          2          5          2
    ## 6          2          5          3          2          2          5          4
    ##   T2_FMPS_18 T2_FMPS_19 T2_FMPS_20 T2_FMPS_21 T2_FMPS_22 T2_FMPS_23 T2_FMPS_24
    ## 1          1          5          4          1          1          1          4
    ## 2          3          4          2          4          1          1          3
    ## 3          4          3          4          4          5          3          3
    ## 4          4          5          1          2          1          1          4
    ## 5          4          5          5          1          1          1          5
    ## 6          4          4          3          2          3          2          5
    ##   T2_FMPS_25 T2_FMPS_26 T2_FMPS_27 T2_FMPS_28 T2_FMPS_29 T2_FMPS_30 T2_FMPS_31
    ## 1          1          1          5          2          5          3          4
    ## 2          4          1          4          3          3          4          4
    ## 3          4          5          2          4          2          3          2
    ## 4          2          1          5          1          5          4          5
    ## 5          2          1          5          1          4          5          5
    ## 6          2          1          4          2          3          4          4
    ##   T2_FMPS_32 T2_FMPS_33 T2_FMPS_34 T2_FMPS_35 T2_SSES_1 T2_SSES_2 T2_SSES_3
    ## 1          2          2          2          1         5         1         4
    ## 2          1          5          4          1         4         4         2
    ## 3          3          2          3          5         3         4         3
    ## 4          1          2          2          1         4         2         3
    ## 5          3          2          3          1         5         4         4
    ## 6          2          3          2          3         4         3         5
    ##   T2_SSES_4 T2_SSES_5 T2_SSES_6 T2_SSES_7 T2_SSES_8 T2_SSES_9 T2_SSES_10
    ## 1         1         2         5         1         1         5          1
    ## 2         3         1         3         3         5         4          3
    ## 3         4         3         2         2         4         2          3
    ## 4         1         3         4         2         2         4          1
    ## 5         2         1         5         2         2         4          1
    ## 6         2         2         3         1         2         4          2
    ##   T2_SSES_11 T2_SSES_12 T2_SSES_13 T2_SSES_14 T2_SSES_15 T2_SSES_16 T2_SSES_17
    ## 1          5          5          1          4          1          1          1
    ## 2          3          4          5          5          2          3          5
    ## 3          3          3          3          2          3          2          4
    ## 4          4          3          2          4          1          2          4
    ## 5          4          4          2          5          1          2          1
    ## 6          4          5          4          4          1          1          2
    ##   T2_SSES_18 T2_SSES_19 T2_SSES_20 T2_DEQ_1 T2_DEQ_2 T2_DEQ_3 T2_DEQ_4 T2_DEQ_5
    ## 1          1          1          1        1        1        1        1        1
    ## 2          1          2          5        5        2        6        3        1
    ## 3          3          2          3        6        5        5        5        5
    ## 4          2          2          4        2        1        2        1        1
    ## 5          1          1          1        1        3        2        2        1
    ## 6          1          1          2        3        2        3        5        2
    ##   T2_DEQ_6 T2_DEQ_7 T2_DEQ_8 T2_DEQ_9 T2_SIAS_1 T2_SIAS_2 T2_SIAS_3 T2_SIAS_4
    ## 1        1        2        5        7         1         1         1         1
    ## 2        2        5        6        4         5         1         3         5
    ## 3        5        6        5        3         3         3         5         3
    ## 4        2        1        1        6         2         2         3         2
    ## 5        1        1        3        7         1         1         1         1
    ## 6        6        3        6        6         2         1         4         2
    ##   T2_SIAS_5 T2_SIAS_6 T2_SIAS_7 T2_SIAS_8 T2_SIAS_9 T2_SIAS_10 T2_SIAS_11
    ## 1         5         1         1         2         5          1          5
    ## 2         2         2         4         3         1          4          1
    ## 3         3         3         4         2         2          3          2
    ## 4         4         2         3         2         3          2          2
    ## 5         3         3         2         1         5          1          4
    ## 6         3         2         2         1         3          2          2
    ##   T2_SIAS_12 T2_SIAS_13 T2_SIAS_14 T2_SIAS_15 T2_SIAS_16 T2_SIAS_17 T2_SIAS_18
    ## 1          1          1          1          1          1          1          1
    ## 2          4          1          5          5          5          5          3
    ## 3          4          4          2          4          4          4          4
    ## 4          2          1          1          2          3          2          1
    ## 5          3          1          1          2          1          1          1
    ## 6          3          3          3          2          3          1          1
    ##   T2_SIAS_19 T2_SIAS_20 T2_SPS_1 T2_SPS_2 T2_SPS_3 T2_SPS_4 T2_SPS_5 T2_SPS_6
    ## 1          1          1        1        2        1        1        1        1
    ## 2          5          1        1        1        1        3        2        4
    ## 3          4          4        4        2        1        2        1        3
    ## 4          2          2        1        2        3        1        1        2
    ## 5          1          1        1        1        1        1        1        1
    ## 6          2          3        2        1        1        2        1        1
    ##   T2_SPS_7 T2_SPS_8 T2_SPS_9 T2_SPS_10 T2_SPS_11 T2_SPS_12 T2_SPS_13 T2_SPS_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        1        1        1         2         2         4         4         3
    ## 3        1        1        1         2         2         2         3         2
    ## 4        1        1        1         1         1         2         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        1        1         1         1         2         1         1
    ##   T2_SPS_15 T2_SPS_16 T2_SPS_17 T2_SPS_18 T2_SPS_19 T2_SPS_20 T2_BFNE_1
    ## 1         1         1         1         1         1         1         1
    ## 2         3         2         4         4         1         4         3
    ## 3         3         3         3         3         1         3         3
    ## 4         2         1         1         3         1         2         2
    ## 5         1         1         1         1         1         1         2
    ## 6         2         1         1         3         1         2         4
    ##   T2_BFNE_2 T2_BFNE_3 T2_BFNE_4 T2_BFNE_5 T2_BFNE_6 T2_BFNE_7 T2_BFNE_8
    ## 1         3         1         2         1         1         4         1
    ## 2         1         4         1         4         4         2         5
    ## 3         1         3         2         3         2         2         3
    ## 4         3         2         2         1         1         3         2
    ## 5         5         1         1         1         1         2         2
    ## 6         2         2         3         2         2         2         3
    ##   T2_BFNE_9 T2_BFNE_10 T2_BFNE_11 T2_BFNE_12 T2_FPES_1 T2_FPES_2 T2_FPES_3
    ## 1         1          4          2          1         1         1         1
    ## 2         4          1          3          4         6         6         1
    ## 3         3          2          3          3         6         8         8
    ## 4         2          3          3          1         5         4         6
    ## 5         4          3          1          1         1         1         3
    ## 6         3          2          3          2         5         6         3
    ##   T2_FPES_4 T2_FPES_5 T2_FPES_6 T2_FPES_7 T2_FPES_8 T2_FPES_9 T2_FPES_10
    ## 1         1         8         1         1         1         1          1
    ## 2         3         7         9         7         5         5          3
    ## 3         9         8         8         5         8         9          2
    ## 4         3         5         5         7         3         1          1
    ## 5         1        10         3         1         1         1          1
    ## 6         4         6         5         2         4         1          4
    ##   T2_CET_1 T2_CET_2 T2_CET_3 T2_CET_4 T2_CET_5 T2_CET_6 T2_CET_7 T2_CET_8
    ## 1        4        4        4        3        2        2        2        3
    ## 2        5        3        5        5        3        2        5        3
    ## 3        4        3        4        3        3        1        1        4
    ## 4        4        3        5        3        3        2        3        4
    ## 5        5        5        4        1        2        5        5        1
    ## 6        5        4        2        4        2        1        2        3
    ##   T2_CET_9 T2_CET_10 T2_CET_11 T2_CET_12 T2_CET_13 T2_CET_14 T2_CET_15
    ## 1        1         1         1         4         2         1         1
    ## 2        1         5         1         5         4         5         1
    ## 3        4         1         1         3         2         2         2
    ## 4        1         1         1         4         3         3         1
    ## 5        2         3         1         5         5         4         2
    ## 6        1         1         3         4         3         4         2
    ##   T2_CET_16 T2_CET_17 T2_CET_18 T2_CET_19 T2_CET_20 T2_CET_21 T2_CET_22
    ## 1         1         5         1         1         1         2         1
    ## 2         2         5         4         5         2         2         1
    ## 3         2         4         1         1         2         2         3
    ## 4         2         4         1         3         2         2         1
    ## 5         3         5         5         5         3         1         3
    ## 6         1         4         2         1         1         2         2
    ##   T2_CET_23 T2_CET_24 T2_CIA_1 T2_CIA_2 T2_CIA_3 T2_CIA_4 T2_CIA_5 T2_CIA_6
    ## 1         1         1        1        1        1        1        1        1
    ## 2         4         5        2        4        2        2        1        1
    ## 3         2         2        1        1        1        1        1        1
    ## 4         1         3        1        2        1        1        1        1
    ## 5         1         4        1        1        1        1        1        1
    ## 6         1         1        1        2        1        1        1        1
    ##   T2_CIA_7 T2_CIA_8 T2_CIA_9 T2_CIA_10 T2_CIA_11 T2_CIA_12 T2_CIA_13 T2_CIA_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        2        3        2         2         2         1         1         2
    ## 3        1        1        1         1         1         1         1         1
    ## 4        2        2        2         1         2         1         1         1
    ## 5        1        1        1         1         2         1         1         1
    ## 6        1        1        1         1         1         1         1         1
    ##   T2_CIA_15 T2_CIA_16 T2_PANAS_1 T2_PANAS_2 T2_PANAS_3 T2_PANAS_4 T2_PANAS_5
    ## 1         1         1          5          2          5          2          5
    ## 2         2         3          5          3          5          3          4
    ## 3         1         1          3          4          3          3          2
    ## 4         1         1          4          2          3          2          3
    ## 5         1         1          5          1          5          1          5
    ## 6         1         1          4          2          3          3          4
    ##   T2_PANAS_6 T2_PANAS_7 T2_PANAS_8 T2_PANAS_9 T2_PANAS_10 T2_PANAS_11
    ## 1          2          1          1          5           5           1
    ## 2          2          3          3          5           4           2
    ## 3          3          3          3          4           2           4
    ## 4          1          2          1          4           4           2
    ## 5          1          1          2          5           5           3
    ## 6          1          1          1          3           3           2
    ##   T2_PANAS_12 T2_PANAS_13 T2_PANAS_14 T2_PANAS_15 T2_PANAS_16 T2_PANAS_17
    ## 1           2           1           5           2           5           5
    ## 2           3           3           4           5           5           5
    ## 3           2           2           3           3           3           3
    ## 4           4           2           4           1           4           4
    ## 5           5           1           5           2           5           5
    ## 6           4           1           4           3           5           3
    ##   T2_PANAS_18 T2_PANAS_19 T2_PANAS_20 T2_BDI_1 T2_BDI_2 T2_BDI_3 T2_BDI_4
    ## 1           1           3           1        1        1        1        1
    ## 2           4           5           4        1        2        2        1
    ## 3           2           3           2        1        2        3        1
    ## 4           1           4           2        1        1        1        2
    ## 5           1           5           1        1        1        1        1
    ## 6           2           3           2        2        1        1        1
    ##   T2_BDI_5 T2_BDI_6 T2_BDI_7 T2_BDI_8 T2_BDI_9 T2_BDI_10 T2_BDI_11 T2_BDI_12
    ## 1        1        1        1        1        1         1         1         2
    ## 2        2        1        2        2        1         2         1         2
    ## 3        2        2        1        1        1         2         2         1
    ## 4        1        1        2        1        1         1         1         2
    ## 5        1        1        1        1        1         2         1         1
    ## 6        1        1        1        2        2         1         1         1
    ##   T2_BDI_13 T2_BDI_14 T2_BDI_15 T2_BDI_16 T2_BDI_17 T2_BDI_18 T2_BDI_19
    ## 1         1         1         2         1         2         1         1
    ## 2         1         2         3         1         1         1         2
    ## 3         2         2         3         2         1         1         2
    ## 4         1         1         3         1         1         1         1
    ## 5         1         1         3         1         1         1         2
    ## 6         1         1         3         2         1         1         1
    ##   T2_BDI_20 T2_FOF_AE_1 T2_FOF_AE_2 T2_FOF_AE_3 T2_FOF_AE_4 T2_FOF_AE_5
    ## 1         1           1           1           1           1           1
    ## 2         1           5           3           3           5           4
    ## 3         2           1           1           1           1           1
    ## 4         1           1           1           1           1           1
    ## 5         1           1           1           1           1           1
    ## 6         1           1           1           1           1           1
    ##   T2_FOF_AE_6 T2_FOF_AE_7 T2_FOF_AE_8 T2_FOF_FAB_1 T2_FOF_FAB_2 T2_FOF_FAB_3
    ## 1           1           1           1            1            1            1
    ## 2           5           3           5            7            5            1
    ## 3           1           1           1            1            1            1
    ## 4           1           1           1            2            1            2
    ## 5           1           1           1            1            1            1
    ## 6           1           1           1            1            1            1
    ##   T2_FOF_FAB_4 T2_FOF_FAB_5 T2_FOF_FAB_6 T2_FOF_FAB_7 T2_FOF_FAB_8 T2_FOF_FC_1
    ## 1            1            1            1            1            1           1
    ## 2            7            1            7            3            7           7
    ## 3            3            1            1            1            2           1
    ## 4            4            1            4            1            2           1
    ## 5            7            1            4            1            4           1
    ## 6            6            1            2            1            1           1
    ##   T2_FOF_FC_2 T2_FOF_FC_3 T2_FOF_FC_4 T2_FOF_FC_5 T2_FOF_FC_6 T2_FOF_FC_7
    ## 1           1           1           1           1           1           1
    ## 2           1           2           5           5           6           5
    ## 3           1           1           1           2           1           1
    ## 4           1           1           1           1           1           1
    ## 5           1           1           1           1           1           1
    ## 6           1           1           2           1           1           1
    ##   T2_FOF_FC_8 T2_FOF_FC_9 T2_FOFS_1 T2_FOFS_2 T2_FOFS_3 T2_FOFS_4 T2_FOFS_5
    ## 1           1           1       -99       -99       -99       -99       -99
    ## 2           6           3        19        26         5         0        81
    ## 3           1           1         4       -99       -99       -99        27
    ## 4           1           1         5         0         0         0        15
    ## 5           1           1         9       -99       -99       -99        24
    ## 6           1           1         1       -99       -99       -99         2
    ##   T2_FOFS_6 T2_FOFS_7 T2_FOFS_8 T2_FOFS_9
    ## 1       -99       -99       -99       -99
    ## 2        19        22         8        16
    ## 3         1         1       -99       -99
    ## 4         0         0         0         0
    ## 5       -99       -99       -99       -99
    ## 6       -99         1       -99       -99
    ##                                T2_FOFS10 T2_CES_1 T2_CES_2 T2_CES_3 T2_CES_4
    ## 1                                    -99        4        1        1        1
    ## 2 Fried food, large pastries, fatty beef        7        4        4        2
    ## 3                                    -99        7        3        3        5
    ## 4                                    -99        7        3        5        5
    ## 5                   Junk food in general        3        1        1        1
    ## 6                                    -99        8        3        4        3
    ##   T2_CES_5 T2_CES_6 T2_CES_7 T2_CES_8 T2_CES_9 T2_CES_10 T2_CES_11 T2_CES_12
    ## 1        1        1        1        1        1         1         1         1
    ## 2        6        7        6        5        4         4         9         4
    ## 3        4        5        7        7        6         7         6         8
    ## 4        5        2        2        3        1         1         1         7
    ## 5        1        1        4        2        1         1         1         4
    ## 6        5        1        1        2        4         1         1         2
    ##   T2_MINI_1 T2_MINI_2 T2_MINI_3 T2_MINI_4 T2_MINI_5 T2_MINI_6 T2_MINI_7
    ## 1         5         5         2         2         3         1         1
    ## 2         1         4         2         2         1         4         1
    ## 3         1         3         2         3         4         4         4
    ## 4         1         5         5         2         2         4         1
    ## 5         3         3         3         4         5         1         1
    ## 6         4         4         3         1         2         3         2
    ##   T2_MINI_8 T2_MINI_9 T2_MINI_10 T2_MINI_11 T2_MINI_12 T2_MINI_13 T2_MINI_14
    ## 1         1         5          1          5          5          5          1
    ## 2         1         1          3          1          4          5          4
    ## 3         4         2          3          1          4          3          4
    ## 4         1         4          2          2          4          4          1
    ## 5         1         4          2          4          3          4          1
    ## 6         2         5          2          4          4          3          2
    ##   T2_MINI_15 T2_MINI_16 T2_MINI_17 T2_MINI_18 T2_MINI_19 T2_MINI_20 T2_MINI_21
    ## 1          1          1          1          1          1          1          1
    ## 2          1          5          1          3          2          3          4
    ## 3          3          4          3          4          3          2          3
    ## 4          2          4          1          1          2          4          4
    ## 5          2          1          1          1          2          1          4
    ## 6          1          2          1          3          3          2          4
    ##   T2_MINI_22 T2_MINI_23 T2_MINI_24 T2_MINI_25 T2_MINI_26 T2_MINI_27 T2_MINI_28
    ## 1          4          4          3          4          5          3          3
    ## 2        -99          2          4          4          4          4          3
    ## 3          3          2          4          4          3          3          3
    ## 4          4          5          4          5          4          2          4
    ## 5          4          5          2          1          5          3          2
    ## 6          4          5          3          4          4          2          2
    ##   T2_MINI_29 T2_MINI_30 T2_MINI_31 T2_MINI_32 T2_MINI_33 T2_MINI_34 T2_MINI_35
    ## 1          2          1          5          1          1          4          1
    ## 2          4          2          5          1          1          2          1
    ## 3          4          3          3          2          3          4          2
    ## 4          2          1          5          1          5          4          2
    ## 5          1          3          5          4          1          2          1
    ## 6          4          4          5          3          1          4          3
    ##   T2_MINI_36 T2_MINI_37 T2_MINI_38 T2_MINI_39 T2_MINI_40 T2_MINI_41 T2_MINI_42
    ## 1          4          2          5          5          5          1          5
    ## 2          4          2          4          2          5          3          3
    ## 3          4          3          3          2          5          4          3
    ## 4          1          1          4          5          4          2          4
    ## 5          1          1          5          2          4          4          5
    ## 6          3          1          4          3          4          3          4
    ##   T2_MINI_43 T2_MINI_44 T2_MINI_45 T2_MINI_46 T2_MINI_47 T2_IU_1 T2_IU_2
    ## 1          4          5          5          5          1       3       1
    ## 2          1          4          4          4          1       1       3
    ## 3          3          4          3          4          4       4       4
    ## 4          4          4          5          4          1       3       1
    ## 5          2          5          5          5          2       1       1
    ## 6          4          4          5          4          3       2       3
    ##   T2_IU_3 T2_IU_4 T2_IU_5 T2_IU_6 T2_IU_7 T2_IU_8 T2_IU_9 T2_IU_10 T2_IU_11
    ## 1       1       1       1       1       1       1       1        1        1
    ## 2       4       1       4       5       4       5       3        5        4
    ## 3       2       2       2       4       2       4       4        4        4
    ## 4       1       1       1       1       2       3       1        2        1
    ## 5       1       1       1       1       1       2       1        5        1
    ## 6       1       1       1       2       1       2       1        1        1
    ##   T2_IU_12 T2_IU_13 T2_IU_14 T2_IU_15 T2_IU_16 T2_IU_17 T2_IU_18 T2_IU_19
    ## 1        1        1        1        1        1        2        1        1
    ## 2        1        2        3        4        4        4        5        5
    ## 3        3        4        4        4        4        4        4        2
    ## 4        1        1        1        2        2        1        4        3
    ## 5        1        1        1        1        1        1        2        1
    ## 6        2        1        1        1        3        1        1        1
    ##   T2_IU_20 T2_IU_21 T2_IU_22 T2_IU_23 T2_IU_24 T2_IU_25 T2_IU_26 T2_IU_27
    ## 1        1        1        1        1        1        1        1        5
    ## 2        2        3        4        1        1        2        4        4
    ## 3        3        3        4        2        2        3        4        3
    ## 4        4        4        2        1        1        1        1        1
    ## 5        1        2        1        1        1        1        1        1
    ## 6        2        1        2        1        1        1        3        2
    ##   T2_SSGS_1 T2_SSGS_2 T2_SSGS_3 T2_SSGS_4 T2_SSGS_5 T2_SSGS_6 T2_SSGS_7
    ## 1         5         1         1         5         1         1         5
    ## 2         3         1       -99         3         2         4         4
    ## 3         2         3         3         2         3         3         2
    ## 4         3         1         1         4         1         2         4
    ## 5         5         1         1         5         1         2         5
    ## 6         4         1         1         4         2         1         5
    ##   T2_SSGS_8 T2_SSGS_9 T2_SSGS_10 T2_SSGS_11 T2_SSGS_12 T2_SSGS_13 T2_SSGS_14
    ## 1         1         1          5          1          1          5          1
    ## 2         1         2          3          4          1          4          3
    ## 3         1         1          1          1          1          1          1
    ## 4         1         1          4          1          2          1          1
    ## 5         1         2          5          1          1          5          1
    ## 6         1         1          4          1          2          2          1
    ##   T2_SSGS_15 T2_SAFE_1 T2_SAFE_2 T2_SAFE_3 T2_SAFE_4 T2_SAFE_5 T2_SAFE_6
    ## 1          1         1         1         1         1         1         1
    ## 2          2         4         4         4         4         1         2
    ## 3          1         1         4         3         3         1         4
    ## 4          1         2         4         3         4         1         1
    ## 5          1         2         1         3         1         1         1
    ## 6          1         1         3         1         2         1         1
    ##   T2_SAFE_7 T2_SAFE_8 T2_SAFE_9 T2_SAFE_10 T2_SAFE_11 T2_SAFE_12 T2_SAFE_13
    ## 1         1         1         1          1          1          1          1
    ## 2         1         3         2          2          1          1          1
    ## 3         1         2         1          2          1          1          1
    ## 4         4         2         2          1          1          1          1
    ## 5         1         1         2          1          1          1          1
    ## 6         2         2         1          1          1          1          2
    ##   T2_SAFE_14 T2_SAFE_15 T2_SAFE_16 T2_SAFE_17 T2_SAFE_18 T2_SAFE_19 T2_SAFE_20
    ## 1          1          1          1          1          2          1          1
    ## 2          4          3          1          1          5          2          3
    ## 3          2          1          1          1          2          3          1
    ## 4          2          1          1          1          1          2          2
    ## 5          2          1          2          1          2          1          1
    ## 6          1          1          1          1          3          2          1
    ##   T2_SAFE_21 T2_SAFE_22 T2_SAFE_23 T2_SAFE_24 T2_SAFE_25 T2_SAFE_26 T2_SAFE_27
    ## 1          1          1          1          1          1          1          1
    ## 2          2          1          2          2          2          1          2
    ## 3          1          2          2          1          2          1          2
    ## 4          2          1          1          1          1          1          1
    ## 5          1          1          1          1          1          1          1
    ## 6          2          1          1          1          1          2          3
    ##   T2_SAFE_28 T2_SAFE_29 T2_SAFE_30 T2_SAFE_31 T2_SAFE_32 T2_SADS_1 T2_SADS_2
    ## 1          1          1          1          1          1         1         2
    ## 2          2          1          1          1          5         2         1
    ## 3          3          1          1          2          4         2         1
    ## 4          2          2          3          3          4         1         2
    ## 5          1          1          1          1          2         1         2
    ## 6          2          1          1          1          2         1         2
    ##   T2_SADS_3 T2_SADS_4 T2_SADS_5 T2_SADS_6 T2_SADS_7 T2_SADS_8 T2_SADS_9
    ## 1         1         1         2         1         1         2         1
    ## 2         2         2         1         2         2         1         2
    ## 3         2         2         1         2         1         2         1
    ## 4         2         1         2         1         1         2         2
    ## 5         1         1         1         1         1         1         1
    ## 6         2         1         2         1         1         2         1
    ##   T2_SADS_10 T2_SADS_11 T2_SADS_12 T2_SADS_13 T2_SADS_14 T2_SADS_15 T2_SADS_16
    ## 1          2          2          1          2          2          1          2
    ## 2          2          1          2          1          1          2          1
    ## 3          2          2          1          1          1          2          1
    ## 4          2          1          1          2          1          2          1
    ## 5          1          2          1          2          2          1          2
    ## 6          2          2          1          2          1          1          2
    ##   T2_SADS_17 T2_SADS_18 T2_SADS_19 T2_SADS_20 T2_SADS_21 T2_SADS_22 T2_SADS_23
    ## 1          1          2          1          2          2          1          2
    ## 2          2          1          1          1          1          2          1
    ## 3          2          1          2          1          1          1          1
    ## 4          1          2          1          1          1          1          1
    ## 5          1          2          1          2          2          1          2
    ## 6          2          2          1          2          2          1          2
    ##   T2_SADS_24 T2_SADS_25 T2_SADS_26 T2_SADS_27 T2_SADS_28 T3_EDEQ_1 T3_EDEQ_2
    ## 1          2          1          2          1          1         1         1
    ## 2          1          2          1          1          2         7         1
    ## 3          2          1          1          1          2         1         1
    ## 4          2          1          2          1          2         2         1
    ## 5          2          1          2          1          1         3         1
    ## 6          2          1          2          1          1         1         1
    ##   T3_EDEQ_3 T3_EDEQ_4 T3_EDEQ_5 T3_EDEQ_6 T3_EDEQ_7 T3_EDEQ_8 T3_EDEQ_9
    ## 1         1         1         1         1         1         1         1
    ## 2         5         7         1         4         1         1         1
    ## 3         1         1         1         1         1         1         1
    ## 4         2         3         1         2         3         3         1
    ## 5         4         7         1         1         1         1         1
    ## 6         2         1         1         1         1         2         2
    ##   T3_EDEQ_10 T3_EDEQ_11 T3_EDEQ_12 T3_EDEQ_13 T3_EDEQ_14 T3_EDEQ_15 T3_EDEQ_16
    ## 1          1          1          1          1          1          1          1
    ## 2          7          3          3          6          2          1          3
    ## 3          1          1          1          1          1          1          1
    ## 4          2          1          1          2          2          2          1
    ## 5          7          1          1          1          2          4          1
    ## 6          3          1          1          1          1          1          2
    ##   T3_EDEQ_17A T3_EDEQ_17B T3_EDEQ_17C T3_EDEQ_18A T3_EDEQ_18B T3_EDEQ_19A
    ## 1           1         -99         -99           1         -99           1
    ## 2           1         -99         -99           1         -99           1
    ## 3           1         -99         -99           1         -99           1
    ## 4           2           2           1           1         -99           1
    ## 5           1         -99         -99           1         -99           1
    ## 6           2           2           0           1           0           1
    ##   T3_EDEQ_19B T3_EDEQ_20A T3_EDEQ_20B T3_EDEQ_21A T3_EDEQ_21B T3_EDEQ_22A
    ## 1         -99           1         -99           1         -99           1
    ## 2         -99           1         -99           1         -99           1
    ## 3         -99           1         -99           1         -99           1
    ## 4         -99           1         -99           1         -99           1
    ## 5         -99           1         -99           1         -99           1
    ## 6           0           1           0           1           0           2
    ##   T3_EDEQ_22B T3_EDEQ_23 T3_EDEQ_24 T3_EDEQ_25 T3_EDEQ_26 T3_EDEQ_27 T3_EDEQ_28
    ## 1         -99          1          1          1          1          1          1
    ## 2         -99          1          1          3          2          1          5
    ## 3         -99          1          1          1          1          1          1
    ## 4         -99          3          2          1          1          1          2
    ## 5         -99          1          1          2          2          1          2
    ## 6           3          1          2          3          3          1          1
    ##   T3_EDEQ_29 T3_EDEQ_30 T3_EDEQ_31 T3_EDEQ_32 T3_EDEQ_33 T3_EDEQ_34A
    ## 1          1          1          1          1          1           2
    ## 2          4          4          2          2          2           1
    ## 3          1          1          1          1          1           1
    ## 4          2          3          1          2          2           1
    ## 5          2          5          1          1          1           1
    ## 6          1          2          1          1          1           2
    ##                                                                                                                    T3_EDEQ_34B
    ## 1                                                                                                                          -99
    ## 2 My body image in the past year has in general been worse. This week there were positive influences and mood that boosted it.
    ## 3                                                                                                           too busy with work
    ## 4                                                                           Had several binge episodes, rather than occasional
    ## 5                                                     I've lost weight this year so I only want to take a little bit more off.
    ## 6                                                                                                                          -99
    ##   T3_Height T3_Weight T3_EDI_1 T3_EDI_2 T3_EDI_3 T3_EDI_4 T3_EDI_5 T3_EDI_6
    ## 1        69       145        5        4        2        1        2        1
    ## 2        69       144        4        2        2        3        1        4
    ## 3        64       115        5        3        1        1        1        1
    ## 4        70       175        3        1        2        3        2        3
    ## 5        71       170        5        3        3        5        1        1
    ## 6        70       145        5        3        1        1        3        1
    ##   T3_EDI_7 T3_EDI_8 T3_EDI_9 T3_EDI_10 T3_EDI_11 T3_EDI_12 T3_EDI_13 T3_EDI_14
    ## 1        1        1        5         1         1         6         1         1
    ## 2        5        1        5         2         1         5         5         1
    ## 3        1        1        4         1         1         4         1         1
    ## 4        3        2        4         1         2         3         2         2
    ## 5        1        1        3         1         1         3         3         1
    ## 6        1        1        5         2         3         5         1         1
    ##   T3_EDI_15 T3_EDI_16 T3_EDI_17 T3_EDI_18 T3_EDI_19 T3_EDI_20 T3_EDI_21
    ## 1         6         1         1         1         1         1         6
    ## 2         5         3         1         1         3         1         2
    ## 3         4         1         1         1         1         1         4
    ## 4         3         1         1         1         2         2         1
    ## 5         5         3         1         1         1         1         6
    ## 6         5         1         1         1         2         1         5
    ##   T3_EDI_22 T3_EDI_23 T3_SAAS_1 T3_SAAS_2 T3_SAAS_3 T3_SAAS_4 T3_SAAS_5
    ## 1         1         6         5         1         1         1         1
    ## 2         1         6         4         3         2         2         1
    ## 3         1         1         2         4         3         3         3
    ## 4         2         4         3         3         4         1         1
    ## 5         1         6         3         1         1         1         1
    ## 6         1         5         4         2         2         3         2
    ##   T3_SAAS_6 T3_SAAS_7 T3_SAAS_8 T3_SAAS_9 T3_SAAS_10 T3_SAAS_11 T3_SAAS_12
    ## 1         1         1         1         1          1          1          1
    ## 2         3         3         3         1          1          3          3
    ## 3         3         3         3         3          3          4          3
    ## 4         1         2         1         1          2          3          1
    ## 5         1         2         1         1          1          2          1
    ## 6         3         3         3         1          1          4          1
    ##   T3_SAAS_13 T3_SAAS_14 T3_SAAS_15 T3_SAAS_16 T3_FMPS_1 T3_FMPS_2 T3_FMPS_3
    ## 1          1          1          1          1         4         5         2
    ## 2          4          3          1          2         3         5         1
    ## 3          4          4          3          2         4         2         4
    ## 4          1          2          1          1         1         5         1
    ## 5          1          1          1          1         3         4         1
    ## 6          3          2          1          3         3         2         1
    ##   T3_FMPS_4 T3_FMPS_5 T3_FMPS_6 T3_FMPS_7 T3_FMPS_8 T3_FMPS_9 T3_FMPS_10
    ## 1         1         1         2         5         5         1          1
    ## 2         2         2         4         3         5         1          4
    ## 3         3         4         4         2         4         4          4
    ## 4         4         1         5         5         5         1          3
    ## 5         3         1         5         4         4         2          5
    ## 6         3         1         5         2         2         3          2
    ##   T3_FMPS_11 T3_FMPS_12 T3_FMPS_13 T3_FMPS_14 T3_FMPS_15 T3_FMPS_16 T3_FMPS_17
    ## 1          1          5          1          1          3          5          1
    ## 2          3          4          2          1          2          5          1
    ## 3          4          3          3          3          3          2          4
    ## 4          1          4          1          1          1          5          3
    ## 5          1          5          1          1          2          5          2
    ## 6          2          5          2          2          1          5          2
    ##   T3_FMPS_18 T3_FMPS_19 T3_FMPS_20 T3_FMPS_21 T3_FMPS_22 T3_FMPS_23 T3_FMPS_24
    ## 1          1          5          4          1          1          1          5
    ## 2          3          3          2          4          1          2          4
    ## 3          3          3          4          4          4          2          3
    ## 4          3          5          1          2          1          1          4
    ## 5          5          5          2          2          1          1          5
    ## 6          4          4          2          1          2          1          4
    ##   T3_FMPS_25 T3_FMPS_26 T3_FMPS_27 T3_FMPS_28 T3_FMPS_29 T3_FMPS_30 T3_FMPS_31
    ## 1          1          1          5          1          5          4          5
    ## 2          4          1          4          2          4          4          4
    ## 3          3          4          2          3          3          2          2
    ## 4          2          1          5          2          5          5          5
    ## 5          2          1          4          1          4          5          4
    ## 6          2          1          2          1          2          4          3
    ##   T3_FMPS_32 T3_FMPS_33 T3_FMPS_34 T3_FMPS_35 T3_SSES_1 T3_SSES_2 T3_SSES_3
    ## 1          1          1          1          1         5         1         5
    ## 2          1          4          3          1         5         4         4
    ## 3          3          2          2          4         3         4         3
    ## 4          1          3          1          1         4         1         3
    ## 5          2          2          3          1         5         2         3
    ## 6          1          2          1          2         4         3         4
    ##   T3_SSES_4 T3_SSES_5 T3_SSES_6 T3_SSES_7 T3_SSES_8 T3_SSES_9 T3_SSES_10
    ## 1         1         1         5         1         1         5          1
    ## 2         2         1         4         2         3         4          1
    ## 3         3         3         1         1         4         2          2
    ## 4         1         2         3         2         2         4          1
    ## 5         2         1         5         2         1         5          2
    ## 6         2         2         3         1         2         3          2
    ##   T3_SSES_11 T3_SSES_12 T3_SSES_13 T3_SSES_14 T3_SSES_15 T3_SSES_16 T3_SSES_17
    ## 1          5          5          1          5          1          1          1
    ## 2          4          4          4          4          2          2          5
    ## 3          2          3          4          3          2          3          3
    ## 4          4          3          2          4          1          2          3
    ## 5          4          3          2          5          1          1          3
    ## 6          4          4          3          3          1          1          3
    ##   T3_SSES_18 T3_SSES_19 T3_SSES_20 T3_DEQ_1 T3_DEQ_2 T3_DEQ_3 T3_DEQ_4 T3_DEQ_5
    ## 1          1          1          1        1        1        1        1        1
    ## 2          1          2          5        2        1        5        4        1
    ## 3          3          3          4        6        5        6        6        5
    ## 4          2          1          1        2        1        3        1        1
    ## 5          1          2          1        2        2        5        3        1
    ## 6          2          2          3        2        5        5        3        2
    ##   T3_DEQ_6 T3_DEQ_7 T3_DEQ_8 T3_DEQ_9 T3_SIAS_1 T3_SIAS_2 T3_SIAS_3 T3_SIAS_4
    ## 1        1        1        5        7         1         1         1         1
    ## 2        5        5        6        5         4         1         2         5
    ## 3        5        6        5        4         3         3         4         3
    ## 4        1        1        5        6         2         2         3         1
    ## 5        1        1        3        7         1         1         1         1
    ## 6        6        1        7        6         2         1         3         3
    ##   T3_SIAS_5 T3_SIAS_6 T3_SIAS_7 T3_SIAS_8 T3_SIAS_9 T3_SIAS_10 T3_SIAS_11
    ## 1         5         1         1         1         5          1          5
    ## 2         3         2         4         3         2          3          2
    ## 3         2         3         2         2         1          2          2
    ## 4         3         1         2         2         3          2          3
    ## 5         3         1         1         1         5          1          1
    ## 6         3         2         1         2         3          2          3
    ##   T3_SIAS_12 T3_SIAS_13 T3_SIAS_14 T3_SIAS_15 T3_SIAS_16 T3_SIAS_17 T3_SIAS_18
    ## 1          1          1          1          1          1          1          1
    ## 2          4          1          3          3          5          3          2
    ## 3          3          3          1          3          3          3          3
    ## 4          3          2          1          2          3          3          3
    ## 5          2          1          1          1          1          1          1
    ## 6          2          3          2          1          2          1          1
    ##   T3_SIAS_19 T3_SIAS_20 T3_SPS_1 T3_SPS_2 T3_SPS_3 T3_SPS_4 T3_SPS_5 T3_SPS_6
    ## 1          1          1        1        1        1        1        1        1
    ## 2          4          1        1        1        1        2        2        3
    ## 3          3          4        3        2        1        2        1        2
    ## 4          2          2        1        2        3        1        1        2
    ## 5          1          1        1        1        1        1        1        1
    ## 6          2          3        3        1        1        1        1        1
    ##   T3_SPS_7 T3_SPS_8 T3_SPS_9 T3_SPS_10 T3_SPS_11 T3_SPS_12 T3_SPS_13 T3_SPS_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        1        1        1         1         2         3         3         3
    ## 3        1        1        1         1         1         2         2         2
    ## 4        1        1        1         1         1         2         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        1        1         1         1         2         1         1
    ##   T3_SPS_15 T3_SPS_16 T3_SPS_17 T3_SPS_18 T3_SPS_19 T3_SPS_20 T3_BFNE_1
    ## 1         1         1         1         1         1         1         1
    ## 2         2         2         3         4         1         3         4
    ## 3         2         2         2         3         1         2         4
    ## 4         2         1         1         3         1         1         2
    ## 5         1         1         1         1         1         1         1
    ## 6         1         1         1         2         1         2         2
    ##   T3_BFNE_2 T3_BFNE_3 T3_BFNE_4 T3_BFNE_5 T3_BFNE_6 T3_BFNE_7 T3_BFNE_8
    ## 1         3         1         3         1         1         3         1
    ## 2         1         4         1         4         5         1         4
    ## 3         1         3         1         2         1         3         2
    ## 4         3         2         3         1         2         4         1
    ## 5         2         1         1         1         1         2         3
    ## 6         3         3         3         2         4         3         3
    ##   T3_BFNE_9 T3_BFNE_10 T3_BFNE_11 T3_BFNE_12 T3_FPES_1 T3_FPES_2 T3_FPES_3
    ## 1         1          3          3          1         1         1         1
    ## 2         4          2          3          4         7         3         1
    ## 3         3          2          3          3         9         8         8
    ## 4         2          3          1          2         5         2         4
    ## 5         3          5          3          1         1         1         1
    ## 6         3          2          2          2         3         4         3
    ##   T3_FPES_4 T3_FPES_5 T3_FPES_6 T3_FPES_7 T3_FPES_8 T3_FPES_9 T3_FPES_10
    ## 1         1        10         1         1         1         1          1
    ## 2         4         6         9         9         8         6          2
    ## 3         9         8         9         9         9         9          4
    ## 4         4         3         3         6         3         5          1
    ## 5         1        10         1         1         1         1          1
    ## 6         5         5         6         1         4         2          1
    ##   T3_CET_1 T3_CET_2 T3_CET_3 T3_CET_4 T3_CET_5 T3_CET_6 T3_CET_7 T3_CET_8
    ## 1        4        3        4        3        2        1        1        2
    ## 2        5        3        5        5        2        2        5        3
    ## 3        4        1        4        4        1        1        1        4
    ## 4        5        3        5        4        2        2        3        3
    ## 5        5        5        5        2        2        4        5        1
    ## 6        4        3        2        4        2        1        3        2
    ##   T3_CET_9 T3_CET_10 T3_CET_11 T3_CET_12 T3_CET_13 T3_CET_14 T3_CET_15
    ## 1        1         1         1         4         2         3         1
    ## 2        2         3         1         5         3         4         1
    ## 3        2         1         2         3         2         2         1
    ## 4        2         1         3         4         2         5         3
    ## 5        4         4         1         5         5         5         2
    ## 6        1         1         3         4         3         3         2
    ##   T3_CET_16 T3_CET_17 T3_CET_18 T3_CET_19 T3_CET_20 T3_CET_21 T3_CET_22
    ## 1         1         4         1         1         1         1         1
    ## 2         3         5         3         4         1         1         4
    ## 3         3         3         1         3         2         1         2
    ## 4         1         5         2         4         1         1         1
    ## 5         3         4         4         4         2         1         4
    ## 6         1         4         2         1         1         2         1
    ##   T3_CET_23 T3_CET_24 T3_CIA_1 T3_CIA_2 T3_CIA_3 T3_CIA_4 T3_CIA_5 T3_CIA_6
    ## 1         1         1        1        1        1        1        1        1
    ## 2         4         4        1        2        2        1        1        1
    ## 3         1         3        1        1        1        1        1        1
    ## 4         1         4        1        2        1        1        1        1
    ## 5         4         4        1        1        1        1        1        1
    ## 6         1         3        1        2        1        1        1        1
    ##   T3_CIA_7 T3_CIA_8 T3_CIA_9 T3_CIA_10 T3_CIA_11 T3_CIA_12 T3_CIA_13 T3_CIA_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        2        2        2         2         2         1         1         2
    ## 3        1        1        1         1         1         1         1         1
    ## 4        1        2        2         1         2         1         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        2        1         1         2         1         1         1
    ##   T3_CIA_15 T3_CIA_16 T3_PANAS_1 T3_PANAS_2 T3_PANAS_3 T3_PANAS_4 T3_PANAS_5
    ## 1         1         1          5          2          4          2          5
    ## 2         2         2          5          3          5          3          5
    ## 3         1         1          3          3          2          2          2
    ## 4         1         2          4          1          4          2          5
    ## 5         1         1          5          3          5          3          5
    ## 6         1         1          4          2          3          4          4
    ##   T3_PANAS_6 T3_PANAS_7 T3_PANAS_8 T3_PANAS_9 T3_PANAS_10 T3_PANAS_11
    ## 1          2          1          1          5           5           2
    ## 2          3          3          2          4           4           3
    ## 3          3          2          3          2           2           3
    ## 4          2          1          1          5           4           2
    ## 5          1          1          3          5           5           3
    ## 6          1          3          1          4           4           3
    ##   T3_PANAS_12 T3_PANAS_13 T3_PANAS_14 T3_PANAS_15 T3_PANAS_16 T3_PANAS_17
    ## 1           5           1           5           3           5           5
    ## 2           5           2           5           5           5           5
    ## 3           2           3           2           3           2           2
    ## 4           4           2           3           2           5           4
    ## 5           5           1           5           3           5           5
    ## 6           3           1           4           2           5           3
    ##   T3_PANAS_18 T3_PANAS_19 T3_PANAS_20 T3_BDI_1 T3_BDI_2 T3_BDI_3 T3_BDI_4
    ## 1           3           5           2        1        1        1        1
    ## 2           5           3           3        1        1        2        1
    ## 3           2           2           2        1        1        1        1
    ## 4           1           4           1        1        1        1        1
    ## 5           1           5           1        1        1        1        1
    ## 6           2           4           3        2        1        1        1
    ##   T3_BDI_5 T3_BDI_6 T3_BDI_7 T3_BDI_8 T3_BDI_9 T3_BDI_10 T3_BDI_11 T3_BDI_12
    ## 1        1        1        1        1        1         1         1         2
    ## 2        2        1        2        1        1         2         2         1
    ## 3        1        2        3        1        1         1         2         2
    ## 4        1        1        2        1        1         1         1         1
    ## 5        1        1        1        1        1         2         2         2
    ## 6        1        1        1        1        2         2         1         1
    ##   T3_BDI_13 T3_BDI_14 T3_BDI_15 T3_BDI_16 T3_BDI_17 T3_BDI_18 T3_BDI_19
    ## 1         1         2         1         1         1         1         1
    ## 2         1         2         1         1         3         2         1
    ## 3         1         2         3         2         3         2         2
    ## 4         1         1         3         1         3         1         1
    ## 5         1         1         1         1         1         1         2
    ## 6         1         1         1         2         1         1         1
    ##   T3_BDI_20 T3_FOF_AE_1 T3_FOF_AE_2 T3_FOF_AE_3 T3_FOF_AE_4 T3_FOF_AE_5
    ## 1         1           1           1           1           1           1
    ## 2         1           5           2           2           2           2
    ## 3         2           1           1           1           1           1
    ## 4         1           1           1           1           2           1
    ## 5         1           1           1           1           1           1
    ## 6         2           1           1           1           1           1
    ##   T3_FOF_AE_6 T3_FOF_AE_7 T3_FOF_AE_8 T3_FOF_FAB_1 T3_FOF_FAB_2 T3_FOF_FAB_3
    ## 1           1           1           1            1            1            1
    ## 2           3           1           3            7            3            1
    ## 3           1           1           1            1            1            1
    ## 4           1           1           1            1            3            3
    ## 5           1           1           1            1            6            1
    ## 6           1           1           1            1            1            1
    ##   T3_FOF_FAB_4 T3_FOF_FAB_5 T3_FOF_FAB_6 T3_FOF_FAB_7 T3_FOF_FAB_8 T3_FOF_FC_1
    ## 1            1            1            1            1            1           1
    ## 2            6            1            7            2            5           5
    ## 3            1            1            1            1            1           1
    ## 4            7            1            4            1            4           1
    ## 5            7            1            7            1            5           1
    ## 6            6            1            2            1            1           2
    ##   T3_FOF_FC_2 T3_FOF_FC_3 T3_FOF_FC_4 T3_FOF_FC_5 T3_FOF_FC_6 T3_FOF_FC_7
    ## 1           1           1           1           1           1           1
    ## 2           1           3           3           2           4           4
    ## 3           1           1           1           1           1           1
    ## 4           1           1           1           1           1           1
    ## 5           1           1           1           1           1           1
    ## 6           1           1           2           1           1           1
    ##   T3_FOF_FC_8 T3_FOF_FC_9 T3_FOFS_1 T3_FOFS_2 T3_FOFS_3 T3_FOFS_4 T3_FOFS_5
    ## 1           1           1       -99       -99       -99       -99       -99
    ## 2           7           3        10        20        10         0        81
    ## 3           1           1         1       -99       -99       -99        18
    ## 4           1           1         9         0         0         0        14
    ## 5           1           1         8       -99       -99       -99        24
    ## 6           1           1         2       -99       -99       -99         3
    ##   T3_FOFS_6 T3_FOFS_7 T3_FOFS_8 T3_FOFS_9
    ## 1       -99       -99       -99       -99
    ## 2         6        19         1         6
    ## 3       -99       -99       -99       -99
    ## 4         0         0         0         0
    ## 5       -99       -99       -99       -99
    ## 6       -99       -99       -99       -99
    ##                                   T3_FOFS_10 T3_CES_1 T3_CES_2 T3_CES_3
    ## 1                                        -99        4        1        1
    ## 2 Fried food, large pastries, non-lean meat.        6        8        2
    ## 3                                        -99        6        3        6
    ## 4                                        -99        6        3        7
    ## 5                                  Junk food        2        1        2
    ## 6                                       none        6        1        3
    ##   T3_CES_4 T3_CES_5 T3_CES_6 T3_CES_7 T3_CES_8 T3_CES_9 T3_CES_10 T3_CES_11
    ## 1        1        1        1        1        1        1         1         1
    ## 2        1        3        7        7        2        6         6        10
    ## 3        4        4        4        3        6        6         4         3
    ## 4        4        5        1        2        5        3         1         1
    ## 5        1        1        1        2        2        1         1         1
    ## 6        2        4        3        1        3        1         1         1
    ##   T3_CES_12 T3_MINI_1 T3_MINI_2 T3_MINI_3 T3_MINI_4 T3_MINI_5 T3_MINI_6
    ## 1         1         5         5         1         2         3         1
    ## 2         1         3         5         2         2         2         2
    ## 3         8         1         3         1         4         4         4
    ## 4         7         1         5         5         1         2         4
    ## 5         2         4         4         2         4         5         1
    ## 6         1         4         5         2         4         2         3
    ##   T3_MINI_7 T3_MINI_8 T3_MINI_9 T3_MINI_10 T3_MINI_11 T3_MINI_12 T3_MINI_13
    ## 1         1         2         5          1          5          5          5
    ## 2         1         1         2          3          1          4          5
    ## 3         2         4       -99        -99        -99        -99        -99
    ## 4         4         1         4          2          2          4          5
    ## 5         1         1         4          2          5          3          5
    ## 6         4         2         5          2          3          4          2
    ##   T3_MINI_14 T3_MINI_15 T3_MINI_16 T3_MINI_17 T3_MINI_18 T3_MINI_19 T3_MINI_20
    ## 1          1          1          1          1          1          4          1
    ## 2          4          1          3          1          4          2          3
    ## 3        -99        -99        -99          3        -99          2          2
    ## 4          1          3          4          1          1          4          4
    ## 5          2          1          1          1          1          1          1
    ## 6          4          3          3          1          1          2          4
    ##   T3_MINI_21 T3_MINI_22 T3_MINI_23 T3_MINI_24 T3_MINI_25 T3_MINI_26 T3_MINI_27
    ## 1          3          2          4          3          5          5          2
    ## 2          4          1          5          4          4          4          4
    ## 3          3          3          2          3          4          3          4
    ## 4          4          5          5          4          5          4          1
    ## 5          4          2          5          4          2          5          2
    ## 6          5          4          5          4          5          4          1
    ##   T3_MINI_28 T3_MINI_29 T3_MINI_30 T3_MINI_31 T3_MINI_32 T3_MINI_33 T3_MINI_34
    ## 1          4          1          1          5          1          2          4
    ## 2          3          4          3          5          1          1          2
    ## 3          2          4          4          3          3          2          4
    ## 4          4          1          1          4          1          1          4
    ## 5          4          4          2          5          3          1          2
    ## 6          3          4          3          5          2          1          5
    ##   T3_MINI_35 T3_MINI_36 T3_MINI_37 T3_MINI_38 T3_MINI_39 T3_MINI_40 T3_MINI_41
    ## 1          1          4          2          5          5          5          1
    ## 2          1          4          2          4          2          5          4
    ## 3          1          4          3          3          2          4          4
    ## 4          4          2          1          4          4          5          1
    ## 5          1          4          1          5          2          2          2
    ## 6          4          4          1          4          3          4          2
    ##   T3_MINI_42 T3_MINI_43 T3_MINI_44 T3_MINI_45 T3_MINI_46 T3_MINI_47 T3_IU_1
    ## 1          4          4          5          5          5          1       3
    ## 2          3          1          4          4          4          2       2
    ## 3          3          4          4          4          4          4       4
    ## 4          4          4          5          5          4          1       2
    ## 5          5          2          5          5          5          2       1
    ## 6          4          4          4          5          5          3       4
    ##   T3_IU_2 T3_IU_3 T3_IU_4 T3_IU_5 T3_IU_6 T3_IU_7 T3_IU_8 T3_IU_9 T3_IU_10
    ## 1       1       1       1       1       1       1       1       1        1
    ## 2       1       4       1       4       4       4       5       1        5
    ## 3       4       2       1       2       1       2       4       4        3
    ## 4       1       1       1       1       1       1       3       1        2
    ## 5       1       1       1       1       1     -99       1       1        1
    ## 6       2       1       1       1       1       1       3       1        1
    ##   T3_IU_11 T3_IU_12 T3_IU_13 T3_IU_14 T3_IU_15 T3_IU_16 T3_IU_17 T3_IU_18
    ## 1        1        1        1        1        1        1        1        1
    ## 2        4        1        1        3        4        2        4        5
    ## 3        3        3        4        3        4        4        4        4
    ## 4        1        1        1        1        1        1        1        1
    ## 5        1        1        1        1        1        1        1        1
    ## 6        1        2        1        1        3        2        1        1
    ##   T3_IU_19 T3_IU_20 T3_IU_21 T3_IU_22 T3_IU_23 T3_IU_24 T3_IU_25 T3_IU_26
    ## 1        1        2        1        2        1        1        1        1
    ## 2        4        1        4        4        1        1        1        4
    ## 3        2        3        4        4        1        2        1        3
    ## 4        2        1        1        1        1        1        1        1
    ## 5        1        1        2        1        1        1        1        1
    ## 6        1        2        1        2        1        1        1        1
    ##   T3_IU_27 T3_SSGS_1 T3_SSGS_2 T3_SSGS_3 T3_SSGS_4 T3_SSGS_5 T3_SSGS_6
    ## 1        3         5         1         1         5         1         1
    ## 2        2         4         3         3         3         2         5
    ## 3        3         2         4         4         2         4         4
    ## 4        1         4         1         1         4         1         2
    ## 5        1         4         1         1         5         1         1
    ## 6        1         4         1         2         4         1         1
    ##   T3_SSGS_7 T3_SSGS_8 T3_SSGS_9 T3_SSGS_10 T3_SSGS_11 T3_SSGS_12 T3_SSGS_13
    ## 1         5         1         1          5          1          1          5
    ## 2         4         4         5          4          3          5          5
    ## 3         3         2         2          1          1          1          3
    ## 4         4         1         4          4          1          1          4
    ## 5         5         1         1          5          1          1          5
    ## 6         5         2         1          4          1          2          4
    ##   T3_SSGS_14 T3_SSGS_15 T3_SAFE_1 T3_SAFE_2 T3_SAFE_3 T3_SAFE_4 T3_SAFE_5
    ## 1          1          1         1         1         1         3         1
    ## 2          3          5         3         4         4         3         1
    ## 3          1          1         3         3         3         1         1
    ## 4          1          1         2         3         3         4         1
    ## 5          1          1         2         1         1         1         1
    ## 6          1          2         1         3         2         1         1
    ##   T3_SAFE_6 T3_SAFE_7 T3_SAFE_8 T3_SAFE_9 T3_SAFE_10 T3_SAFE_11 T3_SAFE_12
    ## 1         1         1         1         1          1          1          1
    ## 2         2         2         2         1          3          1          1
    ## 3         3         1         3         1          2          1          1
    ## 4         1         1         1         1          2          1          1
    ## 5         1         1         1         1          1          1          1
    ## 6         2         3         1         1          1          1          1
    ##   T3_SAFE_13 T3_SAFE_14 T3_SAFE_15 T3_SAFE_16 T3_SAFE_17 T3_SAFE_18 T3_SAFE_19
    ## 1          1          1          1          1          1          1          1
    ## 2          1          2          3          1          1          5          3
    ## 3          1          3          1          1          1          2          3
    ## 4          1          2          1          1          1          2          2
    ## 5          1          2          1          1          1          1          1
    ## 6          1          1          1          1          1          2          3
    ##   T3_SAFE_20 T3_SAFE_21 T3_SAFE_22 T3_SAFE_23 T3_SAFE_24 T3_SAFE_25 T3_SAFE_26
    ## 1          1          1          1          1          1          1          1
    ## 2          1          2          1          2          1          3          1
    ## 3          1          2          2          2          1          3          1
    ## 4          2          1          1          1          1          2          1
    ## 5          1          1          1          1          1          1          1
    ## 6          2          1          1          1          1          2          2
    ##   T3_SAFE_27 T3_SAFE_28 T3_SAFE_29 T3_SAFE_30 T3_SAFE_31 T3_SAFE_32 T3_SADS_1
    ## 1          1          1          1          1          1          1         1
    ## 2          3          3          1          1          2          5         2
    ## 3          1          2          1          1          3          3         2
    ## 4          1          2          1          2          3          3         2
    ## 5          1          1          1          1          1          1         1
    ## 6          2          2          1          1          1          3         1
    ##   T3_SADS_2 T3_SADS_3 T3_SADS_4 T3_SADS_5 T3_SADS_6 T3_SADS_7 T3_SADS_8
    ## 1         2         1         1         2         1         1         2
    ## 2         1         2         2         1         2         2         1
    ## 3         2         2         1         2         2         1         1
    ## 4         1         2         1         2         1         1         1
    ## 5         2         1         1         2         1         1         2
    ## 6         2         2         1         2         1         1         1
    ##   T3_SADS_9 T3_SADS_10 T3_SADS_11 T3_SADS_12 T3_SADS_13 T3_SADS_14 T3_SADS_15
    ## 1         1          2          2          1          2          2          1
    ## 2         2          1          1          1          1          1          2
    ## 3         1          2          1          1          2          1          2
    ## 4         2          1          1          1          2          1          1
    ## 5         1          2          2          1          2          2          1
    ## 6         2          2          1          1          2          1          2
    ##   T3_SADS_16 T3_SADS_17 T3_SADS_18 T3_SADS_19 T3_SADS_20 T3_SADS_21 T3_SADS_22
    ## 1          2          1          2          1          2          2          1
    ## 2          1          2          1          2          2          1          2
    ## 3          1          1          1          1          1          2          1
    ## 4          2          1          1          1          1          2          1
    ## 5          2          1          2          1          2          2          1
    ## 6          1          2          2          1          2          2          1
    ##   T3_SADS_23 T3_SADS_24 T3_SADS_25 T3_SADS_26 T3_SADS_27 T3_SADS_28 T4_EDEQ_1
    ## 1          2          2          1          2          1          1         2
    ## 2          1          1          2          2          1          1         7
    ## 3          2          2          1          2          1          1         1
    ## 4          2          2          1          2          1          1         1
    ## 5          2          2          1          2          1          1         4
    ## 6          2          2          1          2          1          1         1
    ##   T4_EDEQ_2 T4_EDEQ_3 T4_EDEQ_4 T4_EDEQ_5 T4_EDEQ_6 T4_EDEQ_7 T4_EDEQ_8
    ## 1         1         2         1         1         1         3         1
    ## 2         1         4         7         2         4         2         1
    ## 3         1         1         1         1         1         1         1
    ## 4         1         2         2         1         2         1         1
    ## 5         1         1         1         1         1         1         1
    ## 6         1         1         1         1         1         1         1
    ##   T4_EDEQ_9 T4_EDEQ_10 T4_EDEQ_11 T4_EDEQ_12 T4_EDEQ_13 T4_EDEQ_14 T4_EDEQ_15
    ## 1         1          1          1          1          1          1          1
    ## 2         1          7          3          2          7          5          4
    ## 3         1          1          1          1          1          1          1
    ## 4         1          2          1          1          1          2          1
    ## 5         1          1          1          1          1          3          4
    ## 6         2          2          1          1          1          1          1
    ##   T4_EDEQ16 T4_EDEQ_17A T4_EDEQ_17B T4_EDEQ_17C T4_EDEQ_18A T4_EDEQ_18B
    ## 1         1           1         -99         -99           2           2
    ## 2         3           2           2           2           1         -99
    ## 3         1           1         -99         -99           1         -99
    ## 4         1           1         -99         -99           1         -99
    ## 5         1           1         -99         -99           1         -99
    ## 6         1           2           1           0           1           0
    ##   T4_EDEQ_19A T4_EDEQ_19B T4_EDEQ_20A T4_EDEQ_20B T4_EDEQ_21A T4_EDEQ_21B
    ## 1           1         -99           1         -99           1         -99
    ## 2           1         -99           1         -99           1         -99
    ## 3           1         -99           1         -99           1         -99
    ## 4           1         -99           1         -99           1         -99
    ## 5           1         -99           1         -99           1         -99
    ## 6           1           0           1           0           1           0
    ##   T4_EDEQ_22A T4_EDEQ_22B T4_EDEQ_23A T4_EDEQ_24 T4_EDEQ_25 T4_EDEQ_26
    ## 1           1         -99           3          1          1          1
    ## 2           2           1           3          1          5          3
    ## 3           1         -99           1          1          1          1
    ## 4           1         -99           1          1          1          2
    ## 5           1         -99           1          1          2          2
    ## 6           2           2           1          1          2          3
    ##   T4_EDEQ_27 T4_EDEQ_28 T4_EDEQ_29 T4_EDEQ_30 T4_EDEQ_31 T4_EDEQ_32 T4_EDEQ_33
    ## 1          1          1          1          1          1          1          1
    ## 2          1          7          4          6          4          4          6
    ## 3          1          1          1          1          1          1          1
    ## 4          1          1          2          2          1          3          3
    ## 5          1          3          1          4          1          2          1
    ## 6          1          1          1          2          1          1          1
    ##   T4_EDEQ_34A
    ## 1           1
    ## 2           1
    ## 3           1
    ## 4           1
    ## 5           1
    ## 6           2
    ##                                                                                     T4_EDEQ_34B
    ## 1                                                                                           -99
    ## 2 The past year has been more controlled, in terms of eating, and less happy, in terms of mood.
    ## 3                                                                     too busy with school work
    ## 4                                                                    No episode of Binge eating
    ## 5                                                          I'm trying to lose some more weight.
    ## 6                                                                                           -99
    ##   T4_Height T4_Weight T4_EDI_1 T4_EDI_2 T4_EDI_3 T4_EDI_4 T4_EDI_5 T4_EDI_6
    ## 1        69       145        5        4        1        1        3        1
    ## 2        69       145        3        3        4        2        1        6
    ## 3        64       115      -99        1        1        1        2        1
    ## 4        70       175        3        1        2        2        2        3
    ## 5        71       172        4        3        3        4        1        1
    ## 6        70       145        6        2        1        1        3        1
    ##   T4_EDI_7 T4_EDI_8 T4_EDI_9 T4_EDI_10 T4_EDI_11 T4_EDI_12 T4_EDI_13 T4_EDI_14
    ## 1        1        1        5         1         1         6         1         1
    ## 2        6        2        2         6         1         3         4         1
    ## 3        1        1        5         1         1         5         1         1
    ## 4        1        1        2         1         2         3         3         1
    ## 5        1        1        3         3         1         4         2         1
    ## 6        1        1        5         1         1         5         1         1
    ##   T4_EDI_15 T4_EDI_16 T4_EDI_17 T4_EDI_18 T4_EDI_19 T4_EDI_20 T4_EDI_21
    ## 1         6         1         1         1         1         1         5
    ## 2         5         3         1         2         6         1         1
    ## 3         5         1         1       -99         1         1         1
    ## 4         2         1         1         1         1         1         2
    ## 5         5         2         1         1         1         1         5
    ## 6         6         1         1         1         1         1         6
    ##   T4_EDI_22 T4_EDI_23 T4_SAAS_1 T4_SAAS_2 T4_SAAS_3 T4_SAAS_4 T4_SAAS_5
    ## 1         1         6         5         1         1         1         1
    ## 2         1         3         2         5         5         5         5
    ## 3         1         5         4         3         4         3         1
    ## 4         3         4         3         4         3         1         1
    ## 5         1         5         4         2       -99         2         1
    ## 6         1         6         4         1         2         1         2
    ##   T4_SAAS_6 T4_SAAS_7 T4_SAAS_8 T4_SAAS_9 T4_SAAS_10 T4_SAAS_11 T4_SAAS_12
    ## 1         1         1         1         1          1          1          1
    ## 2         5         5         2         2          4          5          5
    ## 3         1         3         2         1          2          3          1
    ## 4         2         2         1         1          2          2          1
    ## 5         2         2         1         1          1          1          1
    ## 6         1         3         2         1          1          2          2
    ##   T4_SAAS_13 T4_SAAS_14 T4_SAAS_15 T4_SAAS_16 T4_FMPS_1 T4_FMPS_2 T4_FMPS_3
    ## 1          1          1          1          1         4         5         1
    ## 2          5          5          4          2         3         5         1
    ## 3          2          4          2          3         4         2         4
    ## 4          1          1          1          2         1         5         1
    ## 5          2          1          1          1         2         5         1
    ## 6          2          3          1          3         2         2         1
    ##   T4_FMPS_4 T4_FMPS_5 T4_FMPS_6 T4_FMPS_7 T4_FMPS_8 T4_FMPS_9 T4_FMPS_10
    ## 1         1         1         1         5         5         1          1
    ## 2         2         2         5         4         5         3          5
    ## 3         3         2         4         1         3         3          4
    ## 4         3         1         4         5         5         1          4
    ## 5         2         1         5         5         5         3          4
    ## 6         3         1         4         3         2         3          3
    ##   T4_FMPS_11 T4_FMPS_12 T4_FMPS_13 T4_FMPS_14 T4_FMPS_15 T4_FMPS_16 T4_FMPS_17
    ## 1          1          5          1          1          1          5          1
    ## 2          4          4          2          1          4          5          2
    ## 3          5          2          5          4          3          2          4
    ## 4          1          4          1          3          1          5          2
    ## 5          2          5          2          1          1          5          1
    ## 6          2          5          2          2          1          5          2
    ##   T4_FMPS_18 T4_FMPS_19 T4_FMPS_20 T4_FMPS_21 T4_FMPS_22 T4_FMPS_23 T4_FMPS_24
    ## 1          1          5          4          1          1          1          5
    ## 2          4          4          3          5          2          3          4
    ## 3          4          4          4          3          3          1          2
    ## 4          4          4          1          2          1          1          4
    ## 5          5          5          3          2          1          1          5
    ## 6          4          4          2          2          2          1          5
    ##   T4_FMPS_25 T4_FMPS_26 T4_FMPS_27 T4_FMPS_28 T4_FMPS_29 T4_FMPS_30 T4_FMPS_31
    ## 1          1          1          5          3          5          2          5
    ## 2          4          1          4          3          4          3          4
    ## 3          2          4          2          3          2          3          2
    ## 4          1          1          5          2          5          4          5
    ## 5          2          2          5          1          5          5          5
    ## 6          3          1          2          2          2          4          3
    ##   T4_FMPS_32 T4_FMPS_33 T4_FMPS_34 T4_FMPS_35 T4_SSES_1 T4_SSES_2 T4_SSES_3
    ## 1          1          1          1          1         5         1         5
    ## 2          1          5          4          1         3         4         2
    ## 3          3          2          2          3         3         3         4
    ## 4          1          3          2          1         4         1         3
    ## 5          1          1          2          1         5         2         3
    ## 6          1          3          2          2         4         2         4
    ##   T4_SSES_4 T4_SSES_5 T4_SSES_6 T4_SSES_7 T4_SSES_8 T4_SSES_9 T4_SSES_10
    ## 1         1         1         5         1         1         5          1
    ## 2         4         3         3         5         5         2          3
    ## 3         4         2         2         1         4         3          2
    ## 4         2         2         3         2         3         3          1
    ## 5         4         1         5         3         2         5          3
    ## 6         1         2         3         1         3         3          2
    ##   T4_SSES_11 T4_SSES_12 T4_SSES_13 T4_SSES_14 T4_SSES_15 T4_SSES_16 T4_SSES_17
    ## 1          5          5          1          5          1          1          1
    ## 2          3          2          5          3          3          4          5
    ## 3          2          3          3          2          3          2          2
    ## 4          4          3          2          4          1          2          3
    ## 5          4          3          2          5          1          2          5
    ## 6          4          4          3          4          2          2          3
    ##   T4_SSES_18 T4_SSES_19 T4_SSES_20 T4_DEQ_1 T4_DEQ_2 T4_DEQ_3 T4_DEQ_4 T4_DEQ_5
    ## 1          1          1          1        1        1        1        1        1
    ## 2          3          3          5        6        2        6        5        2
    ## 3          3          3          3        6        5        5        6        5
    ## 4          1          1          2        3        1        2        1        1
    ## 5          1          1          1        1        1        3        2        1
    ## 6          2          1          2        5        2        2        5        1
    ##   T4_DEQ_6 T4_DEQ_7 T4_DEQ_8 T4_DEQ_9 T4_SIAS_1 T4_SIAS_2 T4_SIAS_3 T4_SIAS_4
    ## 1        1        1        3        1         1         1         1         1
    ## 2        4        6        7        3         4         2         5         5
    ## 3        6        5        5        3         3         3         3         2
    ## 4        2        1        1        6         2         1         3         1
    ## 5        1        1        1        6         1         1         1         1
    ## 6        6        1        7        3         1         1         2         2
    ##   T4_SIAS_5 T4_SIAS_6 T4_SIAS_7 T4_SIAS_8 T4_SIAS_9 T4_SIAS_10 T4_SIAS_11
    ## 1         5         1         1         1         5          1          5
    ## 2         2         2         5         3         1          3          1
    ## 3         2         4         3         3         3          2          1
    ## 4         4         2         2         1         2          3          2
    ## 5         4         1         1         1         4          1          4
    ## 6         3         2         1         2         3          2          3
    ##   T4_SIAS_12 T4_SIAS_13 T4_SIAS_14 T4_SIAS_15 T4_SIAS_16 T4_SIAS_17 T4_SIAS_18
    ## 1          1          1          1          1          1          1          1
    ## 2          5          1          5          5          5          4          3
    ## 3          3          3          1          3          3          3          3
    ## 4          3          3          1          3          3          2          1
    ## 5          1          1          1          1          1          1          1
    ## 6          2          4          2          3          3          2          1
    ##   T4_SIAS_19 T4_SIAS_20 T4_SPS_1 T4_SPS_2 T4_SPS_3 T4_SPS_4 T4_SPS_5 T4_SPS_6
    ## 1          1          1        1        1        1        1        1        1
    ## 2          5          2        1        1        2        5        1        4
    ## 3          3          4        3        2        1        2        1        2
    ## 4          2          1        1        2        1        2        1        2
    ## 5          1          1        1        1        1        1        1        1
    ## 6          2          3        1        1        1        1        1        1
    ##   T4_SPS_7 T4_SPS_8 T4_SPS_9 T4_SPS_10 T4_SPS_11 T4_SPS_12 T4_SPS_13 T4_SPS_14
    ## 1        1        1        1         1         1         1         1         1
    ## 2        1        1        1         1         3         4         4         3
    ## 3        1        1        1         1         2         2         2         2
    ## 4        1        1        1         1         1         2         1         1
    ## 5        1        1        1         1         1         1         1         1
    ## 6        1        1        1         1         1         2         1         1
    ##   T4_SPS_15 T4_SPS_16 T4_SPS_17 T4_SPS_18 T4_SPS_19 T4_SPS_20 T4_BFNE_1
    ## 1         1         1         1         1         1         1         1
    ## 2         4         3         5         5         1         5         5
    ## 3         3         2         2         3         1         3         3
    ## 4         1         1         1         3         1         2         2
    ## 5         1         1         1         1         1         1         1
    ## 6         1         1         1         1         1         2         3
    ##   T4_BFNE_2 T4_BFNE_3 T4_BFNE_4 T4_BFNE_5 T4_BFNE_6 T4_BFNE_7 T4_BFNE_8
    ## 1         4         1         3         1         1         2         1
    ## 2         1         5         1         5         5         1         5
    ## 3         1         2         1         2         2         2         2
    ## 4         3         1         3         1         2         2         3
    ## 5         2         1         1         1         1         2         1
    ## 6         2         1         3         2         2         1         3
    ##   T4_BFNE_9 T4_BFNE_10 T4_BFNE_11 T4_BFNE_12 T4_FPES_1 T4_FPES_2 T4_FPES_3
    ## 1         1          4          1          1         1         1         1
    ## 2         4          1          5          4         7         8         1
    ## 3         2          1          4          3        10         9         6
    ## 4         2          3          1          2         5         4         7
    ## 5         3          1          1          1         1         1         1
    ## 6         4          2          3          2         3         2         4
    ##   T4_FPES_4 T4_FPES_5 T4_FPES_6 T4_FPES_7 T4_FPES_8 T4_FPES_9 T4_FPES_10
    ## 1         1         1         1         1         1         1          1
    ## 2         5         6        10         8         8        10          2
    ## 3         9         8         5         9         9         8          3
    ## 4         5         3         6         7         8         7          1
    ## 5         1        10         1         1         1         3          1
    ## 6         3         6         3         1         5         1          2
    ##   T4_CET_1 T4_CET_2 T4_CET_3 T4_CET_4 T4_CET_5 T4_CET_6 T4_CET_7 T4_CET_8
    ## 1        4        2        3        4        2        1        1        4
    ## 2        5        3        5        5        3        2        5        4
    ## 3        4        1        4        4        1        1        1        5
    ## 4        4        2        5        4        1        2        4        3
    ## 5        4        5        4        2        1        4        4        1
    ## 6        4        4        2        4        1        1        3        2
    ##   T4_CET_9 T4_CET_10 T4_CET_11 T4_CET_12 T4_CET_13 T4_CET_14 T4_CET_15
    ## 1        1         1         1         4         2         5         1
    ## 2        3         3         1         4         4         5         1
    ## 3        2         1         1         3         1         4         1
    ## 4        1         1         2         5         2         1         3
    ## 5        4         5         1         5         5         5         3
    ## 6        1         1         3         4         3         5         2
    ##   T4_CET_16 T4_CET_17 T4_CET_18 T4_CET_19 T4_CET_20 T4_CET_21 T4_CET_22
    ## 1         1         4         1         1         1         1         1
    ## 2         4         5         5         4         3         2         4
    ## 3         3         4         1         1         3         1         1
    ## 4         1         4         1         4         1         1         1
    ## 5         4         5         5         5         1         1         4
    ## 6         1         4         2         1         1         2         1
    ##   T4_CET_23 T4_CET_24 T4_CIA_1 T4_CIA_2 T4_CIA_3 T4_CIA_4 T4_CIA_5 T4_CIA_6
    ## 1         1         5        1        1        1        1        1        1
    ## 2         4         5        2        4        3        2        1        2
    ## 3         1         2        1        1        1        1        1        1
    ## 4         1         4        1        2        1        1        1        1
    ## 5         3         5        1        1        1        1        1        1
    ## 6         1         3        1        1        1        1        1        1
    ##   T4_CIA_7 T4_CIA_8 T4_CIA_9 T4_CIA_10 T4_CIA_11 T4_CIA_12 T4_CIA_13 T4_CIA_14
    ## 1        1        1        1         1         2         1         1         1
    ## 2        4        3        3         4         3         3         1         3
    ## 3        1        1        1         1         1         1         1         1
    ## 4        1        2        1         1         2         1         1         1
    ## 5        1        1        1         1         2         1         1         1
    ## 6        1        1        1         1         1         1         1         1
    ##   T4_CIA_15 T4_CIA_16 T4_PANAS_1 T4_PANAS_2 T4_PANAS_3 T4_PANAS_4 T4_PANAS_5
    ## 1         1         1          5          2          5          1          5
    ## 2         3         4          5          4          5          5          4
    ## 3         1         1          3          4          2          2          2
    ## 4         1         1          4          2          4          1          4
    ## 5         1         1          5          3          5          3          5
    ## 6         1         1          4          2          3          3          4
    ##   T4_PANAS_6 T4_PANAS_7 T4_PANAS_8 T4_PANAS_9 T4_PANAS_10 T4_PANAS_11
    ## 1          2          1          1          5           5           2
    ## 2          3          3          3          4           3           3
    ## 3          2          2          2          2           1           3
    ## 4          2          1          1          4           5           2
    ## 5          1          1          3          5           5           3
    ## 6          1          2          1          4           2           2
    ##   T4_PANAS_12 T4_PANAS_13 T4_PANAS_14 T4_PANAS_15 T4_PANAS_16 T4_PANAS_17
    ## 1           4           2           5           2           5           5
    ## 2           3           4           5           5           5           3
    ## 3           3           1           2           2           3           3
    ## 4           2           2           4           1           4           4
    ## 5           5           1           5           3           5           5
    ## 6           3           1           5           2           5           3
    ##   T4_PANAS_18 T4_PANAS_19 T4_PANAS_20 T4_BDI_1 T4_BDI_2 T4_BDI_3 T4_BDI_4
    ## 1           3           5           1        1        1        1        1
    ## 2           5           4           5        1        2        1        1
    ## 3           2           2           1        1        1        2        2
    ## 4           1           4           1        1        1        1        1
    ## 5           3           5           1        1        1        1        1
    ## 6           2           3           2        1        1        1        1
    ##   T4_BDI_5 T4_BDI_6 T4_BDI_7 T4_BDI_8 T4_BDI_9 T4_BDI_10 T4_BDI_11 T4_BDI_12
    ## 1        1        1        1        1        1         1         1         1
    ## 2        2        1        2        2        1         2         1         2
    ## 3        1        2        1        1        1         2         1         1
    ## 4        1        1        1        1        1         1         1         1
    ## 5        1        1        1        1        1         1         2         1
    ## 6        1        1        1        1        1         1         1         1
    ##   T4_BDI_13 T4_BDI_14 T4_BDI_15 T4_BDI_16 T4_BDI_17 T4_BDI_18 T4_BDI_19
    ## 1         1         1         1         1         1         1         1
    ## 2         1         2         3         1         3         2         2
    ## 3         1         1         1         2         1         1         2
    ## 4         1         1         3         1         2         1         1
    ## 5         1         1         1         1         1         1         1
    ## 6         1         1         1         2         1         1         2
    ##   T4_BDI_20 T4_FOF_AE_1 T4_FOF_AE_2 T4_FOF_AE_3 T4_FOF_AE_4 T4_FOF_AE_5
    ## 1         1           1           1           1           1           1
    ## 2         1           6           4           5           5           5
    ## 3         1           1           1           1           1           1
    ## 4         1           1           1           1           1           1
    ## 5         1           1           1           1           1           1
    ## 6         1           1           1           1           1           1
    ##   T4_FOF_AE_6 T4_FOF_AE_7 T4_FOF_AE_8 T4_FOF_FAB_1 T4_FOF_FAB_2 T4_FOF_FAB_3
    ## 1           1           1           1            1            1            1
    ## 2           6           1           5            7            5            2
    ## 3           1           1           1            1            1            1
    ## 4           1           1           1            1            3            4
    ## 5           1           1           1            1            1            1
    ## 6           1           1           1            1            1            1
    ##   T4_FOF_FAB_4 T4_FOF_FAB_5 T4_FOF_FAB_6 T4_FOF_FAB_7 T4_FOF_FAB_8 T4_FOF_FC_1
    ## 1            1            1            1            1            1           1
    ## 2            7            2            7            5            7           7
    ## 3            1            1            1            1            1           1
    ## 4            4            1            4            1            4           1
    ## 5            7            1            4            1            4           1
    ## 6            6            1            4            1            1           1
    ##   T4_FOF_FC_2 T4_FOF_FC_3 T4_FOF_FC_4 T4_FOF_FC_5 T4_FOF_FC_6 T4_FOF_FC_7
    ## 1           1           1           1           1           1           1
    ## 2           1           3           7           7           7           6
    ## 3           1           1           1           1           1           1
    ## 4           1           1           1           1           1           1
    ## 5           1           1           1           1           1           1
    ## 6           1           1           2           1           1           1
    ##   T4_FOF_FC_8 T4_FOF_FC_9 T4_FOFS_1 T4_FOFS_2 T4_FOFS_3 T4_FOFS_4 T4_FOFS_5
    ## 1           1           1       -99       -99       -99       -99       -99
    ## 2           7           4        15        20         5         0        85
    ## 3           1           1       -99       -99       -99       -99       -99
    ## 4           1           1        15         0         0         0        50
    ## 5           1           1        10       -99       -99       -99        20
    ## 6           1           1         1       -99       -99       -99         4
    ##   T4_FOFS_6 T4_FOFS_7 T4_FOFS_8 T4_FOFS_9
    ## 1       -99       -99       -99       -99
    ## 2        20        30        12        12
    ## 3         4       -99       -99       -99
    ## 4       -99         0         0         0
    ## 5       -99       -99       -99       -99
    ## 6       -99       -99       -99       -99
    ##                                   T4_FOFS_10 T4_CES_1 T4_CES_2 T4_CES_3
    ## 1                                         no        5        1        1
    ## 2 Fried food, large pastries, non-lean meat.        6        3        2
    ## 3                                        -99        7        4        5
    ## 4                                        -99        7        3        6
    ## 5                                  Junk Food        2        1        1
    ## 6                                        -99        4        2        5
    ##   T4_CES_4 T4_CES_5 T4_CES_6 T4_CES_7 T4_CES_8 T4_CES_9 T4_CES_10 T4_CES_11
    ## 1        1        3        1        1        1        1         1         1
    ## 2        3        4        4        5        1        6         1         9
    ## 3        5        4        4        3        5        6         5         5
    ## 4        4        5        2        2        5        1         2         1
    ## 5        1        1        1        1        1        1         1         1
    ## 6        2        4        2        1        3        7         2         1
    ##   T4_CES_12 T4_MINI_1 T4_MINI_2 T4_MINI_3 T4_MINI_4 T4_MINI_5 T4_MINI_6
    ## 1         1         5         5         3         3         4         1
    ## 2         3         1         5         2         2         2         3
    ## 3         7         1         3         2         3         3         4
    ## 4         5         1         5         5         2         2         4
    ## 5         2         4         4         2         4         5         1
    ## 6         1         4         5         2         2         2         3
    ##   T4_MINI_7 T4_MINI_8 T4_MINI_9 T4_MINI_10 T4_MINI_11 T4_MINI_12 T4_MINI_13
    ## 1         1         1         5          1          4          5          5
    ## 2         4         1         1          2          1          3          5
    ## 3         3         4         3          2          2          3          3
    ## 4         1         1         4          3          2          4          5
    ## 5         2         1         5          1          5          3          5
    ## 6         4         2         5          2          4          4          2
    ##   T4_MINI_14 T4_MINI_15 T4_MINI_16 T4_MINI_17 T4_MINI_18 T4_MINI_19 T4_MINI_20
    ## 1          3          1          1          1          1          2          1
    ## 2          4          1          4          1          4          2          4
    ## 3          3          3          4          3          4          3          3
    ## 4          1          2          4          1          2          5          4
    ## 5          2          1          1          1          1          4          1
    ## 6          3          1          4          1          1          2          3
    ##   T4_MINI_21 T4_MINI_22 T4_MINI_23 T4_MINI_24 T4_MINI_25 T4_MINI_26 T4_MINI_27
    ## 1          4          4          5          4          5          5          3
    ## 2          2          1          5          4          5          3          4
    ## 3          4          4          3          4          3          3          2
    ## 4          4          5          5          4          5          4          1
    ## 5          4          2          5          1          2          1          3
    ## 6          4          5          4          2          4          4          1
    ##   T4_MINI_28 T4_MINI_29 T4_MINI_30 T4_MINI_31 T4_MINI_32 T4_MINI_33 T4_MINI_34
    ## 1          3          1          1          5          1          1          4
    ## 2          2          5          4          4          1          1          2
    ## 3          3          4          3          3          3          2          4
    ## 4          4          2          1          4          1          1          4
    ## 5          2          3          2          5          3          1          2
    ## 6          2          3          2          5          2          1          5
    ##   T4_MINI_35 T4_MINI_36 T4_MINI_37 T4_MINI_38 T4_MINI_39 T4_MINI_40 T4_MINI_41
    ## 1          1          4          1          5          1          5          3
    ## 2          1          4          2          4          2          5          4
    ## 3          1          4          3          3          3          5          4
    ## 4          2          2          2          4          4          4          2
    ## 5          1          2          1          5          2          3          2
    ## 6          4          2          1          4          3          4          4
    ##   T4_MINI_42 T4_MINI_43 T4_MINI_44 T4_MINI_45 T4_MINI_46 T4_MINI_47 T4_IU_1
    ## 1          4          4          5          5          5          1       3
    ## 2          3          1          4          4          4          2       1
    ## 3          3          4          3          3          3          3       4
    ## 4          4          5          4          4          4          1       2
    ## 5          5          2          5          5          5          2       1
    ## 6          5          5          4          5          5          3       3
    ##   T4_IU_2 T4_IU_3 T4_IU_4 T4_IU_5 T4_IU_6 T4_IU_7 T4_IU_8 T4_IU_9 T4_IU_10
    ## 1       1       1       1       1       1       1       1       1        1
    ## 2       1       3       1       5       5       5       5       2        5
    ## 3       3       2       1       1       2       1       4       3        4
    ## 4       1       1       1       1       1       1       3       1        1
    ## 5       1       1       1       1       1       1       1       1        1
    ## 6       1       1       1       1       1       1       2       1        1
    ##   T4_IU_11 T4_IU_12 T4_IU_13 T4_IU_14 T4_IU_15 T4_IU_16 T4_IU_17 T4_IU_18
    ## 1        1        1        1        1        1        1        1        1
    ## 2        4        1        1        3        5        3        5        5
    ## 3        3        2        3        2        3        4        2        3
    ## 4        1        1        1        1        1        1        1        1
    ## 5        1        1        1        1        1        1        1        1
    ## 6        1        1        1        1        1        3        1        1
    ##   T4_IU_19 T4_IU_20 T4_IU_21 T4_IU_22 T4_IU_23 T4_IU_24 T4_IU_25 T4_IU_26
    ## 1        1        1        1        1        1        1        1        1
    ## 2        4        1        4        4        1        3        1        4
    ## 3        2        2        3        4        2        1        2        3
    ## 4        1        2        2        1        1        1        1        1
    ## 5        1        1        2        1        1        1        1        1
    ## 6        1        2        1        1        1        1        1        1
    ##   T4_IU_27 T4_SSGS_1 T4_SSGS_2 T4_SSGS_3 T4_SSGS_4 T4_SSGS_5 T4_SSGS_6
    ## 1        1         5         1         1         5         1         2
    ## 2        4         3         3         1         3         2         4
    ## 3        2         3         1         2         2         3         2
    ## 4        1         4         1         1         4         2         2
    ## 5        1         4         1         1         5         1         1
    ## 6        1         4         1         1         4         2         1
    ##   T4_SSGS_7 T4_SSGS_8 T4_SSGS_9 T4_SSGS_10 T4_SSGS_11 T4_SSGS_12 T4_SSGS_13
    ## 1         5         1         1          5          1          3          5
    ## 2         4         1         1          1          3          1          4
    ## 3         2         1         1          1          1          1          2
    ## 4         4         1         1          4          1          1          4
    ## 5         5         1         1          5          1          1          5
    ## 6         5         1         1          4          1          2          3
    ##   T4_SSGS_14 T4_SSGS_15 T4_SAFE_1 T4_SAFE_2 T4_SAFE_3 T4_SAFE_4 T4_SAFE_5
    ## 1          1          1         1         1         1         1         1
    ## 2          1          2         4         4         5         3         1
    ## 3          2          1         1         3         2         2         1
    ## 4          1          1         2         4         2         5         1
    ## 5          1          1         2         1         1         1         1
    ## 6          2          1         1         2         1         1         1
    ##   T4_SAFE_6 T4_SAFE_7 T4_SAFE_8 T4_SAFE_9 T4_SAFE_10 T4_SAFE_11 T4_SAFE_12
    ## 1         1         1         1         1          1          1          1
    ## 2         2         1         2         1          2          1          1
    ## 3         3         1         2         1          1          1          1
    ## 4         1         2         2         1          2          1          1
    ## 5         1         1         1         1          1          1          1
    ## 6         2         1         1         1          1          1          1
    ##   T4_SAFE_13 T4_SAFE_14 T4_SAFE_15 T4_SAFE_16 T4_SAFE_17 T4_SAFE_18 T4_SAFE_19
    ## 1          1          1          1          1          1          1          1
    ## 2          1          3          3          1          1          5          2
    ## 3          2          1          1          1          1          1          3
    ## 4          1          2          1          1          1          2          1
    ## 5          1          1          1          1          1          1          1
    ## 6          1          1          1          1          1          3          2
    ##   T4_SAFE_20 T4_SAFE_21 T4_SAFE_22 T4_SAFE_23 T4_SAFE_24 T4_SAFE_25 T4_SAFE_26
    ## 1          1          1          1          1          1          1          1
    ## 2          3          2          1          1          1          5          1
    ## 3          1          1          2          1          2          1          1
    ## 4          2          2          1          1          1          2          1
    ## 5          1          1          1          1          1          1          1
    ## 6          1          1          1          1          1          1          1
    ##   T4_SAFE_27 T4_SAFE_28 T4_SAFE_29 T4_SAFE_30 T4_SAFE_31 T4_SAFE_32 T4_SADS_1
    ## 1          1          1          1          1          1          1         1
    ## 2          3          3          2          1          3          5         2
    ## 3          1          3          2          1          1          3         2
    ## 4          1          2          1          1          1          3         2
    ## 5          1          1          1          1          1          1         1
    ## 6          1          2          1          1          1          3         2
    ##   T4_SADS_2 T4_SADS_3 T4_SADS_4 T4_SADS_5 T4_SADS_6 T4_SADS_7 T4_SADS_8
    ## 1         2         1         1         2         1         1         2
    ## 2         1         2         2         1         2         2         1
    ## 3         2         2         1         2         1         1         1
    ## 4         1         2         1         2         1         1         2
    ## 5         2         1         1         2         1         1         2
    ## 6         2         1         1         2         1         1         1
    ##   T4_SADS_9 T4_SADS_10 T4_SADS_11 T4_SADS_12 T4_SADS_13 T4_SADS_14 T4_SADS_15
    ## 1         1          2          2          1          2          2          1
    ## 2         2          1          1          2          1          1          2
    ## 3         1          2          1          2          2          1          1
    ## 4         1          2          2          1          2          1          1
    ## 5         1          2          2          1          2          2          1
    ## 6         2          2          1          1          2          1          1
    ##   T4_SADS_16 T4_SADS_17 T4_SADS_18 T4_SADS_19 T4_SADS_20 T4_SADS_21 T4_SADS_22
    ## 1          2          1          2          1          2          2          1
    ## 2          1          2          1          1          1          1          2
    ## 3          1          1          1          1          2          1          1
    ## 4          2          1          1          1          2          1          1
    ## 5          2          1          2          1          2          2          1
    ## 6          2          1          2          1          2          2          1
    ##   T4_SADS_23 T4_SADS_24 T4_SADS_25 T4_SADS_26 T4_SADS_27 T4_SADS_28
    ## 1          2          2          1          2          1          1
    ## 2          1          1          2          1          1          2
    ## 3          2          2          1          2          1          1
    ## 4          1          2          1          2          1          1
    ## 5          2          2          1          2          1          1
    ## 6          2          2          1          2          1          1

``` r
summary(PHData)
```

    ##    ID_Number       Gender             Age         Stud_Status    
    ##  Min.   :  1   Min.   :-99.000   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.: 55   1st Qu.:  1.000   1st Qu.: 18.0   1st Qu.:  1.00  
    ##  Median :109   Median :  2.000   Median : 18.0   Median :  1.00  
    ##  Mean   :109   Mean   : -0.576   Mean   : 16.1   Mean   : -1.76  
    ##  3rd Qu.:163   3rd Qu.:  2.000   3rd Qu.: 19.0   3rd Qu.:  1.00  
    ##  Max.   :217   Max.   :  2.000   Max.   : 24.0   Max.   :  1.00  
    ##   Program_Year     Year_School      Work_Status        Religion     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.: 12.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median : 12.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -1.41   Mean   :  9.69   Mean   : -1.22   Mean   :  1.25  
    ##  3rd Qu.:  2.00   3rd Qu.: 13.00   3rd Qu.:  3.00   3rd Qu.:  7.00  
    ##  Max.   :  5.00   Max.   : 16.00   Max.   :  3.00   Max.   : 10.00  
    ##   Religion2           Ethnicity        Ethnicity2     Ethnicity3       
    ##  Length:217         Min.   :-99.00   Min.   :-99.0   Length:217        
    ##  Class :character   1st Qu.:  2.00   1st Qu.:-99.0   Class :character  
    ##  Mode  :character   Median :  6.00   Median :-99.0   Mode  :character  
    ##                     Mean   :  1.19   Mean   :-64.4                     
    ##                     3rd Qu.:  6.00   3rd Qu.:  1.0                     
    ##                     Max.   :  6.00   Max.   :  6.0                     
    ##    USCitizen        T1_EDEQ_1        T1_EDEQ_2         T1_EDEQ_3     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  1.000   Median :  4.00  
    ##  Mean   : -2.12   Mean   :  1.82   Mean   : -0.438   Mean   :  2.15  
    ##  3rd Qu.:  1.00   3rd Qu.:  6.00   3rd Qu.:  1.000   3rd Qu.:  7.00  
    ##  Max.   :  2.00   Max.   :  7.00   Max.   :  7.000   Max.   :  7.00  
    ##    T1_EDEQ_4        T1_EDEQ_5         T1_EDEQ_6         T1_EDEQ_7      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.00   Median :  1.000   Median :  1.000   Median :  1.000  
    ##  Mean   :  1.31   Mean   : -0.447   Mean   :  0.115   Mean   : -0.793  
    ##  3rd Qu.:  6.00   3rd Qu.:  1.000   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  7.00   Max.   :  7.000   Max.   :  7.000   Max.   :  7.000  
    ##    T1_EDEQ_8         T1_EDEQ_9         T1_EDEQ_10       T1_EDEQ_11     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1.000   Median :  4.00   Median :  1.000  
    ##  Mean   : -0.525   Mean   : -0.968   Mean   :  1.47   Mean   : -0.286  
    ##  3rd Qu.:  1.000   3rd Qu.:  1.000   3rd Qu.:  7.00   3rd Qu.:  1.000  
    ##  Max.   :  7.000   Max.   :  7.000   Max.   :  7.00   Max.   :  7.000  
    ##    T1_EDEQ_12        T1_EDEQ_13       T1_EDEQ_14        T1_EDEQ_15    
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  3.00   Median :  3.000   Median :  3.00  
    ##  Mean   : -0.733   Mean   :  1.47   Mean   :  0.945   Mean   :  1.09  
    ##  3rd Qu.:  1.000   3rd Qu.:  6.00   3rd Qu.:  5.000   3rd Qu.:  6.00  
    ##  Max.   :  7.000   Max.   :  7.00   Max.   :  7.000   Max.   :  7.00  
    ##    T1_EDEQ_16      T1_EDEQ_17A       T1_EDEQ_17B     T1_EDEQ_17C   
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:-99.0   1st Qu.:-99.0  
    ##  Median :  2.00   Median :  1.000   Median :  0.0   Median :  0.0  
    ##  Mean   :  0.29   Mean   : -0.525   Mean   :-32.8   Mean   :-33.6  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.000   3rd Qu.:  0.0   3rd Qu.:  0.0  
    ##  Max.   :  7.00   Max.   :  2.000   Max.   :  7.0   Max.   :  4.0  
    ##   T1_EDEQ_18A      T1_EDEQ_18B     T1_EDEQ_19A      T1_EDEQ_19B   
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.00   Median :  0.0   Median :  1.00   Median :  0.0  
    ##  Mean   : -1.12   Mean   :-32.9   Mean   : -1.76   Mean   :-42.8  
    ##  3rd Qu.:  1.00   3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  2.00   Max.   : 10.0   Max.   :  2.00   Max.   : 18.0  
    ##   T1_EDEQ_20A       T1_EDEQ_20B     T1_EDEQ_21A      T1_EDEQ_21B   
    ##  Min.   :-99.000   Min.   :-99.0   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.000   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.000   Median :  0.0   Median :  1.00   Median :  0.0  
    ##  Mean   : -0.834   Mean   :-42.9   Mean   : -2.22   Mean   :-44.2  
    ##  3rd Qu.:  1.000   3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  2.000   Max.   :  3.0   Max.   :  2.00   Max.   :  5.0  
    ##   T1_EDEQ_22A       T1_EDEQ_22B      T1_EDEQ_23       T1_EDEQ_24    
    ##  Min.   :-99.000   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  0.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -0.544   Mean   :-32.8   Mean   : -1.14   Mean   : -2.88  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  2.000   Max.   :  7.0   Max.   :  7.00   Max.   :  5.00  
    ##    T1_EDEQ_25       T1_EDEQ_26       T1_EDEQ_27        T1_EDEQ_28    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.000   Median :  3.00  
    ##  Mean   :  1.08   Mean   :  1.32   Mean   :  0.595   Mean   :  1.36  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  3.000   3rd Qu.:  5.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.000   Max.   :  7.00  
    ##    T1_EDEQ_29        T1_EDEQ_30        T1_EDEQ_31        T1_EDEQ_32     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  4.000   Median :  1.000   Median :  2.000  
    ##  Mean   :  0.595   Mean   :  0.654   Mean   : -0.415   Mean   :  0.166  
    ##  3rd Qu.:  5.000   3rd Qu.:  5.000   3rd Qu.:  2.000   3rd Qu.:  3.000  
    ##  Max.   :  7.000   Max.   :  7.000   Max.   :  7.000   Max.   :  7.000  
    ##    T1_EDEQ_33       T1_EDEQ_34A      T1_EDEQ_34B          T1_Height    
    ##  Min.   :-99.000   Min.   :-99.000   Length:217         Min.   :-99.0  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   Class :character   1st Qu.: 64.0  
    ##  Median :  2.000   Median :  2.000   Mode  :character   Median : 66.5  
    ##  Mean   :  0.963   Mean   : -0.111                      Mean   : 63.8  
    ##  3rd Qu.:  4.000   3rd Qu.:  2.000                      3rd Qu.: 69.0  
    ##  Max.   :  7.000   Max.   :  2.000                      Max.   :165.0  
    ##    T1_Weight      T1_EDI_1         T1_EDI_2          T1_EDI_3      
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:120   1st Qu.:  3.00   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :135   Median :  4.00   Median :  3.000   Median :  3.000  
    ##  Mean   :131   Mean   :  1.52   Mean   :  0.424   Mean   :  0.737  
    ##  3rd Qu.:150   3rd Qu.:  5.00   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :250   Max.   :  6.00   Max.   :  6.000   Max.   :  6.000  
    ##     T1_EDI_4          T1_EDI_5          T1_EDI_6         T1_EDI_7      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  2.000  
    ##  Median :  3.000   Median :  2.000   Median :  3.00   Median :  3.000  
    ##  Mean   :  0.853   Mean   : -0.493   Mean   :  0.83   Mean   :  0.843  
    ##  3rd Qu.:  5.000   3rd Qu.:  3.000   3rd Qu.:  5.00   3rd Qu.:  5.000  
    ##  Max.   :  6.000   Max.   :  5.000   Max.   :  6.00   Max.   :  6.000  
    ##     T1_EDI_8         T1_EDI_9         T1_EDI_10         T1_EDI_11     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.000   Median :  3.000   Median :  1.00  
    ##  Mean   : -0.83   Mean   :  0.701   Mean   :  0.673   Mean   : -1.27  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  2.00  
    ##  Max.   :  6.00   Max.   :  6.000   Max.   :  6.000   Max.   :  6.00  
    ##    T1_EDI_12        T1_EDI_13         T1_EDI_14         T1_EDI_15     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  2.000   Median :  1.000   Median :  4.00  
    ##  Mean   :  1.04   Mean   :  0.304   Mean   : -0.816   Mean   :  1.42  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.000   3rd Qu.:  2.000   3rd Qu.:  5.00  
    ##  Max.   :  6.00   Max.   :  6.000   Max.   :  6.000   Max.   :  6.00  
    ##    T1_EDI_16        T1_EDI_17         T1_EDI_18         T1_EDI_19      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.00   Median :  1.000   Median :  1.000   Median :  2.000  
    ##  Mean   : -0.29   Mean   : -0.899   Mean   : -0.355   Mean   :  0.101  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.000   3rd Qu.:  3.000   3rd Qu.:  3.000  
    ##  Max.   :  6.00   Max.   :  6.000   Max.   :  6.000   Max.   :  6.000  
    ##    T1_EDI_20         T1_EDI_21         T1_EDI_22         T1_EDI_23     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  3.00  
    ##  Median :  1.000   Median :  3.000   Median :  1.000   Median :  4.00  
    ##  Mean   : -0.899   Mean   :  0.687   Mean   : -0.359   Mean   :  1.39  
    ##  3rd Qu.:  2.000   3rd Qu.:  5.000   3rd Qu.:  2.000   3rd Qu.:  5.00  
    ##  Max.   :  6.000   Max.   :  6.000   Max.   :  6.000   Max.   :  6.00  
    ##    T1_SAAS_1         T1_SAAS_2         T1_SAAS_3         T1_SAAS_4      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  2.000   Median :  2.000   Median :  2.000  
    ##  Mean   :  0.535   Mean   : -0.373   Mean   : -0.194   Mean   : -0.507  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SAAS_5         T1_SAAS_6         T1_SAAS_7          T1_SAAS_8      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  2.0000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  2.000   Median :  3.0000   Median :  2.000  
    ##  Mean   : -0.641   Mean   : -0.272   Mean   : -0.0553   Mean   : -0.618  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  4.0000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000  
    ##    T1_SAAS_9         T1_SAAS_10       T1_SAAS_11        T1_SAAS_12    
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  1.00   Median :  2.000   Median :  2.00  
    ##  Mean   : -0.816   Mean   : -0.88   Mean   : -0.272   Mean   : -1.06  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.00   3rd Qu.:  4.000   3rd Qu.:  3.00  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.00  
    ##    T1_SAAS_13        T1_SAAS_14       T1_SAAS_15       T1_SAAS_16     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  3.00   Median :  2.00   Median :  2.000  
    ##  Mean   : -0.309   Mean   : -0.35   Mean   : -0.47   Mean   : -0.364  
    ##  3rd Qu.:  4.000   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.000  
    ##    T1_FMPS_1         T1_FMPS_2       T1_FMPS_3         T1_FMPS_4      
    ##  Min.   :-99.000   Min.   :-99.0   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.000   1st Qu.:  3.0   1st Qu.:  1.000   1st Qu.:  2.000  
    ##  Median :  4.000   Median :  4.0   Median :  2.000   Median :  3.000  
    ##  Mean   :  0.995   Mean   :  1.1   Mean   : -0.668   Mean   :  0.364  
    ##  3rd Qu.:  5.000   3rd Qu.:  5.0   3rd Qu.:  3.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.0   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FMPS_5         T1_FMPS_6        T1_FMPS_7         T1_FMPS_8      
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  3.00   1st Qu.:  3.000   1st Qu.:  4.000  
    ##  Median :  1.000   Median :  4.00   Median :  4.000   Median :  4.000  
    ##  Mean   : -0.972   Mean   :  1.08   Mean   :  0.346   Mean   :  0.889  
    ##  3rd Qu.:  2.000   3rd Qu.:  5.00   3rd Qu.:  4.000   3rd Qu.:  5.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FMPS_9         T1_FMPS_10         T1_FMPS_11        T1_FMPS_12     
    ##  Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  2.0000   1st Qu.:  2.000   1st Qu.:  3.000  
    ##  Median :  3.000   Median :  3.0000   Median :  3.000   Median :  4.000  
    ##  Mean   : -0.415   Mean   :  0.0092   Mean   : -0.378   Mean   :  0.719  
    ##  3rd Qu.:  4.000   3rd Qu.:  4.0000   3rd Qu.:  4.000   3rd Qu.:  5.000  
    ##  Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FMPS_13        T1_FMPS_14       T1_FMPS_15       T1_FMPS_16     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.000  
    ##  Median :  2.000   Median :  2.00   Median :  2.00   Median :  4.000  
    ##  Mean   : -0.714   Mean   : -1.49   Mean   : -1.25   Mean   :  0.134  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.000  
    ##    T1_FMPS_17        T1_FMPS_18        T1_FMPS_19        T1_FMPS_20     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  3.000   1st Qu.:  3.000  
    ##  Median :  3.000   Median :  3.000   Median :  4.000   Median :  4.000  
    ##  Mean   : -0.691   Mean   : -0.479   Mean   :  0.115   Mean   : -0.171  
    ##  3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  5.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FMPS_21       T1_FMPS_22       T1_FMPS_23      T1_FMPS_24     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  2.000  
    ##  Median :  3.00   Median :  2.00   Median :  2.0   Median :  3.000  
    ##  Mean   : -1.03   Mean   : -1.18   Mean   : -1.2   Mean   : -0.226  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.000  
    ##    T1_FMPS_25        T1_FMPS_26       T1_FMPS_27       T1_FMPS_28    
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  2.000   Median :  2.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -0.714   Mean   : -1.55   Mean   :  0.23   Mean   : -1.65  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T1_FMPS_29        T1_FMPS_30        T1_FMPS_31        T1_FMPS_32     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.000   1st Qu.:  3.000   1st Qu.:  3.000   1st Qu.:  1.000  
    ##  Median :  4.000   Median :  4.000   Median :  4.000   Median :  2.000  
    ##  Mean   :  0.323   Mean   :  0.147   Mean   :  0.931   Mean   : -0.544  
    ##  3rd Qu.:  5.000   3rd Qu.:  4.000   3rd Qu.:  5.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FMPS_33         T1_FMPS_34        T1_FMPS_35       T1_SSES_1     
    ##  Min.   :-99.0000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.0000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.0000   Median :  2.000   Median :  1.00   Median :  3.00  
    ##  Mean   :  0.0369   Mean   : -0.424   Mean   : -1.35   Mean   :  1.11  
    ##  3rd Qu.:  4.0000   3rd Qu.:  3.000   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.0000   Max.   :  5.000   Max.   :  5.00   Max.   :  5.00  
    ##    T1_SSES_2         T1_SSES_3         T1_SSES_4         T1_SSES_5       
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000  
    ##  1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  1.0000  
    ##  Median :  3.000   Median :  3.000   Median :  2.000   Median :  2.0000  
    ##  Mean   :  0.539   Mean   :  0.521   Mean   :  0.147   Mean   : -0.0645  
    ##  3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  3.0000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000  
    ##    T1_SSES_6         T1_SSES_7         T1_SSES_8         T1_SSES_9      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.000   1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :  3.000   Median :  2.000   Median :  3.000   Median :  3.000  
    ##  Mean   :  0.774   Mean   :  0.226   Mean   :  0.654   Mean   :  0.866  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SSES_10         T1_SSES_11        T1_SSES_12        T1_SSES_13     
    ##  Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.0000   1st Qu.:  3.000   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :  2.0000   Median :  3.000   Median :  3.000   Median :  3.000  
    ##  Mean   : -0.0968   Mean   :  0.636   Mean   :  0.779   Mean   :  0.756  
    ##  3rd Qu.:  3.0000   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SSES_14        T1_SSES_15        T1_SSES_16        T1_SSES_17     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  2.000  
    ##  Median :  3.000   Median :  2.000   Median :  2.000   Median :  3.000  
    ##  Mean   :  0.484   Mean   : -0.226   Mean   : -0.599   Mean   :  0.313  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SSES_18        T1_SSES_19        T1_SSES_20         T1_DEQ_1     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.00  
    ##  Median :  2.000   Median :  2.000   Median :  3.000   Median :  4.00  
    ##  Mean   :  0.166   Mean   : -0.152   Mean   :  0.359   Mean   :  1.57  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  5.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  7.00  
    ##     T1_DEQ_2         T1_DEQ_3         T1_DEQ_4         T1_DEQ_5      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.000  
    ##  Median :  3.00   Median :  5.00   Median :  3.00   Median :  2.000  
    ##  Mean   :  1.01   Mean   :  1.89   Mean   :  1.21   Mean   :  0.277  
    ##  3rd Qu.:  5.00   3rd Qu.:  6.00   3rd Qu.:  5.00   3rd Qu.:  4.000  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.000  
    ##     T1_DEQ_6         T1_DEQ_7          T1_DEQ_8         T1_DEQ_9     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  2.00   1st Qu.:  4.00  
    ##  Median :  3.00   Median :  2.000   Median :  4.00   Median :  5.00  
    ##  Mean   :  0.94   Mean   :  0.668   Mean   :  1.59   Mean   :  2.38  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.000   3rd Qu.:  5.00   3rd Qu.:  6.00  
    ##  Max.   :  7.00   Max.   :  7.000   Max.   :  7.00   Max.   :  7.00  
    ##    T1_SIAS_1         T1_SIAS_2         T1_SIAS_3         T1_SIAS_4     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  2.000   Median :  2.000   Median :  2.000   Median :  2.00  
    ##  Mean   :  0.323   Mean   : -0.853   Mean   : -0.124   Mean   : -0.35  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  2.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.00  
    ##    T1_SIAS_5         T1_SIAS_6         T1_SIAS_7          T1_SIAS_8      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.0000   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  2.000   Median :  2.0000   Median :  2.000  
    ##  Mean   :  0.935   Mean   : -0.299   Mean   : -0.0737   Mean   : -0.475  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  3.0000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000  
    ##    T1_SIAS_9          T1_SIAS_10        T1_SIAS_11        T1_SIAS_12      
    ##  Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000  
    ##  1st Qu.:  2.0000   1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.0000  
    ##  Median :  3.0000   Median :  2.000   Median :  3.000   Median :  3.0000  
    ##  Mean   :  0.0323   Mean   : -0.811   Mean   : -0.433   Mean   : -0.0968  
    ##  3rd Qu.:  4.0000   3rd Qu.:  2.000   3rd Qu.:  4.000   3rd Qu.:  4.0000  
    ##  Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000  
    ##    T1_SIAS_13       T1_SIAS_14       T1_SIAS_15        T1_SIAS_16      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.0000  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.000   1st Qu.:  2.0000  
    ##  Median :  2.00   Median :  2.00   Median :  2.000   Median :  3.0000  
    ##  Mean   : -1.03   Mean   : -0.41   Mean   : -0.152   Mean   : -0.0138  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.000   3rd Qu.:  4.0000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.000   Max.   :  5.0000  
    ##    T1_SIAS_17       T1_SIAS_18        T1_SIAS_19        T1_SIAS_20     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  2.000  
    ##  Median :  2.00   Median :  3.000   Median :  2.000   Median :  3.000  
    ##  Mean   : -0.41   Mean   : -0.171   Mean   : -0.954   Mean   : -0.101  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_SPS_1          T1_SPS_2          T1_SPS_3          T1_SPS_4      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1.000   Median :  2.000   Median :  2.000  
    ##  Mean   : -0.447   Mean   : -0.553   Mean   : -0.281   Mean   : -0.442  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_SPS_5          T1_SPS_6           T1_SPS_7          T1_SPS_8      
    ##  Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.0000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  2.0000   Median :  1.000   Median :  1.000  
    ##  Mean   : -0.645   Mean   :  0.0369   Mean   : -0.724   Mean   : -0.843  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.0000   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_SPS_9         T1_SPS_10     T1_SPS_11         T1_SPS_12      
    ##  Min.   :-99.000   Min.   :-99   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1   Median :  1.000   Median :  2.000  
    ##  Mean   : -0.876   Mean   : -1   Mean   : -0.728   Mean   : -0.341  
    ##  3rd Qu.:  1.000   3rd Qu.:  1   3rd Qu.:  2.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SPS_13        T1_SPS_14        T1_SPS_15         T1_SPS_16      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  1.00   Median :  2.000   Median :  1.000  
    ##  Mean   : -1.05   Mean   : -2.29   Mean   : -0.502   Mean   : -0.751  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SPS_17         T1_SPS_18         T1_SPS_19        T1_SPS_20      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  2.000   Median :  1.00   Median :  2.000  
    ##  Mean   : -0.797   Mean   : -0.263   Mean   : -1.09   Mean   : -0.106  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  1.00   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.00   Max.   :  5.000  
    ##    T1_BFNE_1        T1_BFNE_2         T1_BFNE_3        T1_BFNE_4      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  3.00   Median :  2.000   Median :  2.00   Median :  2.000  
    ##  Mean   :  0.41   Mean   : -0.221   Mean   : -0.23   Mean   : -0.645  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.000   3rd Qu.:  4.00   3rd Qu.:  3.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.00   Max.   :  5.000  
    ##    T1_BFNE_5         T1_BFNE_6         T1_BFNE_7         T1_BFNE_8      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  2.000  
    ##  Median :  3.000   Median :  2.000   Median :  2.000   Median :  2.000  
    ##  Mean   :  0.286   Mean   :  0.258   Mean   : -0.198   Mean   : -0.235  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_BFNE_9         T1_BFNE_10        T1_BFNE_11        T1_BFNE_12     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :  3.000   Median :  2.000   Median :  3.000   Median :  2.000  
    ##  Mean   :  0.465   Mean   : -0.235   Mean   :  0.161   Mean   :  0.364  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_FPES_1        T1_FPES_2        T1_FPES_3        T1_FPES_4      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  5.00   Median :  4.00   Median :  2.00   Median :  2.000  
    ##  Mean   :  2.08   Mean   :  1.81   Mean   :  0.94   Mean   :  0.848  
    ##  3rd Qu.:  6.00   3rd Qu.:  6.00   3rd Qu.:  5.00   3rd Qu.:  4.000  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.000  
    ##    T1_FPES_5        T1_FPES_6        T1_FPES_7         T1_FPES_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  5.00   1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  7.00   Median :  5.00   Median :  2.000   Median :  3.00  
    ##  Mean   :  3.77   Mean   :  2.28   Mean   :  0.488   Mean   :  1.25  
    ##  3rd Qu.:  9.00   3rd Qu.:  7.00   3rd Qu.:  4.000   3rd Qu.:  5.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.000   Max.   : 10.00  
    ##    T1_FPES_9        T1_FPES_10        T1_CET_1         T1_CET_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  4.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  3.00   Median :  4.00   Median :  4.00  
    ##  Mean   :  1.21   Mean   :  1.47   Mean   :  1.72   Mean   :  1.32  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   :  5.00   Max.   :  5.00  
    ##     T1_CET_3         T1_CET_4          T1_CET_5          T1_CET_6      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.00   1st Qu.:  3.000   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :  4.00   Median :  4.000   Median :  3.000   Median :  3.000  
    ##  Mean   :  1.12   Mean   :  0.853   Mean   : -0.295   Mean   : -0.562  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_CET_7           T1_CET_8          T1_CET_9         T1_CET_10     
    ##  Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  2.0000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  3.0000   Median :  2.000   Median :  2.000   Median :  2.00  
    ##  Mean   :  0.0184   Mean   : -0.309   Mean   : -0.705   Mean   : -1.55  
    ##  3rd Qu.:  4.0000   3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  3.00  
    ##  Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.00  
    ##    T1_CET_11         T1_CET_12         T1_CET_13         T1_CET_14      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  3.000   1st Qu.:  2.000   1st Qu.:  3.000  
    ##  Median :  1.000   Median :  4.000   Median :  3.000   Median :  4.000  
    ##  Mean   : -0.995   Mean   :  0.641   Mean   :  0.447   Mean   :  0.341  
    ##  3rd Qu.:  2.000   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  5.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_CET_15         T1_CET_16         T1_CET_17         T1_CET_18       
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  3.000   1st Qu.:  1.0000  
    ##  Median :  2.000   Median :  2.000   Median :  4.000   Median :  3.0000  
    ##  Mean   : -0.461   Mean   : -0.654   Mean   :  0.972   Mean   :  0.0138  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  5.000   3rd Qu.:  4.0000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000  
    ##    T1_CET_19         T1_CET_20        T1_CET_21         T1_CET_22     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  2.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  3.000   Median :  2.00   Median :  2.000   Median :  2.00  
    ##  Mean   : -0.465   Mean   : -1.24   Mean   : -0.419   Mean   : -1.04  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.00   3rd Qu.:  3.000   3rd Qu.:  3.00  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.00  
    ##    T1_CET_23         T1_CET_24          T1_CIA_1          T1_CIA_2      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  3.000   Median :  1.000   Median :  2.000  
    ##  Mean   : -0.765   Mean   : -0.415   Mean   : -0.963   Mean   : -0.175  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  2.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  4.000   Max.   :  4.000  
    ##     T1_CIA_3         T1_CIA_4         T1_CIA_5         T1_CIA_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -1.07   Mean   : -1.62   Mean   : -1.62   Mean   : -1.06  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T1_CIA_7          T1_CIA_8         T1_CIA_9         T1_CIA_10     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  1.00   Median :  1.000   Median :  1.00  
    ##  Mean   : -0.972   Mean   : -1.04   Mean   : -0.595   Mean   : -1.02  
    ##  3rd Qu.:  1.000   3rd Qu.:  2.00   3rd Qu.:  2.000   3rd Qu.:  1.00  
    ##  Max.   :  4.000   Max.   :  4.00   Max.   :  4.000   Max.   :  4.00  
    ##    T1_CIA_11         T1_CIA_12        T1_CIA_13        T1_CIA_14      
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  1.00   Median :  1.00   Median :  1.000  
    ##  Mean   : -0.498   Mean   : -1.05   Mean   : -1.57   Mean   : -0.917  
    ##  3rd Qu.:  2.000   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.000  
    ##  Max.   :  4.000   Max.   :  4.00   Max.   :  4.00   Max.   :  4.000  
    ##    T1_CIA_15        T1_CIA_16         T1_PANAS_1       T1_PANAS_2     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  3.00   1st Qu.:  2.000  
    ##  Median :  1.00   Median :  2.000   Median :  4.00   Median :  3.000  
    ##  Mean   : -1.06   Mean   : -0.452   Mean   :  1.49   Mean   :  0.415  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.000   3rd Qu.:  4.00   3rd Qu.:  3.000  
    ##  Max.   :  4.00   Max.   :  4.000   Max.   :  5.00   Max.   :  5.000  
    ##    T1_PANAS_3       T1_PANAS_4         T1_PANAS_5        T1_PANAS_6     
    ##  Min.   :-99.00   Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.00   1st Qu.:  1.0000   1st Qu.:  2.000   1st Qu.:  1.000  
    ##  Median :  4.00   Median :  2.0000   Median :  3.000   Median :  2.000  
    ##  Mean   :  1.25   Mean   : -0.0461   Mean   : -0.387   Mean   : -0.346  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.0000   3rd Qu.:  4.000   3rd Qu.:  3.000  
    ##  Max.   :  5.00   Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_PANAS_7        T1_PANAS_8       T1_PANAS_9       T1_PANAS_10     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  3.000   1st Qu.:  2.000  
    ##  Median :  2.000   Median :  1.00   Median :  4.000   Median :  3.000  
    ##  Mean   : -0.166   Mean   : -1.36   Mean   :  0.691   Mean   :  0.203  
    ##  3rd Qu.:  3.000   3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##   T1_PANAS_11       T1_PANAS_12       T1_PANAS_13       T1_PANAS_14     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  2.000  
    ##  Median :  2.000   Median :  3.000   Median :  1.000   Median :  3.000  
    ##  Mean   : -0.691   Mean   :  0.272   Mean   : -0.968   Mean   :  0.336  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  2.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##   T1_PANAS_15       T1_PANAS_16      T1_PANAS_17      T1_PANAS_18     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  4.00   Median :  4.00   Median :  2.000  
    ##  Mean   :  0.258   Mean   :  1.35   Mean   :  1.23   Mean   : -0.152  
    ##  3rd Qu.:  4.000   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.000  
    ##   T1_PANAS_19       T1_PANAS_20         T1_BDI_1         T1_BDI_2      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  2.000   Median :  1.00   Median :  1.000  
    ##  Mean   :  0.848   Mean   : -0.221   Mean   : -1.41   Mean   : -0.862  
    ##  3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  2.00   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  4.00   Max.   :  4.000  
    ##     T1_BDI_3         T1_BDI_4         T1_BDI_5          T1_BDI_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.000   Median :  1.00  
    ##  Mean   : -1.35   Mean   : -1.47   Mean   : -0.885   Mean   : -1.16  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.000   3rd Qu.:  1.00  
    ##  Max.   :  3.00   Max.   :  4.00   Max.   :  4.000   Max.   :  4.00  
    ##     T1_BDI_7         T1_BDI_8         T1_BDI_9         T1_BDI_10      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  1.00   Median :  1.000   Median :  1.000  
    ##  Mean   : -1.23   Mean   : -0.71   Mean   : -0.935   Mean   : -0.972  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.000   3rd Qu.:  2.000  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.000   Max.   :  4.000  
    ##    T1_BDI_11        T1_BDI_12         T1_BDI_13        T1_BDI_14      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  1.000   Median :  1.00   Median :  1.000  
    ##  Mean   : -1.47   Mean   : -0.848   Mean   : -1.47   Mean   : -0.926  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.000   3rd Qu.:  1.00   3rd Qu.:  2.000  
    ##  Max.   :  4.00   Max.   :  4.000   Max.   :  4.00   Max.   :  4.000  
    ##    T1_BDI_15         T1_BDI_16        T1_BDI_17         T1_BDI_18     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  3.000   Median :  1.00   Median :  2.000   Median :  1.00  
    ##  Mean   :  0.189   Mean   : -1.11   Mean   : -0.304   Mean   : -1.35  
    ##  3rd Qu.:  3.000   3rd Qu.:  1.00   3rd Qu.:  3.000   3rd Qu.:  2.00  
    ##  Max.   :  7.000   Max.   :  4.00   Max.   :  7.000   Max.   :  4.00  
    ##    T1_BDI_19        T1_BDI_20       T1_FOF_AE_1     T1_FOF_AE_2    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -1.29   Mean   : -2.07   Mean   : -1.8   Mean   : -2.17  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.0   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  7.0   Max.   :  7.00  
    ##   T1_FOF_AE_3      T1_FOF_AE_4      T1_FOF_AE_5      T1_FOF_AE_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -2.18   Mean   : -2.21   Mean   : -2.06   Mean   : -1.61  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T1_FOF_AE_7      T1_FOF_AE_8     T1_FOF_FAB_1      T1_FOF_FAB_2    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  1.0   Median :  1.000   Median :  1.000  
    ##  Mean   : -2.34   Mean   : -2.7   Mean   : -0.945   Mean   : -0.756  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.0   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  7.00   Max.   :  7.0   Max.   :  7.000   Max.   :  7.000  
    ##   T1_FOF_FAB_3     T1_FOF_FAB_4     T1_FOF_FAB_5      T1_FOF_FAB_6   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  5.00   Median :  1.000   Median :  4.00  
    ##  Mean   : -1.06   Mean   :  1.47   Mean   : -0.903   Mean   :  0.12  
    ##  3rd Qu.:  2.00   3rd Qu.:  6.00   3rd Qu.:  3.000   3rd Qu.:  6.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.000   Max.   :  7.00  
    ##   T1_FOF_FAB_7     T1_FOF_FAB_8      T1_FOF_FC_1       T1_FOF_FC_2    
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.000   Median :  2.000   Median :  1.00  
    ##  Mean   : -2.42   Mean   : -0.323   Mean   : -0.189   Mean   : -1.36  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.000   Max.   :  7.000   Max.   :  7.00  
    ##   T1_FOF_FC_3      T1_FOF_FC_4       T1_FOF_FC_5      T1_FOF_FC_6     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  2.000   Median :  1.00   Median :  1.000  
    ##  Mean   : -1.08   Mean   : -0.816   Mean   : -2.11   Mean   : -0.668  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  2.00   3rd Qu.:  2.000  
    ##  Max.   :  7.00   Max.   :  7.000   Max.   :  7.00   Max.   :  7.000  
    ##   T1_FOF_FC_7      T1_FOF_FC_8       T1_FOF_FC_9       T1_FOFS_1    
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.00   Median :  1.000   Median :  1.00   Median :  1.0  
    ##  Mean   : -1.08   Mean   :  0.184   Mean   : -1.05   Mean   :-23.2  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  1.00   3rd Qu.: 24.0  
    ##  Max.   :  7.00   Max.   :  7.000   Max.   :  7.00   Max.   :100.0  
    ##    T1_FOFS_2       T1_FOFS_3       T1_FOFS_4       T1_FOFS_5     
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.00  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Median : 13.00  
    ##  Mean   :-48.2   Mean   :-62.1   Mean   :-70.3   Mean   :  1.77  
    ##  3rd Qu.:  4.0   3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.: 50.00  
    ##  Max.   :100.0   Max.   :100.0   Max.   : 30.0   Max.   :100.00  
    ##    T1_FOFS_6       T1_FOFS_7       T1_FOFS_8       T1_FOFS_9  
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Min.   :-99  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Median :-99  
    ##  Mean   :-47.3   Mean   :-54.2   Mean   :-53.6   Mean   :-65  
    ##  3rd Qu.:  4.0   3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0  
    ##  Max.   : 91.0   Max.   :100.0   Max.   : 91.0   Max.   : 86  
    ##   T1_FOFS10            T1_CES_1         T1_CES_2          T1_CES_3      
    ##  Length:217         Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  Class :character   1st Qu.:  3.00   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Mode  :character   Median :  5.00   Median :  2.000   Median :  2.000  
    ##                     Mean   :  2.62   Mean   :  0.429   Mean   :  0.825  
    ##                     3rd Qu.:  7.00   3rd Qu.:  5.000   3rd Qu.:  4.000  
    ##                     Max.   : 10.00   Max.   : 10.000   Max.   : 10.000  
    ##     T1_CES_4          T1_CES_5          T1_CES_6          T1_CES_7      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  2.000   Median :  2.000   Median :  2.000  
    ##  Mean   :  0.926   Mean   :  0.876   Mean   :  0.143   Mean   :  0.157  
    ##  3rd Qu.:  5.000   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4.000  
    ##  Max.   : 10.000   Max.   : 10.000   Max.   : 10.000   Max.   : 10.000  
    ##     T1_CES_8          T1_CES_9       T1_CES_10         T1_CES_11      
    ##  Min.   :-99.000   Min.   :-99.0   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.0   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  3.000   Median :  2.0   Median :  1.000   Median :  2.000  
    ##  Mean   :  0.825   Mean   :  1.3   Mean   : -0.562   Mean   :  0.111  
    ##  3rd Qu.:  5.000   3rd Qu.:  6.0   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   : 10.000   Max.   : 10.0   Max.   : 10.000   Max.   : 10.000  
    ##    T1_CES_12        T1_MINI_1          T1_MINI_2         T1_MINI_3      
    ##  Min.   :-99.00   Min.   :-99.0000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.0000   1st Qu.:  4.000   1st Qu.:  2.000  
    ##  Median :  3.00   Median :  2.0000   Median :  4.000   Median :  4.000  
    ##  Mean   :  1.15   Mean   : -0.0645   Mean   :  0.765   Mean   :  0.479  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.0000   3rd Qu.:  5.000   3rd Qu.:  4.000  
    ##  Max.   : 10.00   Max.   :  5.0000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_4         T1_MINI_5         T1_MINI_6         T1_MINI_7      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  3.000   Median :  3.000   Median :  2.000  
    ##  Mean   : -0.507   Mean   : -0.498   Mean   : -0.152   Mean   : -0.839  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_8         T1_MINI_9         T1_MINI_10        T1_MINI_11 
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  2.000   1st Qu.:  2  
    ##  Median :  2.000   Median :  3.000   Median :  2.000   Median :  2  
    ##  Mean   : -0.945   Mean   :  0.277   Mean   : -0.783   Mean   : -1  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  4  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5  
    ##    T1_MINI_12       T1_MINI_13       T1_MINI_14        T1_MINI_15     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  2.000   1st Qu.:  2.000  
    ##  Median :  4.00   Median :  4.00   Median :  2.000   Median :  2.000  
    ##  Mean   :  0.35   Mean   :  1.19   Mean   : -0.203   Mean   : -0.359  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.000   3rd Qu.:  3.000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_16         T1_MINI_17       T1_MINI_18       T1_MINI_19     
    ##  Min.   :-99.0000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  2.0000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.000  
    ##  Median :  3.0000   Median :  2.00   Median :  2.00   Median :  3.000  
    ##  Mean   :  0.0783   Mean   : -1.83   Mean   : -2.18   Mean   : -0.912  
    ##  3rd Qu.:  4.0000   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  4.000  
    ##  Max.   :  5.0000   Max.   :  5.00   Max.   :  4.00   Max.   :  5.000  
    ##    T1_MINI_20       T1_MINI_21        T1_MINI_22        T1_MINI_23     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  2.000   1st Qu.:  3.000   1st Qu.:  3.000  
    ##  Median :  2.00   Median :  3.000   Median :  4.000   Median :  4.000  
    ##  Mean   : -1.47   Mean   : -0.516   Mean   : -0.207   Mean   : -0.106  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_24       T1_MINI_25        T1_MINI_26         T1_MINI_27    
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.0000   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  3.000   1st Qu.:  3.0000   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  4.000   Median :  4.0000   Median :  2.00  
    ##  Mean   : -1.04   Mean   : -0.249   Mean   : -0.0461   Mean   : -1.34  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.000   3rd Qu.:  4.0000   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.0000   Max.   :  5.00  
    ##    T1_MINI_28       T1_MINI_29       T1_MINI_30       T1_MINI_31      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0000  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.0000  
    ##  Median :  3.00   Median :  3.00   Median :  2.00   Median :  4.0000  
    ##  Mean   : -1.29   Mean   : -0.83   Mean   : -2.37   Mean   :  0.0783  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.0000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  5.0000  
    ##    T1_MINI_32       T1_MINI_33       T1_MINI_34        T1_MINI_35    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.000   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  4.000   Median :  1.00  
    ##  Mean   : -1.28   Mean   : -1.45   Mean   :  0.424   Mean   : -1.59  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.000   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.000   Max.   :  5.00  
    ##    T1_MINI_36       T1_MINI_37       T1_MINI_38        T1_MINI_39     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.000   1st Qu.:  2.000  
    ##  Median :  3.00   Median :  2.00   Median :  4.000   Median :  3.000  
    ##  Mean   : -1.29   Mean   : -1.44   Mean   :  0.383   Mean   : -0.793  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_40        T1_MINI_41        T1_MINI_42        T1_MINI_43     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  4.000   1st Qu.:  1.000   1st Qu.:  3.000   1st Qu.:  3.000  
    ##  Median :  4.000   Median :  2.000   Median :  4.000   Median :  4.000  
    ##  Mean   :  0.664   Mean   : -0.323   Mean   :  0.737   Mean   :  0.719  
    ##  3rd Qu.:  5.000   3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_MINI_44       T1_MINI_45       T1_MINI_46       T1_MINI_47    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  4.00   1st Qu.:  4.00   1st Qu.:  4.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  4.00   Median :  2.00  
    ##  Mean   :  1.13   Mean   :  1.34   Mean   :  1.16   Mean   : -1.29  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T1_IU_1          T1_IU_2           T1_IU_3           T1_IU_4       
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  3.00   Median :  2.000   Median :  2.000   Median :  2.000  
    ##  Mean   :  0.47   Mean   : -0.539   Mean   : -0.341   Mean   : -0.664  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  3.000  
    ##  Max.   :  5.00   Max.   :  4.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_IU_5           T1_IU_6           T1_IU_7            T1_IU_8       
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  1.0000   1st Qu.:  3.000  
    ##  Median :  2.000   Median :  3.000   Median :  2.0000   Median :  3.000  
    ##  Mean   : -0.336   Mean   :  0.576   Mean   :  0.0599   Mean   :  0.585  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  3.0000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000  
    ##     T1_IU_9          T1_IU_10          T1_IU_11          T1_IU_12     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.000   Median :  2.000   Median :  2.00  
    ##  Mean   : -1.12   Mean   : -0.912   Mean   : -0.853   Mean   : -1.12  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.000   Max.   :  5.00  
    ##     T1_IU_13          T1_IU_14        T1_IU_15          T1_IU_16     
    ##  Min.   :-99.000   Min.   :-99.0   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.0   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  2.0   Median :  2.000   Median :  2.00  
    ##  Mean   : -0.995   Mean   : -1.2   Mean   : -0.134   Mean   : -0.71  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.0   3rd Qu.:  3.000   3rd Qu.:  4.00  
    ##  Max.   :  5.000   Max.   :  5.0   Max.   :  5.000   Max.   :  5.00  
    ##     T1_IU_17          T1_IU_18          T1_IU_19          T1_IU_20      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  3.000   Median :  2.000   Median :  2.000  
    ##  Mean   : -0.525   Mean   : -0.346   Mean   : -0.654   Mean   : -0.511  
    ##  3rd Qu.:  3.000   3rd Qu.:  4.000   3rd Qu.:  3.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_IU_21         T1_IU_22          T1_IU_23          T1_IU_24      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  2.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  3.00   Median :  2.000   Median :  2.000   Median :  1.000  
    ##  Mean   :  0.53   Mean   : -0.023   Mean   : -0.198   Mean   : -0.424  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  2.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##     T1_IU_25          T1_IU_26           T1_IU_27          T1_SSGS_1      
    ##  Min.   :-99.000   Min.   :-99.0000   Min.   :-99.0000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.0000   1st Qu.:  1.0000   1st Qu.:  3.000  
    ##  Median :  2.000   Median :  2.0000   Median :  2.0000   Median :  4.000  
    ##  Mean   : -0.493   Mean   :  0.0645   Mean   :  0.0553   Mean   :  0.664  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.0000   3rd Qu.:  3.0000   3rd Qu.:  4.000  
    ##  Max.   :  5.000   Max.   :  5.0000   Max.   :  5.0000   Max.   :  5.000  
    ##    T1_SSGS_2         T1_SSGS_3        T1_SSGS_4        T1_SSGS_5      
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1.00   Median :  4.00   Median :  2.000  
    ##  Mean   : -0.774   Mean   : -1.09   Mean   :  1.17   Mean   : -0.299  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.000  
    ##    T1_SSGS_6          T1_SSGS_7        T1_SSGS_8         T1_SSGS_9      
    ##  Min.   :-99.0000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.0000   1st Qu.:  3.00   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.0000   Median :  4.00   Median :  1.000   Median :  1.000  
    ##  Mean   : -0.0184   Mean   :  1.21   Mean   : -0.728   Mean   : -0.516  
    ##  3rd Qu.:  3.0000   3rd Qu.:  4.00   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  5.0000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SSGS_10        T1_SSGS_11        T1_SSGS_12       T1_SSGS_13    
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.000   Median :  1.000   Median :  1.00   Median :  3.00  
    ##  Mean   :  0.839   Mean   : -0.876   Mean   : -0.65   Mean   :  1.04  
    ##  3rd Qu.:  4.000   3rd Qu.:  2.000   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.00   Max.   :  5.00  
    ##    T1_SSGS_14       T1_SSGS_15        T1_SAFE_1          T1_SAFE_2      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.0000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.0000   1st Qu.:  2.000  
    ##  Median :  1.00   Median :  1.000   Median :  2.0000   Median :  2.000  
    ##  Mean   : -1.16   Mean   : -0.424   Mean   : -0.0507   Mean   :  0.212  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.000   3rd Qu.:  3.0000   3rd Qu.:  4.000  
    ##  Max.   :  5.00   Max.   :  5.000   Max.   :  5.0000   Max.   :  5.000  
    ##    T1_SAFE_3         T1_SAFE_4         T1_SAFE_5         T1_SAFE_6      
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  2.000   Median :  1.000   Median :  1.000  
    ##  Mean   : -0.788   Mean   : -0.138   Mean   : -0.765   Mean   : -0.452  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  2.000   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SAFE_7         T1_SAFE_8         T1_SAFE_9         T1_SAFE_10     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  2.000   Median :  2.000   Median :  1.000  
    ##  Mean   : -0.507   Mean   : -0.189   Mean   : -0.304   Mean   : -0.488  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SAFE_11        T1_SAFE_12        T1_SAFE_13       T1_SAFE_14     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1.000   Median :  1.00   Median :  2.000  
    ##  Mean   : -0.903   Mean   : -0.493   Mean   : -1.46   Mean   :  0.175  
    ##  3rd Qu.:  1.000   3rd Qu.:  2.000   3rd Qu.:  2.00   3rd Qu.:  3.000  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.00   Max.   :  5.000  
    ##    T1_SAFE_15        T1_SAFE_16        T1_SAFE_17        T1_SAFE_18    
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  1.000   Median :  1.000   Median :  2.00  
    ##  Mean   : -0.415   Mean   : -0.488   Mean   : -0.691   Mean   :  0.12  
    ##  3rd Qu.:  3.000   3rd Qu.:  3.000   3rd Qu.:  2.000   3rd Qu.:  3.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.00  
    ##    T1_SAFE_19        T1_SAFE_20       T1_SAFE_21        T1_SAFE_22     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.000   Median :  1.00   Median :  1.000   Median :  1.000  
    ##  Mean   : -0.447   Mean   : -1.96   Mean   : -0.641   Mean   : -0.843  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.00   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.000   Max.   :  5.000  
    ##    T1_SAFE_23        T1_SAFE_24       T1_SAFE_25       T1_SAFE_26    
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -0.682   Mean   : -1.11   Mean   : -1.76   Mean   : -0.76  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T1_SAFE_27        T1_SAFE_28        T1_SAFE_29        T1_SAFE_30    
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  1.000   Median :  1.000   Median :  1.00  
    ##  Mean   : -0.461   Mean   : -0.982   Mean   : -0.912   Mean   : -1.55  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  2.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  5.000   Max.   :  5.00  
    ##    T1_SAFE_31        T1_SAFE_32        T1_SADS_1         T1_SADS_2     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  2.000   Median :  2.000   Median :  2.00  
    ##  Mean   : -0.627   Mean   : -0.378   Mean   : -0.682   Mean   : -0.77  
    ##  3rd Qu.:  2.000   3rd Qu.:  3.000   3rd Qu.:  2.000   3rd Qu.:  2.00  
    ##  Max.   :  5.000   Max.   :  5.000   Max.   :  2.000   Max.   :  2.00  
    ##    T1_SADS_3        T1_SADS_4         T1_SADS_5         T1_SADS_6      
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.00   Median :  1.000   Median :  2.000   Median :  1.000  
    ##  Mean   : -0.65   Mean   : -0.968   Mean   : -0.558   Mean   : -0.935  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  2.00   Max.   :  2.000   Max.   :  2.000   Max.   :  2.000  
    ##    T1_SADS_7        T1_SADS_8         T1_SADS_9         T1_SADS_10     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  1.00   Median :  2.000   Median :  1.000   Median :  2.000  
    ##  Mean   : -1.03   Mean   : -0.737   Mean   : -0.935   Mean   : -0.571  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  2.00   Max.   :  2.000   Max.   :  2.000   Max.   :  2.000  
    ##    T1_SADS_11        T1_SADS_12        T1_SADS_13        T1_SADS_14     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  1.000   Median :  2.000   Median :  1.000  
    ##  Mean   : -0.797   Mean   : -0.995   Mean   : -0.659   Mean   : -0.922  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.000  
    ##  Max.   :  2.000   Max.   :  2.000   Max.   :  2.000   Max.   :  2.000  
    ##    T1_SADS_15        T1_SADS_16        T1_SADS_17        T1_SADS_18    
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.000   Median :  2.000   Median :  1.000   Median :  1.00  
    ##  Mean   : -0.834   Mean   : -0.696   Mean   : -0.917   Mean   : -1.41  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  2.00  
    ##  Max.   :  2.000   Max.   :  2.000   Max.   :  2.000   Max.   :  2.00  
    ##    T1_SADS_19       T1_SADS_20        T1_SADS_21        T1_SADS_22    
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.000   Median :  2.000   Median :  1.00  
    ##  Mean   : -1.64   Mean   : -0.659   Mean   : -0.696   Mean   : -1.08  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.000   Max.   :  2.000   Max.   :  2.00  
    ##    T1_SADS_23        T1_SADS_24        T1_SADS_25      T1_SADS_26     
    ##  Min.   :-99.000   Min.   :-99.000   Min.   :-99.0   Min.   :-99.000  
    ##  1st Qu.:  1.000   1st Qu.:  1.000   1st Qu.:  1.0   1st Qu.:  1.000  
    ##  Median :  2.000   Median :  2.000   Median :  1.0   Median :  2.000  
    ##  Mean   : -0.664   Mean   : -0.622   Mean   : -1.1   Mean   : -0.567  
    ##  3rd Qu.:  2.000   3rd Qu.:  2.000   3rd Qu.:  1.0   3rd Qu.:  2.000  
    ##  Max.   :  2.000   Max.   :  2.000   Max.   :  2.0   Max.   :  2.000  
    ##    T1_SADS_27       T1_SADS_28       T2_EDEQ_1        T2_EDEQ_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -1.09   Mean   : -1.01   Mean   : -1.26   Mean   : -2.72  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  7.00   Max.   :  7.00  
    ##    T2_EDEQ_3       T2_EDEQ_4        T2_EDEQ_5        T2_EDEQ_6     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -1.1   Mean   : -1.33   Mean   : -2.71   Mean   : -2.57  
    ##  3rd Qu.:  5.0   3rd Qu.:  4.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  7.0   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T2_EDEQ_7        T2_EDEQ_8        T2_EDEQ_9        T2_EDEQ_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -2.67   Mean   : -2.86   Mean   : -2.89   Mean   : -1.56  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  6.00  
    ##  Max.   :  7.00   Max.   :  4.00   Max.   :  5.00   Max.   :  7.00  
    ##    T2_EDEQ_11       T2_EDEQ_12       T2_EDEQ_13       T2_EDEQ_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -2.63   Mean   : -2.75   Mean   : -1.86   Mean   : -2.11  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T2_EDEQ_15      T2_EDEQ_16      T2_EDEQ_17A      T2_EDEQ_17B   
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  2.0   Median :  2.00   Median :  1.00   Median :-99.0  
    ##  Mean   : -1.1   Mean   : -2.09   Mean   : -2.93   Mean   :-50.7  
    ##  3rd Qu.:  5.0   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  7.0   Max.   :  7.00   Max.   :  2.00   Max.   : 10.0  
    ##   T2_EDEQ_17C     T2_EDEQ_18A   T2_EDEQ_18B     T2_EDEQ_19A      T2_EDEQ_19B   
    ##  Min.   :-99.0   Min.   :-99   Min.   :-99.0   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:-99.0   1st Qu.:  1   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :-99.0   Median :  1   Median :-99.0   Median :  1.00   Median :-99.0  
    ##  Mean   :-51.9   Mean   : -3   Mean   :-52.6   Mean   : -3.59   Mean   :-59.7  
    ##  3rd Qu.:  0.0   3rd Qu.:  1   3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  3.0   Max.   :  2   Max.   :  7.0   Max.   :  2.00   Max.   :  8.5  
    ##   T2_EDEQ_20A     T2_EDEQ_20B     T2_EDEQ_21A      T2_EDEQ_21B   
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.0   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.0   Median :-99.0   Median :  1.00   Median :-99.0  
    ##  Mean   : -3.6   Mean   :-61.1   Mean   : -4.07   Mean   :-61.6  
    ##  3rd Qu.:  1.0   3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  2.0   Max.   :  2.0   Max.   :  1.00   Max.   :  0.0  
    ##   T2_EDEQ_22A     T2_EDEQ_22B      T2_EDEQ_23       T2_EDEQ_24    
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :-99.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.4   Mean   :-51.8   Mean   : -3.59   Mean   : -3.36  
    ##  3rd Qu.:  1.0   3rd Qu.:  0.0   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  2.0   Max.   :  7.0   Max.   :  7.00   Max.   :  5.00  
    ##    T2_EDEQ_25       T2_EDEQ_26       T2_EDEQ_27      T2_EDEQ_28   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  2.00   Median :  2.0   Median :  2.0  
    ##  Mean   : -1.46   Mean   : -1.47   Mean   : -1.7   Mean   : -1.7  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  5.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.0   Max.   :  7.0  
    ##    T2_EDEQ_29       T2_EDEQ_30       T2_EDEQ_31       T2_EDEQ_32    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  4.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -1.53   Mean   : -1.47   Mean   : -2.55   Mean   : -2.43  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T2_EDEQ_33      T2_EDEQ_34A     T2_EDEQ_34B          T2_Height    
    ##  Min.   :-99.00   Min.   :-99.00   Length:217         Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   Class :character   1st Qu.: 64.0  
    ##  Median :  2.00   Median :  2.00   Mode  :character   Median : 66.0  
    ##  Mean   : -2.15   Mean   : -2.34                      Mean   : 57.9  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00                      3rd Qu.: 69.0  
    ##  Max.   :  7.00   Max.   :  2.00                      Max.   : 76.0  
    ##    T2_Weight      T2_EDI_1          T2_EDI_2         T2_EDI_3     
    ##  Min.   :-99   Min.   :-99.000   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:118   1st Qu.:  2.000   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :135   Median :  4.000   Median :  3.00   Median :  3.00  
    ##  Mean   :125   Mean   : -0.406   Mean   : -1.59   Mean   : -1.27  
    ##  3rd Qu.:150   3rd Qu.:  5.000   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :250   Max.   :  6.000   Max.   :  6.00   Max.   :  6.00  
    ##     T2_EDI_4         T2_EDI_5         T2_EDI_6         T2_EDI_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -1.37   Mean   : -2.59   Mean   : -1.34   Mean   : -1.38  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##     T2_EDI_8         T2_EDI_9        T2_EDI_10        T2_EDI_11     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -2.72   Mean   : -1.15   Mean   : -1.48   Mean   : -2.64  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T2_EDI_12         T2_EDI_13        T2_EDI_14        T2_EDI_15     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.000   Median :  2.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -0.894   Mean   : -1.69   Mean   : -2.72   Mean   : -0.65  
    ##  3rd Qu.:  5.000   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  5.00  
    ##  Max.   :  6.000   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T2_EDI_16        T2_EDI_17       T2_EDI_18        T2_EDI_19     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.0   Median :  1.00   Median :  2.00  
    ##  Mean   : -1.87   Mean   : -2.9   Mean   : -2.13   Mean   : -1.78  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.0   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  6.00   Max.   :  6.0   Max.   :  6.00   Max.   :  6.00  
    ##    T2_EDI_20        T2_EDI_21        T2_EDI_22        T2_EDI_23      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.000  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  3.000  
    ##  Mean   : -2.82   Mean   : -1.18   Mean   : -2.77   Mean   : -0.664  
    ##  3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  5.000  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.000  
    ##    T2_SAAS_1        T2_SAAS_2        T2_SAAS_3        T2_SAAS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -1.41   Mean   : -2.33   Mean   : -2.27   Mean   : -3.38  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAAS_5        T2_SAAS_6        T2_SAAS_7        T2_SAAS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -2.57   Mean   : -2.31   Mean   : -2.12   Mean   : -2.65  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAAS_9        T2_SAAS_10       T2_SAAS_11       T2_SAAS_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.72   Mean   : -2.75   Mean   : -2.24   Mean   : -2.54  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAAS_13      T2_SAAS_14       T2_SAAS_15       T2_SAAS_16    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -2.3   Mean   : -1.99   Mean   : -3.02   Mean   : -2.34  
    ##  3rd Qu.:  3.0   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_1        T2_FMPS_2        T2_FMPS_3       T2_FMPS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  1.0   Median :  3.00  
    ##  Mean   : -1.69   Mean   : -1.85   Mean   : -3.2   Mean   : -2.47  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  2.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_FMPS_5        T2_FMPS_6        T2_FMPS_7        T2_FMPS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  4.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -3.28   Mean   : -1.88   Mean   : -2.05   Mean   : -1.13  
    ##  3rd Qu.:  2.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_9        T2_FMPS_10       T2_FMPS_11       T2_FMPS_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  3.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -2.73   Mean   : -2.55   Mean   : -2.24   Mean   : -1.44  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_13       T2_FMPS_14       T2_FMPS_15       T2_FMPS_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -2.77   Mean   : -3.51   Mean   : -2.68   Mean   : -1.47  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_17       T2_FMPS_18       T2_FMPS_19       T2_FMPS_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  3.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -2.25   Mean   : -2.12   Mean   : -1.49   Mean   : -2.65  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_21       T2_FMPS_22       T2_FMPS_23      T2_FMPS_24    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.0   Median :  3.00  
    ##  Mean   : -2.65   Mean   : -3.07   Mean   : -3.5   Mean   : -2.09  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_FMPS_25       T2_FMPS_26       T2_FMPS_27       T2_FMPS_28   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  2.00   Median :  4.00   Median :  2.0  
    ##  Mean   : -3.27   Mean   : -2.99   Mean   : -1.29   Mean   : -2.7  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T2_FMPS_29       T2_FMPS_30       T2_FMPS_31       T2_FMPS_32    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  3.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -1.57   Mean   : -1.84   Mean   : -1.46   Mean   : -2.91  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FMPS_33       T2_FMPS_34       T2_FMPS_35       T2_SSES_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  2.00   Median :  1.00   Median :  4.00  
    ##  Mean   : -2.48   Mean   : -3.16   Mean   : -3.18   Mean   : -1.65  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSES_2       T2_SSES_3        T2_SSES_4        T2_SSES_5     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.0   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.0   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.8   Mean   : -2.66   Mean   : -3.65   Mean   : -2.92  
    ##  3rd Qu.:  4.0   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSES_6        T2_SSES_7        T2_SSES_8       T2_SSES_9     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.0   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.0   Median :  3.00  
    ##  Mean   : -2.55   Mean   : -2.65   Mean   : -2.4   Mean   : -2.46  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_SSES_10       T2_SSES_11       T2_SSES_12       T2_SSES_13    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  3.00   Median :  3.00  
    ##  Mean   : -2.94   Mean   : -2.78   Mean   : -2.05   Mean   : -2.37  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSES_14      T2_SSES_15       T2_SSES_16       T2_SSES_17    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.0   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -1.9   Mean   : -3.05   Mean   : -2.95   Mean   : -2.32  
    ##  3rd Qu.:  4.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSES_18       T2_SSES_19       T2_SSES_20        T2_DEQ_1  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  4  
    ##  Mean   : -2.77   Mean   : -2.98   Mean   : -3.17   Mean   : -1  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  7  
    ##     T2_DEQ_2         T2_DEQ_3         T2_DEQ_4         T2_DEQ_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -2.12   Mean   : -1.17   Mean   : -2.34   Mean   : -2.65  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##     T2_DEQ_6         T2_DEQ_7         T2_DEQ_8         T2_DEQ_9      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.000  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  4.000  
    ##  Median :  2.00   Median :  2.00   Median :  4.00   Median :  5.000  
    ##  Mean   : -2.14   Mean   : -2.65   Mean   : -1.43   Mean   : -0.931  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  6.000  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.000  
    ##    T2_SIAS_1        T2_SIAS_2        T2_SIAS_3        T2_SIAS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.29   Mean   : -2.79   Mean   : -2.09   Mean   : -2.63  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SIAS_5        T2_SIAS_6        T2_SIAS_7        T2_SIAS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -1.52   Mean   : -2.58   Mean   : -2.41   Mean   : -2.69  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SIAS_9        T2_SIAS_10       T2_SIAS_11       T2_SIAS_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -1.88   Mean   : -3.16   Mean   : -1.76   Mean   : -2.57  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SIAS_13       T2_SIAS_14       T2_SIAS_15       T2_SIAS_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.91   Mean   : -2.82   Mean   : -2.63   Mean   : -2.53  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SIAS_17      T2_SIAS_18       T2_SIAS_19       T2_SIAS_20    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.8   Mean   : -3.23   Mean   : -3.33   Mean   : -2.58  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_SPS_1         T2_SPS_2         T2_SPS_3         T2_SPS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.27   Mean   : -3.39   Mean   : -3.11   Mean   : -3.18  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_SPS_5         T2_SPS_6         T2_SPS_7         T2_SPS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.94   Mean   : -2.89   Mean   : -3.49   Mean   : -3.51  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_SPS_9        T2_SPS_10        T2_SPS_11        T2_SPS_12     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -3.69   Mean   : -3.76   Mean   : -3.56   Mean   : -3.18  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SPS_13       T2_SPS_14       T2_SPS_15        T2_SPS_16     
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  1.0   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.3   Mean   : -4.1   Mean   : -3.75   Mean   : -3.48  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.0   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SPS_17        T2_SPS_18        T2_SPS_19       T2_SPS_20     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.0   Median :  2.00  
    ##  Mean   : -3.99   Mean   : -2.73   Mean   : -4.3   Mean   : -2.95  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_BFNE_1        T2_BFNE_2       T2_BFNE_3        T2_BFNE_4     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.0   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.13   Mean   : -3.9   Mean   : -3.25   Mean   : -3.31  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T2_BFNE_5       T2_BFNE_6        T2_BFNE_7        T2_BFNE_8     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.2   Mean   : -4.16   Mean   : -3.25   Mean   : -3.21  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_BFNE_9        T2_BFNE_10       T2_BFNE_11       T2_BFNE_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -3.03   Mean   : -3.33   Mean   : -3.36   Mean   : -3.16  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_FPES_1       T2_FPES_2        T2_FPES_3        T2_FPES_4     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.0   Median :  4.00   Median :  3.00   Median :  3.00  
    ##  Mean   : -1.6   Mean   : -1.56   Mean   : -2.36   Mean   : -2.76  
    ##  3rd Qu.:  5.0   3rd Qu.:  6.00   3rd Qu.:  5.00   3rd Qu.:  5.00  
    ##  Max.   : 10.0   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##    T2_FPES_5         T2_FPES_6        T2_FPES_7        T2_FPES_8     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  5.000   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  6.000   Median :  5.00   Median :  2.00   Median :  3.00  
    ##  Mean   :  0.751   Mean   : -1.18   Mean   : -2.54   Mean   : -1.95  
    ##  3rd Qu.:  8.000   3rd Qu.:  6.00   3rd Qu.:  5.00   3rd Qu.:  5.00  
    ##  Max.   : 10.000   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##    T2_FPES_9        T2_FPES_10        T2_CET_1         T2_CET_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  3.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -2.01   Mean   : -1.75   Mean   : -1.77   Mean   : -2.59  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_CET_3         T2_CET_4         T2_CET_5         T2_CET_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -1.82   Mean   : -3.07   Mean   : -2.78   Mean   : -3.46  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_CET_7         T2_CET_8         T2_CET_9        T2_CET_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.21   Mean   : -3.16   Mean   : -4.95   Mean   : -4.46  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_CET_11        T2_CET_12        T2_CET_13       T2_CET_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.0   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  3.00   Median :  3.0   Median :  4.00  
    ##  Mean   : -4.69   Mean   : -2.77   Mean   : -3.3   Mean   : -2.95  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_CET_15        T2_CET_16        T2_CET_17       T2_CET_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  4.0   Median :  2.00  
    ##  Mean   : -4.21   Mean   : -3.88   Mean   : -2.4   Mean   : -4.35  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_CET_19       T2_CET_20        T2_CET_21        T2_CET_22    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  3.0   Median :  2.00   Median :  2.00   Median :  2.0  
    ##  Mean   : -3.8   Mean   : -4.55   Mean   : -3.67   Mean   : -4.3  
    ##  3rd Qu.:  4.0   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.0  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T2_CET_23        T2_CET_24         T2_CIA_1         T2_CIA_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -3.46   Mean   : -2.79   Mean   : -4.67   Mean   : -4.01  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  1.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  4.00  
    ##     T2_CIA_3         T2_CIA_4         T2_CIA_5         T2_CIA_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.72   Mean   : -4.76   Mean   : -5.28   Mean   : -4.79  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T2_CIA_7         T2_CIA_8         T2_CIA_9        T2_CIA_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.72   Mean   : -4.77   Mean   : -4.31   Mean   : -5.19  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T2_CIA_11        T2_CIA_12        T2_CIA_13        T2_CIA_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -4.71   Mean   : -5.15   Mean   : -5.24   Mean   : -5.5  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.0  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.0  
    ##    T2_CIA_15        T2_CIA_16        T2_PANAS_1       T2_PANAS_2   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.0  
    ##  Median :  1.00   Median :  1.00   Median :  4.00   Median :  2.0  
    ##  Mean   : -5.23   Mean   : -4.71   Mean   : -2.54   Mean   : -3.5  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.0  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  5.00   Max.   :  5.0  
    ##    T2_PANAS_3       T2_PANAS_4       T2_PANAS_5      T2_PANAS_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  2.0   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.0   Median :  2.00  
    ##  Mean   : -2.76   Mean   : -3.77   Mean   : -3.5   Mean   : -4.03  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_PANAS_7       T2_PANAS_8       T2_PANAS_9      T2_PANAS_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  1.00   Median :  3.00   Median :  3.00  
    ##  Mean   : -4.05   Mean   : -4.59   Mean   : -2.83   Mean   : -3.18  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T2_PANAS_11     T2_PANAS_12      T2_PANAS_13      T2_PANAS_14    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.0   Median :  3.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -3.9   Mean   : -3.66   Mean   : -4.24   Mean   : -3.12  
    ##  3rd Qu.:  3.0   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T2_PANAS_15      T2_PANAS_16      T2_PANAS_17     T2_PANAS_18    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  3.0   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  4.00   Median :  3.0   Median :  2.00  
    ##  Mean   : -3.35   Mean   : -2.57   Mean   : -2.8   Mean   : -3.96  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##   T2_PANAS_19      T2_PANAS_20        T2_BDI_1         T2_BDI_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.08   Mean   : -4.05   Mean   : -3.76   Mean   : -4.12  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  4.00  
    ##     T2_BDI_3         T2_BDI_4         T2_BDI_5         T2_BDI_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.18   Mean   : -4.26   Mean   : -4.22   Mean   : -4.86  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  3.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T2_BDI_7         T2_BDI_8         T2_BDI_9        T2_BDI_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -3.65   Mean   : -4.05   Mean   : -4.31   Mean   : -3.8  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.0  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.0  
    ##    T2_BDI_11        T2_BDI_12        T2_BDI_13        T2_BDI_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.74   Mean   : -4.17   Mean   : -4.29   Mean   : -4.09  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T2_BDI_15        T2_BDI_16        T2_BDI_17        T2_BDI_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.25   Mean   : -3.81   Mean   : -3.33   Mean   : -3.66  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  7.00   Max.   :  3.00   Max.   :  7.00   Max.   :  4.00  
    ##    T2_BDI_19        T2_BDI_20      T2_FOF_AE_1      T2_FOF_AE_2    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.52   Mean   : -4.4   Mean   : -4.12   Mean   : -4.55  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.0   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.0   Max.   :  7.00   Max.   :  7.00  
    ##   T2_FOF_AE_3      T2_FOF_AE_4      T2_FOF_AE_5      T2_FOF_AE_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.53   Mean   : -4.51   Mean   : -4.88   Mean   : -5.04  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T2_FOF_AE_7     T2_FOF_AE_8      T2_FOF_FAB_1     T2_FOF_FAB_2   
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.6   Mean   : -4.56   Mean   : -4.17   Mean   : -4.06  
    ##  3rd Qu.:  1.0   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  7.0   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T2_FOF_FAB_3     T2_FOF_FAB_4     T2_FOF_FAB_5    T2_FOF_FAB_6   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  4.00   Median :  1.0   Median :  3.00  
    ##  Mean   : -4.76   Mean   : -2.27   Mean   : -4.2   Mean   : -2.71  
    ##  3rd Qu.:  2.00   3rd Qu.:  6.00   3rd Qu.:  2.0   3rd Qu.:  5.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.0   Max.   :  7.00  
    ##   T2_FOF_FAB_7     T2_FOF_FAB_8     T2_FOF_FC_1      T2_FOF_FC_2    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.88   Mean   : -3.81   Mean   : -3.28   Mean   : -4.06  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T2_FOF_FC_3      T2_FOF_FC_4      T2_FOF_FC_5      T2_FOF_FC_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.93   Mean   : -3.34   Mean   : -4.97   Mean   : -4.02  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T2_FOF_FC_7      T2_FOF_FC_8      T2_FOF_FC_9       T2_FOFS_1    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  0.0  
    ##  Mean   : -4.08   Mean   : -3.38   Mean   : -4.71   Mean   :-29.4  
    ##  3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.: 10.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :100.0  
    ##    T2_FOFS_2       T2_FOFS_3       T2_FOFS_4       T2_FOFS_5     
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.00  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Median :  9.00  
    ##  Mean   :-56.6   Mean   :-64.6   Mean   :-69.7   Mean   : -7.57  
    ##  3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.: 38.00  
    ##  Max.   :100.0   Max.   :100.0   Max.   : 55.0   Max.   :100.00  
    ##    T2_FOFS_6       T2_FOFS_7       T2_FOFS_8       T2_FOFS_9    
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Median :-99.0  
    ##  Mean   :-51.8   Mean   :-53.2   Mean   :-56.9   Mean   :-63.3  
    ##  3rd Qu.:  1.0   3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0.0  
    ##  Max.   :100.0   Max.   :100.0   Max.   :100.0   Max.   :100.0  
    ##   T2_FOFS10            T2_CES_1          T2_CES_2         T2_CES_3     
    ##  Length:217         Min.   :-99.000   Min.   :-99.00   Min.   :-99.00  
    ##  Class :character   1st Qu.:  2.000   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Mode  :character   Median :  4.000   Median :  2.00   Median :  2.00  
    ##                     Mean   : -0.995   Mean   : -2.53   Mean   : -2.44  
    ##                     3rd Qu.:  7.000   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##                     Max.   : 10.000   Max.   : 10.00   Max.   : 10.00  
    ##     T2_CES_4         T2_CES_5         T2_CES_6         T2_CES_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.28   Mean   : -2.36   Mean   : -3.43   Mean   : -2.57  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##     T2_CES_8         T2_CES_9        T2_CES_10       T2_CES_11     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.0   Median :  2.00  
    ##  Mean   : -2.24   Mean   : -2.25   Mean   : -3.7   Mean   : -2.55  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.0   Max.   : 10.00  
    ##    T2_CES_12        T2_MINI_1        T2_MINI_2        T2_MINI_3     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  4.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -2.56   Mean   : -3.23   Mean   : -2.13   Mean   : -2.87  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_4        T2_MINI_5        T2_MINI_6        T2_MINI_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -4.31   Mean   : -2.91   Mean   : -3.46   Mean   : -4.04  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_8        T2_MINI_9        T2_MINI_10       T2_MINI_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -4.76   Mean   : -2.93   Mean   : -3.97   Mean   : -3.27  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_12       T2_MINI_13       T2_MINI_14       T2_MINI_15    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.94   Mean   : -2.18   Mean   : -3.56   Mean   : -3.69  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_16       T2_MINI_17       T2_MINI_18       T2_MINI_19    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -3.29   Mean   : -4.64   Mean   : -4.46   Mean   : -3.59  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  4.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_20       T2_MINI_21       T2_MINI_22       T2_MINI_23    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  3.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -4.85   Mean   : -3.27   Mean   : -3.53   Mean   : -4.37  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_24       T2_MINI_25      T2_MINI_26       T2_MINI_27 
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  2.00   1st Qu.:  2.0   1st Qu.:  3.00   1st Qu.:  1  
    ##  Median :  3.00   Median :  4.0   Median :  4.00   Median :  2  
    ##  Mean   : -3.84   Mean   : -3.1   Mean   : -2.94   Mean   : -5  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  4.00   3rd Qu.:  3  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5  
    ##    T2_MINI_28       T2_MINI_29       T2_MINI_30       T2_MINI_31    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  3.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -4.97   Mean   : -3.65   Mean   : -5.07   Mean   : -2.77  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_32       T2_MINI_33       T2_MINI_34      T2_MINI_35    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  4.0   Median :  1.00  
    ##  Mean   : -4.05   Mean   : -4.59   Mean   : -2.9   Mean   : -4.87  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.0   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  4.00  
    ##    T2_MINI_36       T2_MINI_37       T2_MINI_38       T2_MINI_39    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -3.61   Mean   : -4.13   Mean   : -2.94   Mean   : -3.55  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_40       T2_MINI_41       T2_MINI_42       T2_MINI_43    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  4.00   Median :  2.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -3.34   Mean   : -4.08   Mean   : -2.57   Mean   : -2.55  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_MINI_44       T2_MINI_45       T2_MINI_46       T2_MINI_47    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  4.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -2.24   Mean   : -2.11   Mean   : -2.77   Mean   : -4.06  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_1          T2_IU_2          T2_IU_3          T2_IU_4      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.89   Mean   : -4.63   Mean   : -4.56   Mean   : -5.39  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_5          T2_IU_6          T2_IU_7          T2_IU_8      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -4.19   Mean   : -3.87   Mean   : -4.72   Mean   : -3.63  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_9       T2_IU_10         T2_IU_11         T2_IU_12     
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -5   Mean   : -4.41   Mean   : -4.78   Mean   : -4.99  
    ##  3rd Qu.:  3   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_13         T2_IU_14         T2_IU_15         T2_IU_16     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.68   Mean   : -4.91   Mean   : -4.82   Mean   : -5.02  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_17         T2_IU_18         T2_IU_19        T2_IU_20     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.0   Median :  2.00  
    ##  Mean   : -4.82   Mean   : -4.31   Mean   : -4.9   Mean   : -5.75  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##     T2_IU_21        T2_IU_22         T2_IU_23         T2_IU_24     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.0   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -4.2   Mean   : -3.86   Mean   : -3.87   Mean   : -4.62  
    ##  3rd Qu.:  4.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T2_IU_25         T2_IU_26         T2_IU_27        T2_SSGS_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -4.28   Mean   : -4.16   Mean   : -3.68   Mean   : -2.67  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSGS_2        T2_SSGS_3        T2_SSGS_4        T2_SSGS_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -4.46   Mean   : -4.78   Mean   : -2.71   Mean   : -4.18  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSGS_6        T2_SSGS_7        T2_SSGS_8        T2_SSGS_9     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  4.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.95   Mean   : -2.61   Mean   : -4.42   Mean   : -4.29  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSGS_10       T2_SSGS_11       T2_SSGS_12       T2_SSGS_13    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  1.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -2.48   Mean   : -4.52   Mean   : -4.35   Mean   : -2.89  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SSGS_14       T2_SSGS_15       T2_SAFE_1        T2_SAFE_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.37   Mean   : -3.81   Mean   : -3.88   Mean   : -3.75  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_3        T2_SAFE_4        T2_SAFE_5        T2_SAFE_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -5.18   Mean   : -4.83   Mean   : -4.48   Mean   : -4.27  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_7        T2_SAFE_8        T2_SAFE_9        T2_SAFE_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.69   Mean   : -4.98   Mean   : -4.15   Mean   : -4.73  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_11       T2_SAFE_12       T2_SAFE_13      T2_SAFE_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  2.00  
    ##  Mean   : -4.59   Mean   : -4.34   Mean   : -4.3   Mean   : -3.68  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T2_SAFE_15       T2_SAFE_16       T2_SAFE_17       T2_SAFE_18    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -4.23   Mean   : -4.32   Mean   : -4.43   Mean   : -3.86  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_19       T2_SAFE_20       T2_SAFE_21       T2_SAFE_22    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.25   Mean   : -4.28   Mean   : -4.38   Mean   : -5.02  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_23       T2_SAFE_24       T2_SAFE_25       T2_SAFE_26    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.85   Mean   : -4.44   Mean   : -4.59   Mean   : -4.98  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_27       T2_SAFE_28       T2_SAFE_29       T2_SAFE_30    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -5.15   Mean   : -4.32   Mean   : -5.17   Mean   : -4.75  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T2_SAFE_31       T2_SAFE_32       T2_SADS_1        T2_SADS_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -4.79   Mean   : -4.22   Mean   : -4.04   Mean   : -3.97  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  2.00   Max.   :  2.00  
    ##    T2_SADS_3        T2_SADS_4        T2_SADS_5        T2_SADS_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.99   Mean   : -4.75   Mean   : -4.28   Mean   : -4.68  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T2_SADS_7        T2_SADS_8        T2_SADS_9        T2_SADS_10   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  2.0  
    ##  Mean   : -4.76   Mean   : -4.46   Mean   : -4.69   Mean   : -4.3  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.0  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.0  
    ##    T2_SADS_11       T2_SADS_12       T2_SADS_13      T2_SADS_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.0   Median :  1.00  
    ##  Mean   : -4.93   Mean   : -4.71   Mean   : -4.4   Mean   : -4.65  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.0   Max.   :  2.00  
    ##    T2_SADS_15       T2_SADS_16       T2_SADS_17       T2_SADS_18    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.57   Mean   : -4.41   Mean   : -5.11   Mean   : -4.53  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T2_SADS_19       T2_SADS_20       T2_SADS_21       T2_SADS_22   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  1.0  
    ##  Mean   : -4.87   Mean   : -4.38   Mean   : -4.44   Mean   : -5.7  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.0  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.0  
    ##    T2_SADS_23      T2_SADS_24       T2_SADS_25       T2_SADS_26    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -4.4   Mean   : -5.31   Mean   : -4.78   Mean   : -5.26  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  2.0   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T2_SADS_27       T2_SADS_28      T3_EDEQ_1       T3_EDEQ_2     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  1.0   Median :  1.00  
    ##  Mean   : -4.79   Mean   : -4.7   Mean   : -2.6   Mean   : -3.73  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.0   3rd Qu.:  3.0   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.0   Max.   :  7.0   Max.   :  7.00  
    ##    T3_EDEQ_3        T3_EDEQ_4        T3_EDEQ_5       T3_EDEQ_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -2.41   Mean   : -2.65   Mean   : -3.7   Mean   : -3.53  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  1.0   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.0   Max.   :  7.00  
    ##    T3_EDEQ_7        T3_EDEQ_8        T3_EDEQ_9        T3_EDEQ_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -3.68   Mean   : -3.84   Mean   : -4.26   Mean   : -3.73  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  6.00   Max.   :  7.00  
    ##    T3_EDEQ_11      T3_EDEQ_12       T3_EDEQ_13       T3_EDEQ_14    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -4.5   Mean   : -3.74   Mean   : -2.62   Mean   : -3.25  
    ##  3rd Qu.:  1.0   3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  7.0   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T3_EDEQ_15       T3_EDEQ_16      T3_EDEQ_17A      T3_EDEQ_17B   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :-99.0  
    ##  Mean   : -2.84   Mean   : -3.65   Mean   : -4.35   Mean   :-58.5  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  2.00   Max.   :  7.0  
    ##   T3_EDEQ_17C     T3_EDEQ_18A      T3_EDEQ_18B     T3_EDEQ_19A    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0   1st Qu.:  1.00  
    ##  Median :-99.0   Median :  1.00   Median :-99.0   Median :  1.00  
    ##  Mean   :-59.7   Mean   : -3.97   Mean   :-60.9   Mean   : -4.96  
    ##  3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0   3rd Qu.:  1.00  
    ##  Max.   :  3.0   Max.   :  2.00   Max.   :  5.5   Max.   :  2.00  
    ##   T3_EDEQ_19B     T3_EDEQ_20A      T3_EDEQ_20B     T3_EDEQ_21A    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0   1st Qu.:  1.00  
    ##  Median :-99.0   Median :  1.00   Median :-99.0   Median :  1.00  
    ##  Mean   :-65.6   Mean   : -4.52   Mean   :-66.1   Mean   : -5.44  
    ##  3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0   3rd Qu.:  1.00  
    ##  Max.   :  7.0   Max.   :  2.00   Max.   :  6.0   Max.   :  2.00  
    ##   T3_EDEQ_21B     T3_EDEQ_22A      T3_EDEQ_22B      T3_EDEQ_23    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:-99.0   1st Qu.:  1.00  
    ##  Median :-99.0   Median :  1.00   Median :-99.0   Median :  1.00  
    ##  Mean   :-66.6   Mean   : -3.89   Mean   :-57.9   Mean   : -3.65  
    ##  3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  0.0   3rd Qu.:  1.00  
    ##  Max.   :  0.0   Max.   :  2.00   Max.   :  7.0   Max.   :  7.00  
    ##    T3_EDEQ_24       T3_EDEQ_25       T3_EDEQ_26       T3_EDEQ_27    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -4.31   Mean   : -2.68   Mean   : -3.17   Mean   : -2.88  
    ##  3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T3_EDEQ_28       T3_EDEQ_29       T3_EDEQ_30       T3_EDEQ_31 
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  2.00   Median :  2.00   Median :  3.00   Median :  1  
    ##  Mean   : -2.32   Mean   : -2.72   Mean   : -2.23   Mean   : -4  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  1  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7  
    ##    T3_EDEQ_32       T3_EDEQ_33      T3_EDEQ_34A     T3_EDEQ_34B       
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Length:217        
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   Class :character  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Mode  :character  
    ##  Mean   : -2.93   Mean   : -2.81   Mean   : -3.75                     
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00                     
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  2.00                     
    ##    T3_Height       T3_Weight      T3_EDI_1         T3_EDI_2     
    ##  Min.   :-99.0   Min.   :-99   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.: 63.5   1st Qu.:117   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median : 66.0   Median :134   Median :  4.00   Median :  2.00  
    ##  Mean   : 55.6   Mean   :120   Mean   : -1.16   Mean   : -2.52  
    ##  3rd Qu.: 69.0   3rd Qu.:150   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   : 76.0   Max.   :250   Max.   :  6.00   Max.   :  6.00  
    ##     T3_EDI_3        T3_EDI_4         T3_EDI_5         T3_EDI_6     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.8   Mean   : -2.42   Mean   : -4.09   Mean   : -2.86  
    ##  3rd Qu.:  4.0   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  6.0   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##     T3_EDI_7         T3_EDI_8         T3_EDI_9        T3_EDI_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -3.85   Mean   : -5.07   Mean   : -3.39   Mean   : -4.41  
    ##  3rd Qu.:  4.00   3rd Qu.:  1.00   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T3_EDI_11        T3_EDI_12        T3_EDI_13       T3_EDI_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  2.0   Median :  1.00  
    ##  Mean   : -4.96   Mean   : -3.17   Mean   : -3.2   Mean   : -4.16  
    ##  3rd Qu.:  2.00   3rd Qu.:  5.00   3rd Qu.:  3.0   3rd Qu.:  1.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.0   Max.   :  6.00  
    ##    T3_EDI_15        T3_EDI_16        T3_EDI_17        T3_EDI_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -1.94   Mean   : -3.79   Mean   : -4.24   Mean   : -3.97  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  3.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T3_EDI_19        T3_EDI_20        T3_EDI_21        T3_EDI_22     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.71   Mean   : -4.62   Mean   : -3.57   Mean   : -4.11  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  5.00   3rd Qu.:  2.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T3_EDI_23       T3_SAAS_1        T3_SAAS_2        T3_SAAS_3     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.0   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.0   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.7   Mean   : -2.84   Mean   : -4.34   Mean   : -3.84  
    ##  3rd Qu.:  5.0   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  6.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAAS_4        T3_SAAS_5        T3_SAAS_6        T3_SAAS_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.36   Mean   : -4.41   Mean   : -4.23   Mean   : -4.11  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAAS_8        T3_SAAS_9        T3_SAAS_10       T3_SAAS_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -5.02   Mean   : -4.64   Mean   : -5.06   Mean   : -3.73  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAAS_12       T3_SAAS_13      T3_SAAS_14    T3_SAAS_15    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.0   Median :  2   Median :  1.00  
    ##  Mean   : -3.99   Mean   : -4.6   Mean   : -4   Mean   : -4.44  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  4   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5   Max.   :  5.00  
    ##    T3_SAAS_16       T3_FMPS_1        T3_FMPS_2        T3_FMPS_3     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  4.00   Median :  1.00  
    ##  Mean   : -4.24   Mean   : -2.77   Mean   : -2.33   Mean   : -4.14  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_4        T3_FMPS_5        T3_FMPS_6        T3_FMPS_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  1.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -3.55   Mean   : -4.67   Mean   : -2.75   Mean   : -2.99  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_8        T3_FMPS_9       T3_FMPS_10       T3_FMPS_11    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.0   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  4.00   Median :  2.0   Median :  3.00   Median :  3.00  
    ##  Mean   : -2.62   Mean   : -4.6   Mean   : -3.66   Mean   : -3.18  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.0   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_12       T3_FMPS_13       T3_FMPS_14    T3_FMPS_15    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  2.00   Median :  2   Median :  2.00  
    ##  Mean   : -2.31   Mean   : -3.76   Mean   : -4   Mean   : -4.54  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  3   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5   Max.   :  5.00  
    ##    T3_FMPS_16       T3_FMPS_17      T3_FMPS_18       T3_FMPS_19    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  2.0   1st Qu.:  2.00   1st Qu.:  3.00  
    ##  Median :  4.00   Median :  3.0   Median :  3.00   Median :  4.00  
    ##  Mean   : -2.88   Mean   : -4.3   Mean   : -3.61   Mean   : -2.93  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_20       T3_FMPS_21       T3_FMPS_22      T3_FMPS_23    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.0   Median :  2.00  
    ##  Mean   : -3.26   Mean   : -3.59   Mean   : -4.9   Mean   : -3.98  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T3_FMPS_24       T3_FMPS_25       T3_FMPS_26       T3_FMPS_27    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -3.08   Mean   : -3.73   Mean   : -4.81   Mean   : -3.23  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_28       T3_FMPS_29       T3_FMPS_30       T3_FMPS_31    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -4.63   Mean   : -2.92   Mean   : -3.69   Mean   : -2.44  
    ##  3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_FMPS_32       T3_FMPS_33       T3_FMPS_34       T3_FMPS_35    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.87   Mean   : -3.48   Mean   : -3.75   Mean   : -4.07  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSES_1       T3_SSES_2        T3_SSES_3        T3_SSES_4    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  3.0   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.0  
    ##  Median :  4.0   Median :  3.00   Median :  3.00   Median :  2.0  
    ##  Mean   : -2.1   Mean   : -2.93   Mean   : -2.72   Mean   : -3.7  
    ##  3rd Qu.:  4.0   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.0  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T3_SSES_5        T3_SSES_6        T3_SSES_7        T3_SSES_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -4.32   Mean   : -2.94   Mean   : -3.05   Mean   : -3.31  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSES_9        T3_SSES_10       T3_SSES_11      T3_SSES_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  3.0   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.0   Median :  3.00  
    ##  Mean   : -2.82   Mean   : -4.33   Mean   : -2.3   Mean   : -2.58  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T3_SSES_13       T3_SSES_14       T3_SSES_15       T3_SSES_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.88   Mean   : -2.34   Mean   : -3.49   Mean   : -3.87  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSES_17       T3_SSES_18       T3_SSES_19       T3_SSES_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -2.89   Mean   : -3.27   Mean   : -4.37   Mean   : -3.31  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_DEQ_1         T3_DEQ_2         T3_DEQ_3         T3_DEQ_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -1.63   Mean   : -2.14   Mean   : -1.14   Mean   : -1.96  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  5.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##     T3_DEQ_5         T3_DEQ_6         T3_DEQ_7         T3_DEQ_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -2.64   Mean   : -2.21   Mean   : -2.71   Mean   : -1.58  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##     T3_DEQ_9         T3_SIAS_1        T3_SIAS_2        T3_SIAS_3     
    ##  Min.   :-99.000   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  4.000   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  5.000   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -0.571   Mean   : -3.44   Mean   : -3.74   Mean   : -4.17  
    ##  3rd Qu.:  6.000   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  7.000   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SIAS_4        T3_SIAS_5        T3_SIAS_6        T3_SIAS_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.68   Mean   : -2.46   Mean   : -5.01   Mean   : -4.38  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SIAS_8        T3_SIAS_9        T3_SIAS_10       T3_SIAS_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -4.59   Mean   : -4.13   Mean   : -4.59   Mean   : -3.16  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SIAS_12       T3_SIAS_13       T3_SIAS_14       T3_SIAS_15    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.69   Mean   : -4.39   Mean   : -3.91   Mean   : -3.71  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SIAS_16       T3_SIAS_17       T3_SIAS_18       T3_SIAS_19   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.0  
    ##  Mean   : -4.05   Mean   : -4.26   Mean   : -4.24   Mean   : -4.3  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T3_SIAS_20        T3_SPS_1         T3_SPS_2         T3_SPS_3     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.18   Mean   : -3.89   Mean   : -4.38   Mean   : -3.72  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_SPS_4         T3_SPS_5         T3_SPS_6         T3_SPS_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.77   Mean   : -4.99   Mean   : -3.95   Mean   : -4.46  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_SPS_8         T3_SPS_9        T3_SPS_10        T3_SPS_11     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.52   Mean   : -4.65   Mean   : -4.71   Mean   : -4.05  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SPS_12        T3_SPS_13       T3_SPS_14        T3_SPS_15     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.72   Mean   : -3.8   Mean   : -4.07   Mean   : -3.88  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SPS_16       T3_SPS_17        T3_SPS_18        T3_SPS_19     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -4.4   Mean   : -4.43   Mean   : -3.85   Mean   : -4.75  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SPS_20        T3_BFNE_1        T3_BFNE_2        T3_BFNE_3     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.04   Mean   : -3.15   Mean   : -3.39   Mean   : -3.27  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_BFNE_4       T3_BFNE_5        T3_BFNE_6        T3_BFNE_7     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.7   Mean   : -3.67   Mean   : -3.22   Mean   : -3.69  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_BFNE_8        T3_BFNE_9        T3_BFNE_10       T3_BFNE_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.26   Mean   : -3.05   Mean   : -3.37   Mean   : -2.98  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_BFNE_12      T3_FPES_1        T3_FPES_2       T3_FPES_3     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  3.00   Median :  3.0   Median :  2.00  
    ##  Mean   : -3.6   Mean   : -1.55   Mean   : -1.5   Mean   : -1.94  
    ##  3rd Qu.:  3.0   3rd Qu.:  5.00   3rd Qu.:  5.0   3rd Qu.:  5.00  
    ##  Max.   :  5.0   Max.   : 10.00   Max.   : 10.0   Max.   : 10.00  
    ##    T3_FPES_4        T3_FPES_5         T3_FPES_6         T3_FPES_7     
    ##  Min.   :-99.00   Min.   :-99.000   Min.   :-99.000   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  4.000   1st Qu.:  1.000   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  6.000   Median :  4.000   Median :  2.00  
    ##  Mean   : -2.17   Mean   :  0.954   Mean   : -0.982   Mean   : -2.18  
    ##  3rd Qu.:  4.00   3rd Qu.:  8.000   3rd Qu.:  6.000   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.000   Max.   : 10.000   Max.   : 10.00  
    ##    T3_FPES_8        T3_FPES_9        T3_FPES_10        T3_CET_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  3.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -2.23   Mean   : -2.28   Mean   : -1.92   Mean   : -2.27  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   :  5.00  
    ##     T3_CET_2         T3_CET_3         T3_CET_4         T3_CET_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  4.00   Median :  4.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -3.16   Mean   : -2.33   Mean   : -3.07   Mean   : -3.77  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_CET_6         T3_CET_7         T3_CET_8         T3_CET_9    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  2.0  
    ##  Mean   : -3.98   Mean   : -3.74   Mean   : -4.58   Mean   : -5.3  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T3_CET_10       T3_CET_11        T3_CET_12        T3_CET_13     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  2.0   Median :  1.00   Median :  3.00   Median :  3.00  
    ##  Mean   : -4.9   Mean   : -4.65   Mean   : -3.78   Mean   : -3.81  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_CET_14        T3_CET_15        T3_CET_16        T3_CET_17     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  4.00   Median :  2.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -3.58   Mean   : -4.37   Mean   : -4.45   Mean   : -2.86  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_CET_18        T3_CET_19        T3_CET_20        T3_CET_21     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.84   Mean   : -4.19   Mean   : -4.55   Mean   : -4.65  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_CET_22       T3_CET_23        T3_CET_24         T3_CIA_1     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -4.4   Mean   : -4.43   Mean   : -3.73   Mean   : -4.77  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  1.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  4.00  
    ##     T3_CIA_2         T3_CIA_3         T3_CIA_4         T3_CIA_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.67   Mean   : -4.82   Mean   : -4.85   Mean   : -4.86  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T3_CIA_6         T3_CIA_7         T3_CIA_8         T3_CIA_9    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -5.26   Mean   : -4.81   Mean   : -4.88   Mean   : -4.9  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.0  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.0  
    ##    T3_CIA_10        T3_CIA_11        T3_CIA_12        T3_CIA_13     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.79   Mean   : -4.88   Mean   : -5.24   Mean   : -4.87  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T3_CIA_14        T3_CIA_15        T3_CIA_16        T3_PANAS_1    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  4.00  
    ##  Mean   : -4.64   Mean   : -4.82   Mean   : -4.34   Mean   : -2.05  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  5.00  
    ##    T3_PANAS_2    T3_PANAS_3       T3_PANAS_4       T3_PANAS_5    
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -3   Mean   : -2.71   Mean   : -3.32   Mean   : -2.68  
    ##  3rd Qu.:  3   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_PANAS_6       T3_PANAS_7       T3_PANAS_8       T3_PANAS_9    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -4.06   Mean   : -3.71   Mean   : -4.02   Mean   : -2.35  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T3_PANAS_10      T3_PANAS_11      T3_PANAS_12      T3_PANAS_13    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -2.61   Mean   : -3.88   Mean   : -3.11   Mean   : -4.76  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T3_PANAS_14      T3_PANAS_15      T3_PANAS_16     T3_PANAS_17    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  3.0   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  2.00   Median :  4.0   Median :  3.00  
    ##  Mean   : -3.17   Mean   : -3.87   Mean   : -2.1   Mean   : -2.28  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##   T3_PANAS_18      T3_PANAS_19      T3_PANAS_20        T3_BDI_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.95   Mean   : -2.63   Mean   : -4.07   Mean   : -3.78  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  4.00  
    ##     T3_BDI_2         T3_BDI_3         T3_BDI_4         T3_BDI_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.21   Mean   : -4.22   Mean   : -4.32   Mean   : -4.27  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T3_BDI_6      T3_BDI_7         T3_BDI_8         T3_BDI_9     
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4   Mean   : -3.71   Mean   : -4.14   Mean   : -4.32  
    ##  3rd Qu.:  1   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  4   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T3_BDI_10        T3_BDI_11        T3_BDI_12        T3_BDI_13     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -5.18   Mean   : -4.26   Mean   : -4.25   Mean   : -4.76  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T3_BDI_14        T3_BDI_15        T3_BDI_16        T3_BDI_17     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.12   Mean   : -3.88   Mean   : -3.82   Mean   : -3.79  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  7.00   Max.   :  4.00   Max.   :  7.00  
    ##    T3_BDI_18        T3_BDI_19        T3_BDI_20       T3_FOF_AE_1    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.69   Mean   : -3.63   Mean   : -3.95   Mean   : -3.77  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  7.00  
    ##   T3_FOF_AE_2      T3_FOF_AE_3      T3_FOF_AE_4      T3_FOF_AE_5    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.58   Mean   : -3.65   Mean   : -3.62   Mean   : -4.01  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T3_FOF_AE_6      T3_FOF_AE_7      T3_FOF_AE_8      T3_FOF_FAB_1   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.55   Mean   : -3.71   Mean   : -3.64   Mean   : -4.03  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  3.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T3_FOF_FAB_2     T3_FOF_FAB_3     T3_FOF_FAB_4     T3_FOF_FAB_5
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  1.00   Median :  1.00   Median :  3.00   Median :  1  
    ##  Mean   : -4.01   Mean   : -4.24   Mean   : -2.59   Mean   : -4  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  5.00   3rd Qu.:  3  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7  
    ##   T3_FOF_FAB_6     T3_FOF_FAB_7     T3_FOF_FAB_8     T3_FOF_FC_1    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.81   Mean   : -4.47   Mean   : -3.46   Mean   : -2.79  
    ##  3rd Qu.:  5.00   3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T3_FOF_FC_2      T3_FOF_FC_3      T3_FOF_FC_4      T3_FOF_FC_5    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.63   Mean   : -3.45   Mean   : -2.85   Mean   : -4.92  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T3_FOF_FC_6      T3_FOF_FC_7      T3_FOF_FC_8      T3_FOF_FC_9   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -3.53   Mean   : -3.95   Mean   : -2.99   Mean   : -4.2  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.0  
    ##    T3_FOFS_1     T3_FOFS_2       T3_FOFS_3     T3_FOFS_4       T3_FOFS_5    
    ##  Min.   :-99   Min.   :-99.0   Min.   :-99   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:-99   1st Qu.:-99.0   1st Qu.:-99   1st Qu.:-99.0   1st Qu.:-99.0  
    ##  Median :  0   Median :-99.0   Median :-99   Median :-99.0   Median :  6.0  
    ##  Mean   :-29   Mean   :-51.3   Mean   :-63   Mean   :-68.1   Mean   :-15.7  
    ##  3rd Qu.: 14   3rd Qu.:  0.0   3rd Qu.:  0   3rd Qu.:  0.0   3rd Qu.: 30.0  
    ##  Max.   :100   Max.   :100.0   Max.   : 86   Max.   : 72.0   Max.   :100.0  
    ##    T3_FOFS_6       T3_FOFS_7       T3_FOFS_8       T3_FOFS_9    
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Median :-99.0  
    ##  Mean   :-48.3   Mean   :-53.5   Mean   :-54.6   Mean   :-63.7  
    ##  3rd Qu.:  4.0   3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0.0  
    ##  Max.   :100.0   Max.   :100.0   Max.   :100.0   Max.   :100.0  
    ##   T3_FOFS_10           T3_CES_1         T3_CES_2         T3_CES_3     
    ##  Length:217         Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  Class :character   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Mode  :character   Median :  4.00   Median :  2.00   Median :  3.00  
    ##                     Mean   : -1.67   Mean   : -3.02   Mean   : -2.85  
    ##                     3rd Qu.:  7.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##                     Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##     T3_CES_4         T3_CES_5         T3_CES_6         T3_CES_7  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2  
    ##  Mean   : -2.86   Mean   : -2.75   Mean   : -3.29   Mean   : -3  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10  
    ##     T3_CES_8         T3_CES_9        T3_CES_10        T3_CES_11     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -2.69   Mean   : -4.65   Mean   : -3.24   Mean   : -3.07  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##    T3_CES_12        T3_MINI_1        T3_MINI_2       T3_MINI_3    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.0   1st Qu.:  2.0  
    ##  Median :  2.00   Median :  2.00   Median :  4.0   Median :  3.0  
    ##  Mean   : -2.76   Mean   : -3.23   Mean   : -1.7   Mean   : -2.5  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  5.0   3rd Qu.:  4.0  
    ##  Max.   : 10.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.0  
    ##    T3_MINI_4        T3_MINI_5        T3_MINI_6        T3_MINI_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.29   Mean   : -2.99   Mean   : -3.07   Mean   : -4.02  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_8       T3_MINI_9        T3_MINI_10       T3_MINI_11    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.0   Median :  3.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -3.8   Mean   : -3.77   Mean   : -4.09   Mean   : -4.17  
    ##  3rd Qu.:  3.0   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_12       T3_MINI_13       T3_MINI_14       T3_MINI_15    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -3.41   Mean   : -2.64   Mean   : -3.95   Mean   : -3.64  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_16       T3_MINI_17       T3_MINI_18       T3_MINI_19    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -3.81   Mean   : -3.73   Mean   : -3.98   Mean   : -4.53  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  4.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_20       T3_MINI_21       T3_MINI_22      T3_MINI_23    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.0   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  3.00   Median :  4.0   Median :  4.00  
    ##  Mean   : -4.22   Mean   : -3.23   Mean   : -3.6   Mean   : -2.99  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T3_MINI_24       T3_MINI_25       T3_MINI_26       T3_MINI_27    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  4.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -4.45   Mean   : -4.03   Mean   : -3.32   Mean   : -4.64  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_28       T3_MINI_29       T3_MINI_30       T3_MINI_31    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  3.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -3.99   Mean   : -4.55   Mean   : -5.07   Mean   : -3.74  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_32       T3_MINI_33       T3_MINI_34       T3_MINI_35    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  4.00   Median :  1.00  
    ##  Mean   : -4.59   Mean   : -4.65   Mean   : -2.94   Mean   : -5.24  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_36       T3_MINI_37       T3_MINI_38       T3_MINI_39    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -3.63   Mean   : -4.71   Mean   : -3.43   Mean   : -4.64  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_40       T3_MINI_41       T3_MINI_42       T3_MINI_43    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  4.00   Median :  2.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -3.82   Mean   : -5.06   Mean   : -4.46   Mean   : -3.12  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_MINI_44      T3_MINI_45       T3_MINI_46       T3_MINI_47    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.0   1st Qu.:  4.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  4.0   Median :  4.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -2.8   Mean   : -2.58   Mean   : -2.72   Mean   : -4.52  
    ##  3rd Qu.:  4.0   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_IU_1          T3_IU_2          T3_IU_3          T3_IU_4   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  2  
    ##  Mean   : -3.19   Mean   : -3.73   Mean   : -3.65   Mean   : -4  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5  
    ##     T3_IU_5          T3_IU_6         T3_IU_7          T3_IU_8      
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  2.0   Median :  2.00   Median :  3.00  
    ##  Mean   : -3.83   Mean   : -3.6   Mean   : -4.39   Mean   : -3.72  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##     T3_IU_9          T3_IU_10         T3_IU_11         T3_IU_12     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -3.66   Mean   : -3.07   Mean   : -3.43   Mean   : -3.64  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_IU_13         T3_IU_14         T3_IU_15         T3_IU_16     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -4.24   Mean   : -4.05   Mean   : -4.46   Mean   : -3.75  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T3_IU_17         T3_IU_18         T3_IU_19         T3_IU_20  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2  
    ##  Mean   : -3.93   Mean   : -3.52   Mean   : -4.01   Mean   : -4  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5  
    ##     T3_IU_21         T3_IU_22         T3_IU_23      T3_IU_24     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  1   Median :  1.00  
    ##  Mean   : -3.45   Mean   : -3.91   Mean   : -4   Mean   : -5.63  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5   Max.   :  5.00  
    ##     T3_IU_25         T3_IU_26         T3_IU_27        T3_SSGS_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -3.78   Mean   : -3.35   Mean   : -3.86   Mean   : -2.18  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSGS_2        T3_SSGS_3        T3_SSGS_4        T3_SSGS_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -3.93   Mean   : -3.84   Mean   : -2.25   Mean   : -4.54  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSGS_6        T3_SSGS_7        T3_SSGS_8        T3_SSGS_9     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.96   Mean   : -2.68   Mean   : -4.41   Mean   : -4.35  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSGS_10       T3_SSGS_11       T3_SSGS_12       T3_SSGS_13    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  1.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -2.92   Mean   : -4.02   Mean   : -3.89   Mean   : -2.41  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SSGS_14       T3_SSGS_15       T3_SAFE_1        T3_SAFE_2    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  2.0  
    ##  Mean   : -3.94   Mean   : -3.81   Mean   : -3.41   Mean   : -3.3  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T3_SAFE_3        T3_SAFE_4       T3_SAFE_5        T3_SAFE_6     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.36   Mean   : -3.5   Mean   : -4.51   Mean   : -3.68  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAFE_7        T3_SAFE_8        T3_SAFE_9        T3_SAFE_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.82   Mean   : -3.62   Mean   : -3.83   Mean   : -4.27  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAFE_11       T3_SAFE_12       T3_SAFE_13      T3_SAFE_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  2.00  
    ##  Mean   : -4.15   Mean   : -3.86   Mean   : -3.9   Mean   : -3.32  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T3_SAFE_15       T3_SAFE_16      T3_SAFE_17       T3_SAFE_18    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  1.00   Median :  2.00  
    ##  Mean   : -3.76   Mean   : -4.3   Mean   : -4.43   Mean   : -3.32  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAFE_19       T3_SAFE_20       T3_SAFE_21       T3_SAFE_22    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -4.65   Mean   : -5.22   Mean   : -4.35   Mean   : -4.07  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAFE_23      T3_SAFE_24       T3_SAFE_25       T3_SAFE_26 
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  1.0   Median :  1.00   Median :  1.00   Median :  1  
    ##  Mean   : -3.9   Mean   : -3.99   Mean   : -3.63   Mean   : -4  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5  
    ##    T3_SAFE_27       T3_SAFE_28       T3_SAFE_29       T3_SAFE_30    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -3.73   Mean   : -3.87   Mean   : -4.72   Mean   : -3.83  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T3_SAFE_31       T3_SAFE_32       T3_SADS_1     T3_SADS_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1   Median :  2.00  
    ##  Mean   : -3.79   Mean   : -3.36   Mean   : -5   Mean   : -4.85  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  2   Max.   :  2.00  
    ##    T3_SADS_3        T3_SADS_4        T3_SADS_5        T3_SADS_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -4.94   Mean   : -5.68   Mean   : -4.71   Mean   : -5.17  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_7       T3_SADS_8        T3_SADS_9        T3_SADS_10    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -5.2   Mean   : -4.89   Mean   : -5.13   Mean   : -4.78  
    ##  3rd Qu.:  1.0   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.0   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_11       T3_SADS_12      T3_SADS_13       T3_SADS_14    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  2.00   Median :  1.00  
    ##  Mean   : -4.95   Mean   : -6.1   Mean   : -4.88   Mean   : -5.03  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.0   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_15       T3_SADS_16       T3_SADS_17       T3_SADS_18    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -5.07   Mean   : -4.84   Mean   : -5.08   Mean   : -4.99  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_19       T3_SADS_20       T3_SADS_21       T3_SADS_22    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -5.34   Mean   : -4.83   Mean   : -4.88   Mean   : -5.75  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_23       T3_SADS_24       T3_SADS_25       T3_SADS_26    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -4.81   Mean   : -4.86   Mean   : -5.22   Mean   : -4.76  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T3_SADS_27       T3_SADS_28       T4_EDEQ_1       T4_EDEQ_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -5.26   Mean   : -5.21   Mean   : -6.5   Mean   : -7.42  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  3.0   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  7.0   Max.   :  7.00  
    ##    T4_EDEQ_3        T4_EDEQ_4        T4_EDEQ_5        T4_EDEQ_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -6.39   Mean   : -6.48   Mean   : -7.41   Mean   : -7.31  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T4_EDEQ_7        T4_EDEQ_8        T4_EDEQ_9        T4_EDEQ_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -7.95   Mean   : -8.44   Mean   : -8.04   Mean   : -5.77  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  5.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T4_EDEQ_11       T4_EDEQ_12       T4_EDEQ_13       T4_EDEQ_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -7.19   Mean   : -7.45   Mean   : -6.92   Mean   : -6.12  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T4_EDEQ_15       T4_EDEQ16       T4_EDEQ_17A      T4_EDEQ_17B   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :-99.0  
    ##  Mean   : -6.67   Mean   : -7.44   Mean   : -7.62   Mean   :-59.5  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  0.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  2.00   Max.   :  7.0  
    ##   T4_EDEQ_17C     T4_EDEQ_18A     T4_EDEQ_18B         T4_EDEQ_19A    
    ##  Min.   :-99.0   Min.   :-99.00   Length:217         Min.   :-99.00  
    ##  1st Qu.:-99.0   1st Qu.:  1.00   Class :character   1st Qu.:  1.00  
    ##  Median :-99.0   Median :  1.00   Mode  :character   Median :  1.00  
    ##  Mean   :-60.6   Mean   : -8.12                      Mean   : -7.74  
    ##  3rd Qu.:  0.0   3rd Qu.:  1.00                      3rd Qu.:  1.00  
    ##  Max.   :  3.0   Max.   :  2.00                      Max.   :  2.00  
    ##   T4_EDEQ_19B     T4_EDEQ_20A     T4_EDEQ_20B   T4_EDEQ_21A      T4_EDEQ_21B 
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:-99.0   1st Qu.:  1.0   1st Qu.:-99   1st Qu.:  1.00   1st Qu.:-99  
    ##  Median :-99.0   Median :  1.0   Median :-99   Median :  1.00   Median :-99  
    ##  Mean   :-67.0   Mean   : -8.2   Mean   :-67   Mean   : -8.67   Mean   :-68  
    ##  3rd Qu.:  0.0   3rd Qu.:  1.0   3rd Qu.:  0   3rd Qu.:  1.00   3rd Qu.:  0  
    ##  Max.   :  5.5   Max.   :  2.0   Max.   :  4   Max.   :  2.00   Max.   :  1  
    ##   T4_EDEQ_22A      T4_EDEQ_22B     T4_EDEQ_23A       T4_EDEQ_24    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:-99.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :-99.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.64   Mean   :-61.3   Mean   : -7.92   Mean   : -7.57  
    ##  3rd Qu.:  1.00   3rd Qu.:  0.0   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  8.0   Max.   :  7.00   Max.   :  5.00  
    ##    T4_EDEQ_25       T4_EDEQ_26       T4_EDEQ_27       T4_EDEQ_28    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -6.45   Mean   : -6.47   Mean   : -7.12   Mean   : -7.05  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T4_EDEQ_29       T4_EDEQ_30       T4_EDEQ_31       T4_EDEQ_32    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.04   Mean   : -7.12   Mean   : -7.68   Mean   : -7.24  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##    T4_EDEQ_33      T4_EDEQ_34A    T4_EDEQ_34B          T4_Height    
    ##  Min.   :-99.00   Min.   :-99.0   Length:217         Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   Class :character   1st Qu.: 63.0  
    ##  Median :  1.00   Median :  2.0   Mode  :character   Median : 65.0  
    ##  Mean   : -7.13   Mean   : -7.5                      Mean   : 50.8  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.0                      3rd Qu.: 69.0  
    ##  Max.   :  7.00   Max.   :  2.0                      Max.   :165.0  
    ##    T4_Weight      T4_EDI_1         T4_EDI_2         T4_EDI_3     
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:115   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :133   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   :113   Mean   : -6.97   Mean   : -8.29   Mean   : -7.66  
    ##  3rd Qu.:150   3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :245   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##     T4_EDI_4         T4_EDI_5         T4_EDI_6         T4_EDI_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.68   Mean   : -8.31   Mean   : -8.05   Mean   : -7.18  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##     T4_EDI_8         T4_EDI_9        T4_EDI_10        T4_EDI_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  2.00   Median :  2.00   Median :  1.0  
    ##  Mean   : -8.27   Mean   : -6.82   Mean   : -7.73   Mean   : -8.7  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  2.0  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.0  
    ##    T4_EDI_12        T4_EDI_13        T4_EDI_14        T4_EDI_15     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -7.04   Mean   : -7.41   Mean   : -8.34   Mean   : -6.22  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  5.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T4_EDI_16        T4_EDI_17        T4_EDI_18        T4_EDI_19    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -8.01   Mean   : -8.83   Mean   : -9.09   Mean   : -7.9  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  3.0  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.0  
    ##    T4_EDI_20        T4_EDI_21        T4_EDI_22        T4_EDI_23     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -8.81   Mean   : -7.31   Mean   : -8.19   Mean   : -6.81  
    ##  3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  5.00  
    ##  Max.   :  6.00   Max.   :  6.00   Max.   :  6.00   Max.   :  6.00  
    ##    T4_SAAS_1        T4_SAAS_2        T4_SAAS_3        T4_SAAS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -6.59   Mean   : -7.65   Mean   : -8.47   Mean   : -7.66  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAAS_5        T4_SAAS_6        T4_SAAS_7        T4_SAAS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -7.73   Mean   : -7.11   Mean   : -6.96   Mean   : -7.88  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAAS_9        T4_SAAS_10       T4_SAAS_11       T4_SAAS_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -7.91   Mean   : -7.93   Mean   : -7.57   Mean   : -7.72  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAAS_13    T4_SAAS_14       T4_SAAS_15       T4_SAAS_16    
    ##  Min.   :-99   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -8   Mean   : -6.92   Mean   : -7.23   Mean   : -7.06  
    ##  3rd Qu.:  3   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_1        T4_FMPS_2        T4_FMPS_3        T4_FMPS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  4.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -6.58   Mean   : -6.65   Mean   : -7.87   Mean   : -7.73  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_5        T4_FMPS_6        T4_FMPS_7        T4_FMPS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  1.00   Median :  4.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -8.36   Mean   : -6.27   Mean   : -5.83   Mean   : -6.49  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_9        T4_FMPS_10      T4_FMPS_11       T4_FMPS_12    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.0   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.0   Median :  3.00   Median :  4.00  
    ##  Mean   : -7.03   Mean   : -6.5   Mean   : -7.46   Mean   : -7.23  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.0   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_13       T4_FMPS_14       T4_FMPS_15       T4_FMPS_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  4.00  
    ##  Mean   : -8.03   Mean   : -8.64   Mean   : -8.29   Mean   : -6.17  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_17       T4_FMPS_18       T4_FMPS_19       T4_FMPS_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -7.18   Mean   : -6.42   Mean   : -6.74   Mean   : -6.09  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_21       T4_FMPS_22       T4_FMPS_23       T4_FMPS_24   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.0  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  3.0  
    ##  Mean   : -8.36   Mean   : -8.22   Mean   : -7.67   Mean   : -7.4  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  4.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T4_FMPS_25      T4_FMPS_26       T4_FMPS_27       T4_FMPS_28    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -8.9   Mean   : -7.66   Mean   : -6.53   Mean   : -7.79  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_29       T4_FMPS_30       T4_FMPS_31       T4_FMPS_32    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  3.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -6.26   Mean   : -6.59   Mean   : -6.64   Mean   : -8.06  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FMPS_33       T4_FMPS_34       T4_FMPS_35       T4_SSES_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  2.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -7.68   Mean   : -7.93   Mean   : -8.73   Mean   : -6.36  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSES_2        T4_SSES_3        T4_SSES_4        T4_SSES_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.71   Mean   : -6.87   Mean   : -7.94   Mean   : -7.65  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSES_6        T4_SSES_7        T4_SSES_8        T4_SSES_9     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -6.76   Mean   : -7.37   Mean   : -7.68   Mean   : -6.65  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSES_10       T4_SSES_11       T4_SSES_12       T4_SSES_13   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  3.00   Median :  3.00   Median :  2.0  
    ##  Mean   : -7.21   Mean   : -6.07   Mean   : -6.27   Mean   : -6.7  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T4_SSES_14      T4_SSES_15       T4_SSES_16       T4_SSES_17    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -6.1   Mean   : -7.62   Mean   : -7.13   Mean   : -6.73  
    ##  3rd Qu.:  4.0   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSES_18       T4_SSES_19       T4_SSES_20        T4_DEQ_1     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -7.01   Mean   : -7.55   Mean   : -7.05   Mean   : -7.29  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  7.00  
    ##     T4_DEQ_2         T4_DEQ_3         T4_DEQ_4         T4_DEQ_5     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -7.44   Mean   : -6.99   Mean   : -7.56   Mean   : -8.18  
    ##  3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##     T4_DEQ_6         T4_DEQ_7        T4_DEQ_8         T4_DEQ_9     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  2.0   Median :  4.00   Median :  5.00  
    ##  Mean   : -7.79   Mean   : -7.9   Mean   : -7.18   Mean   : -6.32  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.0   3rd Qu.:  5.00   3rd Qu.:  6.00  
    ##  Max.   :  7.00   Max.   :  7.0   Max.   :  7.00   Max.   :  7.00  
    ##    T4_SIAS_1        T4_SIAS_2        T4_SIAS_3       T4_SIAS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.0   Median :  1.00  
    ##  Mean   : -8.64   Mean   : -8.87   Mean   : -8.4   Mean   : -8.82  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  3.0   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T4_SIAS_5        T4_SIAS_6        T4_SIAS_7        T4_SIAS_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.63   Mean   : -8.29   Mean   : -8.59   Mean   : -8.32  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SIAS_9        T4_SIAS_10       T4_SIAS_11       T4_SIAS_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -7.49   Mean   : -8.29   Mean   : -8.38   Mean   : -8.91  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SIAS_13       T4_SIAS_14       T4_SIAS_15       T4_SIAS_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -9.54   Mean   : -9.07   Mean   : -8.84   Mean   : -8.24  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SIAS_17       T4_SIAS_18       T4_SIAS_19       T4_SIAS_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -8.56   Mean   : -8.05   Mean   : -8.13   Mean   : -7.94  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_SPS_1         T4_SPS_2         T4_SPS_3         T4_SPS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.55   Mean   : -8.54   Mean   : -8.39   Mean   : -8.32  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_SPS_5        T4_SPS_6         T4_SPS_7         T4_SPS_8     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.1   Mean   : -7.65   Mean   : -8.61   Mean   : -8.62  
    ##  3rd Qu.:  2.0   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_SPS_9        T4_SPS_10        T4_SPS_11       T4_SPS_12     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -8.33   Mean   : -9.27   Mean   : -8.6   Mean   : -8.31  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.0   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T4_SPS_13        T4_SPS_14        T4_SPS_15        T4_SPS_16     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.41   Mean   : -9.12   Mean   : -8.49   Mean   : -8.08  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SPS_17        T4_SPS_18        T4_SPS_19        T4_SPS_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  2.0  
    ##  Mean   : -8.08   Mean   : -7.62   Mean   : -8.36   Mean   : -7.7  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  3.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T4_BFNE_1        T4_BFNE_2       T4_BFNE_3        T4_BFNE_4     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.0   Median :  2.00   Median :  2.00  
    ##  Mean   : -6.88   Mean   : -7.2   Mean   : -6.97   Mean   : -7.45  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T4_BFNE_5        T4_BFNE_6        T4_BFNE_7        T4_BFNE_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.43   Mean   : -8.35   Mean   : -7.01   Mean   : -7.38  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_BFNE_9       T4_BFNE_10       T4_BFNE_11       T4_BFNE_12    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -8.2   Mean   : -7.57   Mean   : -7.26   Mean   : -7.41  
    ##  3rd Qu.:  3.0   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_FPES_1        T4_FPES_2       T4_FPES_3        T4_FPES_4     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  3.0   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.05   Mean   : -7.2   Mean   : -7.54   Mean   : -7.71  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.0   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.0   Max.   : 10.00   Max.   : 10.00  
    ##    T4_FPES_5        T4_FPES_6        T4_FPES_7       T4_FPES_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  4.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  6.00   Median :  4.00   Median :  2.0   Median :  3.00  
    ##  Mean   : -4.72   Mean   : -6.26   Mean   : -7.4   Mean   : -6.75  
    ##  3rd Qu.:  8.00   3rd Qu.:  5.00   3rd Qu.:  4.0   3rd Qu.:  5.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.0   Max.   : 10.00  
    ##    T4_FPES_9        T4_FPES_10        T4_CET_1         T4_CET_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  3.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -6.99   Mean   : -7.16   Mean   : -6.54   Mean   : -7.46  
    ##  3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_CET_3         T4_CET_4         T4_CET_5         T4_CET_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -6.68   Mean   : -6.72   Mean   : -7.62   Mean   : -8.34  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_CET_7         T4_CET_8         T4_CET_9        T4_CET_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -7.57   Mean   : -8.23   Mean   : -8.16   Mean   : -8.17  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_CET_11        T4_CET_12        T4_CET_13        T4_CET_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  1.00   Median :  3.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -7.88   Mean   : -7.47   Mean   : -6.69   Mean   : -7.29  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_CET_15        T4_CET_16        T4_CET_17       T4_CET_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  4.0   Median :  2.00  
    ##  Mean   : -8.09   Mean   : -8.09   Mean   : -7.1   Mean   : -7.72  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  5.0   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T4_CET_19        T4_CET_20        T4_CET_21        T4_CET_22     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -8.51   Mean   : -8.18   Mean   : -8.46   Mean   : -8.07  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_CET_23        T4_CET_24         T4_CIA_1         T4_CIA_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  3.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.74   Mean   : -6.98   Mean   : -8.46   Mean   : -7.95  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  4.00  
    ##     T4_CIA_3         T4_CIA_4         T4_CIA_5         T4_CIA_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.47   Mean   : -8.99   Mean   : -9.43   Mean   : -8.47  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T4_CIA_7         T4_CIA_8         T4_CIA_9        T4_CIA_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.43   Mean   : -8.58   Mean   : -7.69   Mean   : -7.96  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T4_CIA_11        T4_CIA_12        T4_CIA_13        T4_CIA_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.59   Mean   : -7.96   Mean   : -8.05   Mean   : -8.33  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T4_CIA_15        T4_CIA_16        T4_PANAS_1       T4_PANAS_2    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  1.00   Median :  1.00   Median :  4.00   Median :  2.00  
    ##  Mean   : -8.02   Mean   : -7.63   Mean   : -7.23   Mean   : -8.14  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_PANAS_3       T4_PANAS_4       T4_PANAS_5       T4_PANAS_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -7.92   Mean   : -8.42   Mean   : -7.78   Mean   : -8.42  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_PANAS_7       T4_PANAS_8       T4_PANAS_9      T4_PANAS_10    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  2.00  
    ##  Median :  1.00   Median :  1.00   Median :  3.00   Median :  3.00  
    ##  Mean   : -8.28   Mean   : -8.59   Mean   : -7.03   Mean   : -7.76  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T4_PANAS_11      T4_PANAS_12      T4_PANAS_13      T4_PANAS_14    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  3.00   Median :  1.00   Median :  3.00  
    ##  Mean   : -8.62   Mean   : -7.85   Mean   : -8.87   Mean   : -8.22  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T4_PANAS_15      T4_PANAS_16      T4_PANAS_17      T4_PANAS_18    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -8.11   Mean   : -6.83   Mean   : -7.04   Mean   : -8.18  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##   T4_PANAS_19      T4_PANAS_20        T4_BDI_1         T4_BDI_2     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.29   Mean   : -8.28   Mean   : -7.99   Mean   : -7.92  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  4.00  
    ##     T4_BDI_3         T4_BDI_4         T4_BDI_5         T4_BDI_6     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.89   Mean   : -8.91   Mean   : -7.99   Mean   : -8.59  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##     T4_BDI_7         T4_BDI_8         T4_BDI_9     T4_BDI_10     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1   Median :  1.00  
    ##  Mean   : -8.36   Mean   : -7.86   Mean   : -8   Mean   : -7.97  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  1   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4   Max.   :  4.00  
    ##    T4_BDI_11        T4_BDI_12        T4_BDI_13        T4_BDI_14     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.98   Mean   : -7.98   Mean   : -8.46   Mean   : -8.31  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  4.00  
    ##    T4_BDI_15        T4_BDI_16        T4_BDI_17        T4_BDI_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -7.75   Mean   : -7.97   Mean   : -7.67   Mean   : -9.28  
    ##  3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  4.00   Max.   :  7.00   Max.   :  4.00  
    ##    T4_BDI_19        T4_BDI_20      T4_FOF_AE_1      T4_FOF_AE_2    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.73   Mean   : -8.6   Mean   : -9.31   Mean   : -9.22  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.0   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  4.00   Max.   :  4.0   Max.   :  7.00   Max.   :  7.00  
    ##   T4_FOF_AE_3      T4_FOF_AE_4      T4_FOF_AE_5      T4_FOF_AE_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.15   Mean   : -9.17   Mean   : -9.11   Mean   : -9.26  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.00  
    ##   T4_FOF_AE_7      T4_FOF_AE_8      T4_FOF_FAB_1     T4_FOF_FAB_2  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.0  
    ##  Mean   : -9.28   Mean   : -9.23   Mean   : -8.77   Mean   : -9.2  
    ##  3rd Qu.:  1.00   3rd Qu.:  1.00   3rd Qu.:  2.00   3rd Qu.:  2.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  7.0  
    ##   T4_FOF_FAB_3     T4_FOF_FAB_4  T4_FOF_FAB_5     T4_FOF_FAB_6   
    ##  Min.   :-99.00   Min.   :-99   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2   Median :  1.00   Median :  2.00  
    ##  Mean   : -9.39   Mean   : -8   Mean   : -9.35   Mean   : -8.16  
    ##  3rd Qu.:  2.00   3rd Qu.:  5   3rd Qu.:  2.00   3rd Qu.:  4.00  
    ##  Max.   :  7.00   Max.   :  7   Max.   :  7.00   Max.   :  7.00  
    ##   T4_FOF_FAB_7     T4_FOF_FAB_8     T4_FOF_FC_1      T4_FOF_FC_2    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.17   Mean   : -8.19   Mean   : -8.89   Mean   : -9.23  
    ##  3rd Qu.:  1.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :  6.00  
    ##   T4_FOF_FC_3      T4_FOF_FC_4      T4_FOF_FC_5     T4_FOF_FC_6    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -9.12   Mean   : -9.53   Mean   :-10.9   Mean   : -9.56  
    ##  3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  1.0   3rd Qu.:  1.00  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.0   Max.   :  7.00  
    ##   T4_FOF_FC_7      T4_FOF_FC_8      T4_FOF_FC_9       T4_FOFS_1    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:-99.0  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  0.0  
    ##  Mean   : -9.56   Mean   : -9.06   Mean   : -9.77   Mean   :-37.9  
    ##  3rd Qu.:  1.00   3rd Qu.:  3.00   3rd Qu.:  1.00   3rd Qu.: 13.0  
    ##  Max.   :  7.00   Max.   :  7.00   Max.   :  7.00   Max.   :100.0  
    ##    T4_FOFS_2       T4_FOFS_3       T4_FOFS_4     T4_FOFS_5     T4_FOFS_6  
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99   Min.   :-99   Min.   :-99  
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99   1st Qu.:-99   1st Qu.:-99  
    ##  Median :-99.0   Median :-99.0   Median :-99   Median :  4   Median :-99  
    ##  Mean   :-55.1   Mean   :-64.1   Mean   :-70   Mean   :-20   Mean   :-55  
    ##  3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0   3rd Qu.: 35   3rd Qu.:  0  
    ##  Max.   : 96.0   Max.   : 33.0   Max.   : 38   Max.   :100   Max.   : 86  
    ##    T4_FOFS_7       T4_FOFS_8       T4_FOFS_9      T4_FOFS_10       
    ##  Min.   :-99.0   Min.   :-99.0   Min.   :-99.0   Length:217        
    ##  1st Qu.:-99.0   1st Qu.:-99.0   1st Qu.:-99.0   Class :character  
    ##  Median :-99.0   Median :-99.0   Median :-99.0   Mode  :character  
    ##  Mean   :-57.2   Mean   :-57.7   Mean   :-64.9                     
    ##  3rd Qu.:  0.0   3rd Qu.:  0.0   3rd Qu.:  0.0                     
    ##  Max.   : 66.0   Max.   : 80.0   Max.   : 96.0                     
    ##     T4_CES_1         T4_CES_2         T4_CES_3         T4_CES_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -6.75   Mean   : -7.83   Mean   : -7.58   Mean   : -8.52  
    ##  3rd Qu.:  6.00   3rd Qu.:  4.00   3rd Qu.:  5.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##     T4_CES_5         T4_CES_6         T4_CES_7         T4_CES_8     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -7.56   Mean   : -8.01   Mean   : -7.83   Mean   : -7.51  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  5.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##     T4_CES_9        T4_CES_10        T4_CES_11        T4_CES_12     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  2.00   Median :  2.00  
    ##  Mean   : -8.06   Mean   : -7.53   Mean   : -7.37   Mean   : -7.18  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   : 10.00   Max.   : 10.00   Max.   : 10.00   Max.   : 10.00  
    ##    T4_MINI_1        T4_MINI_2        T4_MINI_3        T4_MINI_4  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  1  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  2  
    ##  Mean   : -8.35   Mean   : -6.83   Mean   : -7.52   Mean   : -9  
    ##  3rd Qu.:  3.00   3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  3  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5  
    ##    T4_MINI_5        T4_MINI_6       T4_MINI_7        T4_MINI_8     
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  2.0   Median :  2.00   Median :  2.00  
    ##  Mean   : -9.07   Mean   : -8.7   Mean   : -8.71   Mean   : -8.42  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.0   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_9        T4_MINI_10       T4_MINI_11       T4_MINI_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  2.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -7.63   Mean   : -8.87   Mean   : -8.37   Mean   : -7.63  
    ##  3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_13       T4_MINI_14       T4_MINI_15       T4_MINI_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  2.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -6.95   Mean   : -8.23   Mean   : -8.34   Mean   : -8.54  
    ##  3rd Qu.:  5.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_17       T4_MINI_18       T4_MINI_19       T4_MINI_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  3.00   Median :  2.00  
    ##  Mean   : -8.74   Mean   : -9.58   Mean   : -7.33   Mean   : -8.04  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_21       T4_MINI_22       T4_MINI_23       T4_MINI_24    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  2.00   1st Qu.:  3.00   1st Qu.:  2.00  
    ##  Median :  3.00   Median :  4.00   Median :  4.00   Median :  3.00  
    ##  Mean   : -7.06   Mean   : -7.29   Mean   : -6.67   Mean   : -7.18  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_25       T4_MINI_26       T4_MINI_27       T4_MINI_28   
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  2.0  
    ##  Median :  4.00   Median :  4.00   Median :  2.00   Median :  3.0  
    ##  Mean   : -7.72   Mean   : -7.19   Mean   : -8.34   Mean   : -7.7  
    ##  3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  3.00   3rd Qu.:  4.0  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.0  
    ##    T4_MINI_29       T4_MINI_30      T4_MINI_31       T4_MINI_32    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.0   1st Qu.:  3.00   1st Qu.:  1.00  
    ##  Median :  3.00   Median :  1.0   Median :  4.00   Median :  2.00  
    ##  Mean   : -7.88   Mean   : -8.8   Mean   : -6.58   Mean   : -7.84  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.0   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_33       T4_MINI_34       T4_MINI_35    T4_MINI_36    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  1   1st Qu.:  2.00  
    ##  Median :  2.00   Median :  4.00   Median :  1   Median :  3.00  
    ##  Mean   : -8.75   Mean   : -7.72   Mean   : -9   Mean   : -7.94  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5   Max.   :  5.00  
    ##    T4_MINI_37       T4_MINI_38       T4_MINI_39       T4_MINI_40    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  2.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  4.00   Median :  3.00   Median :  4.00  
    ##  Mean   : -8.83   Mean   : -7.71   Mean   : -8.29   Mean   : -7.58  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_41       T4_MINI_42       T4_MINI_43       T4_MINI_44    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  3.00  
    ##  Median :  2.00   Median :  3.00   Median :  4.00   Median :  4.00  
    ##  Mean   : -8.29   Mean   : -7.36   Mean   : -6.85   Mean   : -6.57  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  4.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_MINI_45       T4_MINI_46       T4_MINI_47        T4_IU_1      
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  3.00   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  4.00   Median :  4.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -6.89   Mean   : -6.54   Mean   : -8.29   Mean   : -9.35  
    ##  3rd Qu.:  5.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_IU_2          T4_IU_3         T4_IU_4          T4_IU_5   
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1  
    ##  Median :  1.00   Median :  1.0   Median :  1.00   Median :  2  
    ##  Mean   : -9.74   Mean   :-10.2   Mean   : -9.62   Mean   : -9  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  3.00   3rd Qu.:  3  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5  
    ##     T4_IU_6         T4_IU_7       T4_IU_8       T4_IU_9         T4_IU_10     
    ##  Min.   :-99.0   Min.   :-99   Min.   :-99   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1   1st Qu.:  1   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  1   Median :  3   Median :  1.0   Median :  2.00  
    ##  Mean   : -8.8   Mean   :-10   Mean   : -9   Mean   :-10.2   Mean   : -9.33  
    ##  3rd Qu.:  3.0   3rd Qu.:  3   3rd Qu.:  3   3rd Qu.:  2.0   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5   Max.   :  5   Max.   :  5.0   Max.   :  5.00  
    ##     T4_IU_11         T4_IU_12        T4_IU_13         T4_IU_14    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.0  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.0  
    ##  Median :  2.00   Median :  1.0   Median :  1.00   Median :  1.0  
    ##  Mean   : -9.53   Mean   : -9.7   Mean   : -9.38   Mean   : -9.2  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.0  
    ##  Max.   :  5.00   Max.   :  5.0   Max.   :  5.00   Max.   :  5.0  
    ##     T4_IU_15         T4_IU_16         T4_IU_17         T4_IU_18     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -9.18   Mean   : -8.88   Mean   : -9.61   Mean   : -9.29  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_IU_19        T4_IU_20      T4_IU_21         T4_IU_22     
    ##  Min.   :-99.0   Min.   :-99   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  2   Median :  3.00   Median :  2.00  
    ##  Mean   :-10.1   Mean   :-10   Mean   : -8.54   Mean   : -9.07  
    ##  3rd Qu.:  2.0   3rd Qu.:  3   3rd Qu.:  4.00   3rd Qu.:  3.00  
    ##  Max.   :  5.0   Max.   :  5   Max.   :  5.00   Max.   :  5.00  
    ##     T4_IU_23         T4_IU_24         T4_IU_25         T4_IU_26     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -9.09   Mean   : -9.84   Mean   : -9.81   Mean   : -9.41  
    ##  3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##     T4_IU_27        T4_SSGS_1        T4_SSGS_2       T4_SSGS_3  
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.0   1st Qu.:  1  
    ##  Median :  2.00   Median :  3.00   Median :  1.0   Median :  1  
    ##  Mean   : -9.43   Mean   : -7.88   Mean   : -9.1   Mean   : -9  
    ##  3rd Qu.:  3.00   3rd Qu.:  4.00   3rd Qu.:  2.0   3rd Qu.:  2  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.0   Max.   :  5  
    ##    T4_SSGS_4        T4_SSGS_5        T4_SSGS_6        T4_SSGS_7     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  3.00  
    ##  Median :  3.00   Median :  1.00   Median :  2.00   Median :  3.00  
    ##  Mean   : -7.84   Mean   : -8.82   Mean   : -8.23   Mean   : -6.88  
    ##  3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  4.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSGS_8        T4_SSGS_9        T4_SSGS_10       T4_SSGS_11    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  3.00   Median :  1.00  
    ##  Mean   : -9.13   Mean   : -8.63   Mean   : -7.69   Mean   : -9.15  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SSGS_12       T4_SSGS_13       T4_SSGS_14       T4_SSGS_15    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  3.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.08   Mean   : -7.58   Mean   : -9.07   Mean   : -8.99  
    ##  3rd Qu.:  2.00   3rd Qu.:  4.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAFE_1        T4_SAFE_2        T4_SAFE_3        T4_SAFE_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  2.00   Median :  2.00   Median :  1.00  
    ##  Mean   : -8.18   Mean   : -8.12   Mean   : -8.59   Mean   : -9.25  
    ##  3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  3.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAFE_5       T4_SAFE_6        T4_SAFE_7       T4_SAFE_8     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  1.00   Median :  1.0   Median :  1.00  
    ##  Mean   : -9.6   Mean   : -8.46   Mean   : -8.5   Mean   : -8.36  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  2.00  
    ##  Max.   :  5.0   Max.   :  5.00   Max.   :  5.0   Max.   :  5.00  
    ##    T4_SAFE_9        T4_SAFE_10       T4_SAFE_11       T4_SAFE_12    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.54   Mean   : -8.47   Mean   : -9.23   Mean   : -8.56  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  5.00  
    ##    T4_SAFE_13       T4_SAFE_14       T4_SAFE_15       T4_SAFE_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.58   Mean   : -7.66   Mean   : -8.99   Mean   : -8.57  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  5.00   Max.   :  4.00   Max.   :  5.00  
    ##    T4_SAFE_17       T4_SAFE_18       T4_SAFE_19       T4_SAFE_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.13   Mean   : -8.16   Mean   : -8.47   Mean   : -8.08  
    ##  3rd Qu.:  2.00   3rd Qu.:  3.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SAFE_21       T4_SAFE_22       T4_SAFE_23       T4_SAFE_24    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.58   Mean   : -9.18   Mean   : -8.65   Mean   : -8.63  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  4.00   Max.   :  4.00   Max.   :  4.00   Max.   :  5.00  
    ##    T4_SAFE_25       T4_SAFE_26       T4_SAFE_27       T4_SAFE_28    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -8.37   Mean   : -8.63   Mean   : -8.43   Mean   : -8.01  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  4.00   Max.   :  5.00  
    ##    T4_SAFE_29       T4_SAFE_30       T4_SAFE_31       T4_SAFE_32    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   : -8.96   Mean   : -9.07   Mean   : -8.43   Mean   : -7.56  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  3.00  
    ##  Max.   :  5.00   Max.   :  5.00   Max.   :  5.00   Max.   :  5.00  
    ##    T4_SADS_1        T4_SADS_2        T4_SADS_3        T4_SADS_4     
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.19   Mean   : -9.97   Mean   : -9.62   Mean   : -9.82  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T4_SADS_5       T4_SADS_6        T4_SADS_7        T4_SADS_8     
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.0   Median :  1.00   Median :  1.00   Median :  2.00  
    ##  Mean   :-11.2   Mean   : -9.77   Mean   : -9.38   Mean   : -9.98  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  2.00  
    ##  Max.   :  2.0   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T4_SADS_9       T4_SADS_10       T4_SADS_11       T4_SADS_12    
    ##  Min.   :-99.0   Min.   :-99.00   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.0   Median :  2.00   Median :  1.00   Median :  1.00  
    ##  Mean   :-10.2   Mean   : -9.86   Mean   : -9.14   Mean   : -9.83  
    ##  3rd Qu.:  2.0   3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.00  
    ##  Max.   :  2.0   Max.   :  2.00   Max.   :  2.00   Max.   :  2.00  
    ##    T4_SADS_13       T4_SADS_14       T4_SADS_15      T4_SADS_16    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.00   Median :  1.0   Median :  2.00  
    ##  Mean   : -9.49   Mean   : -9.69   Mean   :-10.2   Mean   : -9.97  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  2.0   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.0   Max.   :  2.00  
    ##    T4_SADS_17       T4_SADS_18       T4_SADS_19      T4_SADS_20    
    ##  Min.   :-99.00   Min.   :-99.00   Min.   :-99.0   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  1.00   Median :  1.0   Median :  2.00  
    ##  Mean   : -9.77   Mean   : -9.61   Mean   :-10.4   Mean   : -9.47  
    ##  3rd Qu.:  2.00   3rd Qu.:  2.00   3rd Qu.:  1.0   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.00   Max.   :  2.0   Max.   :  2.00  
    ##    T4_SADS_21       T4_SADS_22      T4_SADS_23       T4_SADS_24    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  2.00   Median :  1.0   Median :  2.00   Median :  2.00  
    ##  Mean   : -9.06   Mean   :-10.8   Mean   : -9.04   Mean   : -9.48  
    ##  3rd Qu.:  2.00   3rd Qu.:  1.0   3rd Qu.:  2.00   3rd Qu.:  2.00  
    ##  Max.   :  2.00   Max.   :  2.0   Max.   :  2.00   Max.   :  2.00  
    ##    T4_SADS_25       T4_SADS_26      T4_SADS_27       T4_SADS_28    
    ##  Min.   :-99.00   Min.   :-99.0   Min.   :-99.00   Min.   :-99.00  
    ##  1st Qu.:  1.00   1st Qu.:  1.0   1st Qu.:  1.00   1st Qu.:  1.00  
    ##  Median :  1.00   Median :  2.0   Median :  1.00   Median :  1.00  
    ##  Mean   : -9.39   Mean   : -9.4   Mean   : -9.88   Mean   : -9.84  
    ##  3rd Qu.:  1.00   3rd Qu.:  2.0   3rd Qu.:  1.00   3rd Qu.:  1.00  
    ##  Max.   :  2.00   Max.   :  2.0   Max.   :  2.00   Max.   :  2.00
