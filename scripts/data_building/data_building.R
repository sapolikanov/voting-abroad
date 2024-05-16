## Building data for the analysis
## Dependencies: raw_data_prep.R, data_raw folder

# Source data from raw_data_prep script
source(here::here("scripts", "data_building", "raw_data_prep.R"))
source(here::here("utilities","check_packages.R"))
  
# Data
  
  ## exit poll 18-03-24 11:21 CET
  # ep <- read_excel("data/exitpoll_18-03-24_11-21CET.xlsx")
  ep <- read_excel(here("data", "data_raw", "exitpoll_results", 
                        "exitpoll_18-03-24_20-29CET.xlsx"), sheet = 2)
  
  ## yandex search data
  yandex_weekly <- read_csv(here("data", "data_raw", "migration", 
                                 "df_countries_weekly.csv"))
  
  ## Migration
  
    ### fsb
    fsb_mig <- read_excel(here("data", "data_raw", "country_level", 
                               "off_migration.xlsx"))
  
    ### Bilateral migration dataset
    bilat_migration <- read_csv(here("data", "data_raw", "migration", 
                                     "bilat_mig_sex.csv"))
    
    ### International migrant stock
      
      #### Main
      intstock_migration <- read_excel(
        here("data", "data_raw", "migration", 
             "undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx"),
        sheet = 2, skip = 9, guess_max = 37051
        )
      
  ## Religion
  wrp <- read_excel(here("data", "data_raw", "country_level", "wrp.xlsx"))

  ## Qog - econ development and democracy
  qog <- read_csv(
    here("data", "data_raw", "country_level", "qog_std_ts_jan24.csv"), 
    guess_max = 15564
    )
  
  ## ATOP
  atop <- read_csv(here("data", "data_raw", "country_level", "atop5_1dy.csv"),
                   guess_max = 136648)
  
  ## Trade
  trade <- read_xlsx(here("data", "data_raw", "country_level", "trade.xlsx"),
                     sheet = 2)
  
  ## Handcoded
  hc <- read_xlsx(here("data", "data_raw", "country_level", "handcoded.xlsx"),
                  na = "NA") |> 
    mutate(countrycode_n = as.numeric(countrycode_n)) |> 
    drop_na(countrycode_c, countrycode_n) |> 
    distinct(countrycode_c, countrycode_n, .keep_all = T)
  
  ## Distance
  geodist <- read_xls(here("data", "data_raw", "country_level", 
                           "dist_cepii.xls"), na = ".")
  
  ## official election results
  
    ### 98% of processed ballots
    # off_res <- read_tsv("data/results-uik-20240318T1503UTC.tsv")
  
    ### Final data dumo (100 processed ballots)
    off_res <- read_tsv(here("data", "data_raw", "official_results",
                             "results-uik-20240320T0352UTC.tsv"))
    
  ## Supplementary data
    
    ### Voting station to country dictionary
    uik_dict <- read_excel(here("data", "data_raw", "official_results", 
                                "uik_dictionary.xlsx"), sheet = 2)
    countrynameru_dict <- read_excel(here("data", "data_raw", 
                                          "official_results", 
                                          "uik_dictionary.xlsx"), 
                                     sheet = 3)
  
  
#-------------------------------------------------------------------------------
# Data cleaning: exit poll

  ## Data cleaning: new ep
  ep_cn <- ep |> 
  fill(country, .direction = "down") |> 
    separate(col = "country", into = c("countryname_ru", "countryname_en"),
             sep = " / ") |> 
    separate(col = "city", into = c("city_ru", "city_en"), 
             sep = " / ") |> 
    mutate(countryname_en = if_else(countryname_en == "Czech", 
                                    "Czechia", countryname_en),
           countrycode_n = countrycode(countryname_en, 
                                       origin = "country.name", 
                                       destination = "iso3n"),
           countrycode_c = countrycode(countrycode_n, 
                                       origin = "iso3n", 
                                       destination = "iso3c"),
           voting_station = as.character(voting_station),
           city_en = if_else(str_detect(city_ru, "№") == T, 
                             paste(city_en, str_sub(city_ru, -1, 
                                                    nchar(city_ru))),
                             city_en),
           voters_counted = as.numeric(
             str_replace(str_replace(voters_counted, "~", ""), ">", "")),
           ep = 1)
  
#------------------------------------------------------------------------------#
# Data cleaning: exit poll raw
  
  ## Aggregate to voting station level
  ep.raw_agg <- ep_raw_clean |> 
    group_by(voting_station, countryname_en, countryname_ru, countrycode_c,
             countrycode_n, city_ru, city_en) |> 
    summarise(vote.raw_putin = sum(vote == "Putin")/n(),
              vote.raw_davankov = sum(vote == "Davankov")/n(),
              vote.raw_spoiled = sum(vote == "Spoiled ballot")/n(),
              share.raw_female = sum(sex == "Female", na.rm = T)/n(),
              share.raw_othergender = sum(sex == "Other", na.rm = T)/n(),
              share.raw_age1824 = sum(age_bin == "18-24", na.rm = T)/n(),
              share.raw_age2444 = sum(age_bin == "24-44", na.rm = T)/n(),
              share.raw_age4564 = sum(age_bin == "45-64", na.rm = T)/n(),
              share.raw_age65 = sum(age_bin == "65+", na.rm = T)/n(),
              share.raw_tourist = 
                sum(out_of_Russia_time == "Tourist (lives in Russia)", 
                    na.rm = T)/n(),
              share.raw_afterfeb = 
                sum(out_of_Russia_time %in% c("6 month - 2 years", 
                                              "< 6 months", 
                                              "< 2 years"), na.rm = T)/n(),
              share.raw_before2014 =
                sum(out_of_Russia_time == "> 10 years", na.rm = T)/n(),
              share.raw_after2014 = 
                sum(out_of_Russia_time == "> 5 years", na.rm = T)/n(),
              share.raw_2019 = 
                sum(out_of_Russia_time == "2 - 5 years", na.rm = T)/n(),
              # ref 2014
              share.raw_afterfebref = 
                sum(out_of_Russia_time %in% c("6 month - 2 years", 
                                              "< 6 months", 
                                              "< 2 years"), na.rm = T)
              /sum(out_of_Russia_time == "> 10 years", na.rm = T),
              share.raw_after2014ref = 
                sum(out_of_Russia_time == "> 5 years", na.rm = T)
              /sum(out_of_Russia_time == "> 10 years", na.rm = T),
              share.raw_2019ref = 
                sum(out_of_Russia_time == "2 - 5 years", na.rm = T)
              /sum(out_of_Russia_time == "> 10 years", na.rm = T),
              share.raw_timemore4h = 
                sum(time_to_vs == "> 4 hours (staying for the night)", 
                    na.rm = T)/n(),
              share.raw_local = 
                sum(time_to_vs %in% c("<30 minutes", "30 minutes - 1 hour"), 
                    na.rm = T)/n(),
              share.raw_trustanyyes = 
                sum(result_trust %in% c("Definitely yes", "Probably yes"), 
                    na.rm = T)/n(),
              share.raw_trustanyno = 
                sum(result_trust %in% c("Definitely no", "Probably no"), 
                    na.rm = T)/n()) |> 
    ungroup() |> 
    select(-countryname_ru, -countryname_en, -city_ru, -city_en)
  
#------------------------------------------------------------------------------#
# Data cleaning: uik dictionary 
  
  ## Convert countrynames in Russian provided by the Russian Ministry of Foreign  
  ## Affairs to match with those int the international code dictionary
  uik_dict_clean <- uik_dict |> 
    mutate(uik = as.character(uik), # For merges
           country_compatible = case_when(
             country == "Южно-Африканская Республика и Лесото" ~ "Южная Африка",
             country == "Шри-Ланка и Мальдивы" ~ "Шри-Ланка",
             country == "Центральноафриканская Республика (ЦАР)"
             ~ "Центральноафриканская Республика",
             country == "Филиппины и Палау" ~ "Филиппины",
             country == "Федеративная Республика Германия" ~ "Германия",
             country == "Уганда и Южный Судан" ~ "Уганда",
             country == "Танзания" ~ "Объединенная Республика Танзания",
             country == "Сирия" ~ "Сирийская Арабская Республика",
             country == "Сенегал и Гамбия" ~ "Сенегал",
             country == "Республика Конго" ~ "Конго",
             country == "Палестина" ~ "Государство Палестина",
             country == "Новая Зеландия, Тонга и Самоа" ~ "Новая Зеландия",
             country == "Никарагуа, Гондурас и Сальвадор" ~ "Никарагуа",
             country == "Нидерланды" ~ "Нидерланды (Королевство)",
             country == "Молдавия" ~ "Республика Молдова",
             country == "Мозамбик и Эсватини" ~ "Мозамбик",
             country == "Мексика и Белиз" ~ "Мексика",
             country == "Мали и Нигер" ~ "Мали",
             country == "Мадагаскар и Коморские Острова" ~ "Мадагаскар",
             country == "Лаос" ~ "Лаосская Народно-Демократическая Республика",
             country == "Кот-д’Ивуар и" ~ "Кот-д'Ивуар",
             country == "КНР" ~ "Китай",
             country == "Киргизия" ~ "Кыргызстан",
             country == "Камерун и Экваториальная Гвинея" ~ "Камерун",
             country == "Иран" ~ "Иран (Исламская Республика)",
             country == "Индонезия и Восточный Тимор" ~ "Индонезия",
             country == "Зимбабве и Малави" ~ "Зимбабве",
             country == "Джибути и Сомали" ~ "Джибути",
             country == "Гвинея и Сьерра-Леоне" ~ "Гвинея",
             country == "Гана и Либерия" ~ "Гана",
             country == "Венесуэла, Доминиканская Республика и Гаити"
             ~ "Венесуэла (Боливарианская Республика)",
             country == "Великобритания"
             ~ "Соединенное Королевство Великобритании и Северной Ирландии",
             country == "Бруней" ~ "Бруней-Даруссалам",
             country == "Бразилия и Суринам" ~ "Бразилия",
             country == "Боливия" ~ "Боливия (Многонациональное Государство)",
             country == "Бенин и Того" ~ "Бенин",
             country == "Белоруссия" ~ "Беларусь",
             country == "Ангола и Сан-Томе и Принсипи" ~ "Ангола", 
             country == "КНДР" ~ "Корейская Народно-Демократическая Республика", 
             .default = country)) |> 
    left_join(countrynameru_dict, 
              by = join_by("country_compatible" == "Country or Area"))
  
#------------------------------------------------------------------------------#
# Data cleaning: official results
  
  ## Select international voting stations
  off_res_select <- off_res |> 
    separate(col = "uik", into = c(NA, "uik_num"), sep = "№") |> 
    filter(nchar(uik_num) == 4 & substring(uik_num, 1, 1) == "8") |> 
    left_join(select(ep_cn, countryname_en, countryname_ru, countrycode_c,
                     countrycode_n, city_ru, city_en, voting_station), 
              by = join_by("uik_num" == "voting_station")) |> 
    left_join(uik_dict_clean, by = join_by("uik_num" == "uik")) |> 
    mutate(countryname_ru = if_else(is.na(countryname_ru), 
                                    country_compatible, countryname_ru),
           city_en = if_else(is.na(city_en), settlement, city_en),
           countrycode_c = if_else(
             tik == "Город Байконур (Республика Казахстан)", 
             "KAZ", countrycode_c),
           countryname_ru = if_else(
             tik == "Город Байконур (Республика Казахстан)", 
             "Казахстан", countryname_ru),
           city_en = if_else(tik == "Город Байконур (Республика Казахстан)", 
                             "Baikonur", city_en),
           city_ru = if_else(tik == "Город Байконур (Республика Казахстан)", 
                             "Байконур", city_ru),
           countrycode_c = if_else(is.na(countrycode_c), `ISO-alpha3 code`, 
                                   countrycode_c),
           countrycode_n = if_else(is.na(countrycode_n), 
                                   countrycode(sourcevar = countrycode_c, 
                                               origin = "iso3c",
                                               destination = "iso3n"),
                                   countrycode_n),
           countryname_en = if_else(is.na(countryname_en), 
                                    countrycode(sourcevar = countrycode_n, 
                                                origin = "iso3n",
                                                destination = "country.name"),
                                    countryname_en)) |> 
    select(voting_station = uik_num, city_ru, city_en,
           countryname_ru, countryname_en, countrycode_c, countrycode_n, 
           voters_in_list = `Число избирателей, включенных в список избирателей`,
           ballots_received = `Число избирательных бюллетеней, полученных участковой избирательной комиссией`,
           early_ballots = `Число избирательных бюллетеней, выданных избирателям, проголосовавшим досрочно`,
           ballots_voting_space = `Число избирательных бюллетеней, выданных в помещении для голосования в день голосования`,
           ballots_out_of_voting_space = `Число избирательных бюллетеней, выданных вне помещения для голосования в день голосования`, 
           ballots_destroyed = `Число погашенных избирательных бюллетеней`,
           ballots_in_movable_boxes = `Число избирательных бюллетеней в переносных ящиках для голосования`,
           ballots_in_stationary_boxes = `Число бюллетеней в стационарных ящиках для голосования`,
           ballots_invalid = `Число недействительных избирательных бюллетеней`,
           ballots_valid = `Число действительных избирательных бюллетеней`,
           ballots_lost = `Число утраченных избирательных бюллетеней`,
           ballots_uncounted = `Число избирательных бюллетеней, не учтенных при получении`,
           davankov.abs_full = `Даванков Владислав Андреевич`,
           davankov_full = `Даванков Владислав Андреевич (%)`,
           putin.abs_full = `Путин Владимир Владимирович`,
           putin_full = `Путин Владимир Владимирович (%)`,
           slutsky.abs_full = `Слуцкий Леонид Эдуардович`,
           slutsky_full = `Слуцкий Леонид Эдуардович (%)` ,
           haritonov.abs_full = `Харитонов Николай Михайлович`,
           haritonov_full = `Харитонов Николай Михайлович (%)`) |> 
    mutate(across(c("putin_full", "davankov_full", 
                    "slutsky_full", "haritonov_full"),
                  ~ ./100),
           spoiled_full = ballots_invalid/(early_ballots + ballots_voting_space
                                           + ballots_out_of_voting_space
                                           + ballots_lost + ballots_uncounted),
           countryname_en = case_when(countryname_en == "USA" ~ "United States",
                                      countryname_en == "UAE" ~ "United Arab Emirates",
                                      .default = countryname_en),
           countryname_ru = case_when(countryname_en == "США" ~ "Соединенные Штаты Америки",
                                      countryname_en == "ОАЭ" ~ "Объединенные Арабские Эмираты",
                                      .default = countryname_ru))

#-------------------------------------------------------------------------------
# Data cleaning: yandex
  
  ## Aggregating yandex data
  yandex_24feb <- yandex_weekly |> 
    mutate(across(c("date_from", "date_to"), ~ ymd(.)),
           period = case_when(date_to > "2022-02-24"
                              & date_from < "2022-09-21" ~ "feb24_sep21",
                              date_to > "2022-09-21" ~ "after_sep21", 
                              .default = "before_feb24")) |> 
    group_by(destination_country, period) |> 
    summarize(searches = mean(count)) |> 
    pivot_wider(id_cols = "destination_country", 
                names_from = "period", values_from = "searches") |> 
    mutate(diff.feb24_sep21 = feb24_sep21 - before_feb24, 
           diff.after_sep21 = after_sep21 - before_feb24)
  
#-------------------------------------------------------------------------------
# Data cleaning: bilateral migration
  
  ## Isolate RUS as origin, select countries with an exit poll (not good)
  out_ru <- bilat_migration |> 
    filter(dest %in% unique(off_res_select$countrycode_c),
           orig == "RUS", year0 %in% c(1990, 2000, 2005, 2010, 2015)) |> 
    group_by(dest, year0) |> 
    summarise(mig_rate = mean(mig_rate)) |> 
    pivot_wider(id_cols = dest, names_from = year0, 
                values_from = mig_rate, names_prefix = "mig_rate_")
  
#-------------------------------------------------------------------------------
# Data cleaning: international migrant stock
  
  ## Isolate Russia as origin, select all countries where vote was held (good?)
  intstock_ru <- intstock_migration |> 
    rename_with(~ c("all_1990", "all_1995", "all_2000", "all_2005", "all_2010", 
                    "all_2015", "all_2020", "male_1990", "male_1995", 
                    "male_2000", "male_2005", "male_2010", "male_2015",
                    "male_2020", "female_1990", "female_1995", "female_2000", 
                    "female_2005", "female_2010", "female_2015", "female_2020"),
                8:28) |> 
    filter(`Location code of origin` == "643",
           `Location code of destination` %in% off_res_select$countrycode_n) |> 
    select(countrycode_n = `Location code of destination`, 8:28) |> 
    mutate(diff2015_2010 = (all_2015 - all_2010)/all_2010,
           diff2020_2015 = (all_2020 - all_2010)/all_2015)
  
#-------------------------------------------------------------------------------
# Data cleaning: fsb migration
  
  ## Select variables
  off_migration <- fsb_mig |> 
    filter(goals == "Итого по целям визита") |> 
    mutate(countrycode_n = 
             case_when(country == "АВСТРАЛИЯ" ~ 36,
                       country == "АВСТРИЯ, Австрийская Республика" ~ 40,
                       country == "АЗЕРБАЙДЖАН, Республика Азербайджан" ~ 31,
                       country == "АЛБАНИЯ, Республика Албания" ~ 8,
                       country == 
                         "АЛЖИР, Алжирская Народная Демократическая Республика" ~ 12,
                       country == "АНГОЛА, Республика Ангола" ~ 24,
                       country == "АРГЕНТИНА, Аргентинская Республика" ~ 32,
                       country == "АРМЕНИЯ, Республика Армения" ~ 51,
                       country == 
                         "АФГАНИСТАН^Переходное Исламское Государство Афганистан" ~ 4,
                       country == "БАХРЕЙН, Королевство Бахрейн" ~ 48,
                       country == "БАНГЛАДЕШ, Народная Республика Бангладеш" ~ 50,
                       country == "БЕНИН, Республика Бенин" ~ 204,
                       country == "БЕЛАРУСЬ, Республика Беларусь" ~ 112,
                       country == "БЕЛЬГИЯ, Королевство Бельгии" ~ 56,
                       country == "БОЛГАРИЯ, Республика Болгария" ~ 100,
                       country == 
                         "БОЛИВИЯ, Республика Боливия" ~ 68,
                       country == "БОСНИЯ И ГЕРЦЕГОВИНА" ~ 70,
                       country == "БОТСВАНА, Республика Ботсвана" ~ 72,
                       country == 
                         "БРАЗИЛИЯ, Федеративная Республика Бразилия" ~ 76,
                       # No Burundi
                       country == "БРУНЕЙ-ДАРУССАЛАМ" ~ 96,
                       country == "ВЕНГРИЯ, Венгерская Республика" ~ 348,
                       country == 
                         "ВЕНЕСУЭЛА, Боливарийская Республика Венесуэла" ~ 862,
                       country == "ВЬЕТНАМ, Социалистическая Республика Вьетнам" ~ 704,

                       country == "ГАБОН, Габонская Республика" ~ 266,
                       country == "ГАЙАНА, Республика Гайана" ~ 328,
                       country == "ГАНА, Республика Гана" ~ 288,
                       country == "ГВАТЕМАЛА, Республика Гватемала" ~ 320,
                       country == "ГВИНЕЯ, Гвинейская Республика" ~ 324,
                       # No Guinea-Bissau
                       country == "ГЕРМАНИЯ, Федеративная Республика Германия" ~ 276,
                       country == 
                         "ПАЛЕСТИНСКАЯ ТЕРРИТОРИЯ, ОККУПИРОВАННАЯ, Оккупированная Палестинская территория" ~ 275,
                       country == "ГРЕЦИЯ, Греческая Республика" ~ 300,
                       country == "ДАНИЯ, Королевство Дания" ~ 208,
                       country == 
                         "КОНГО, ДЕМОКРАТИЧЕСКАЯ РЕСПУБЛИКА, Демократическая Республика Конго" ~ 180,
                       country == "ДЖИБУТИ, Республика Джибути" ~ 262,
                       country == "ЕГИПЕТ, Арабская Республика Египет" ~ 818,
                       country == "ЗАМБИЯ, Республика Замбия" ~ 894,
                       country == "ЗИМБАБВЕ, Республика Зимбабве" ~ 716,
                       country == "ИЗРАИЛЬ, Государство Израиль" ~ 376,
                       country == "ИНДИЯ, Республика Индия" ~ 356,
                       country == "ИНДОНЕЗИЯ, Республика Индонезия" ~ 360,
                       country == "ИОРДАНИЯ, Иорданское Хашимитское Королевство" ~ 400,
                       country == "ИРАК, Республика Ирак" ~ 368,
                       country == 
                         "ИРАН, ИСЛАМСКАЯ РЕСПУБЛИКА, Исламская Республика Иран" ~ 364,
                       country == "ИРЛАНДИЯ" ~ 372,
                       country == "ИСЛАНДИЯ, Республика Исландия" ~ 352,
                       country == "ИСПАНИЯ, Королевство Испания" ~ 724,
                       country == "ИТАЛИЯ, Итальянская Республика" ~ 380,
                       country == "КАБО-ВЕРДЕ, Республика Кабо-Верде" ~ 132,
                       country == "КАЗАХСТАН, Республика Казахстан" ~ 398,
                       country == "КАМБОДЖА, Королевство Камбоджа" ~ 116,
                       country == "КАМЕРУН, Республика Камерун" ~ 120, 
                       
                       country == "КАТАР, Государство Катар" ~ 634, 
                       country == "КЕНИЯ, Республика Кения" ~ 404, 
                       country == "КИПР, Республика Кипр" ~ 196, 
                       country == "КИТАЙ, Китайская Народная Республика" ~ 156, 
                       country == "КОЛУМБИЯ, Республика Колумбия" ~ 170, 
                       country == "КОНГО, Республика Конго" ~ 178, 
                       country == 
                         "КОРЕЯ, НАРОДНО-ДЕМОКРАТИЧЕСКАЯ РЕСПУБЛИКА, Корейская Народно-Демократическая Республика" ~ 408,
                       country == "КОСТА-РИКА, Республика Коста-Рика" ~ 188, 
                       country == "КОТ Д`ИВУАР, Республика Кот д`Ивуар" ~ 384, 
                       country == "КУБА, Республика Куба" ~ 192, 
                       country == "КУВЕЙТ, Государство Кувейт" ~ 414, 
                       country == "КИРГИЗИЯ, Киргизская Республика" ~ 417, 
                       country == "ЛАОССКАЯ НАРОДНО-ДЕМОКРАТИЧЕСКАЯ РЕСПУБЛИКА" ~ 418,
                       country == "ЛАТВИЯ, Латвийская Республика" ~ 428,
                       country == "ЛИВАН, Ливанская Республика" ~ 422, 
                       country == 
                       "ЛИВИЙСКАЯ АРАБСКАЯ ДЖАМАХИРИЯ, Социалистическая Народная Ливийская Арабская Джамахирия" ~ 434, 
                       country == "ЛИТВА, Литовская Республика" ~ 440, 
                       country == "ЛЮКСЕМБУРГ, Великое Герцогство Люксембург" ~ 442,
                       country == "МАВРИТАНИЯ, Исламская Республика Мавритания" ~ 478,
                       country == "МАВРИКИЙ, Республика Маврикий" ~ 480, 
                       country == "МАДАГАСКАР, Республика Мадагаскар" ~ 450, 
                       country == "МАЛАЙЗИЯ" ~ 458, 
                       country == "МАЛИ, Республика Мали" ~ 466, 
                       country == "МАЛЬТА, Республика Мальта" ~ 470, 
                       country == "МАРОККО, Королевство Марокко" ~ 504, 
                       country == "МЕКСИКА, Мексиканские Соединенные Штаты" ~ 484,
                       country == "МОЗАМБИК, Республика Мозамбик" ~ 508, 
                       country == "МОЛДОВА, РЕСПУБЛИКА, Республика Молдова" ~ 498,
                       country == "МОНГОЛИЯ" ~ 496, 
                       country == "МЬЯНМА, Союз Мьянма" ~ 104, 
                       country == "НАМИБИЯ, Республика Намибия" ~ 516, 
                       country == "НЕПАЛ, Королевство Непал" ~ 524, 
                       country == "НИГЕРИЯ, Федеративная Республика Нигерия" ~ 566,
                       country == "НИДЕРЛАНДЫ, Королевство Нидерландов" ~ 528, 
                       country == "НИКАРАГУА, Республика Никарагуа" ~ 558, 
                       country == "НОВАЯ ЗЕЛАНДИЯ" ~ 554, 
                       country == "НОРВЕГИЯ, Королевство Норвегия" ~ 578, 
                       country == "ОБЪЕДИНЕННЫЕ АРАБСКИЕ ЭМИРАТЫ" ~ 784, 
                       country == "ОМАН, Султанат Оман" ~ 512,
                       country == "ПАКИСТАН, Исламская Республика Пакистан" ~ 586,
                       country == "ПАНАМА, Республика Панама" ~ 591, 
                       # No Paraguay
                       country == "ПЕРУ, Республика Перу" ~ 604, 
                       country == "ПОЛЬША, Республика Польша" ~ 616, 
                       country == "ПОРТУГАЛИЯ, Португальская Республика" ~ 620, 
                       country == "КОРЕЯ, РЕСПУБЛИКА, Республика Корея" ~ 410, 
                       country == "РУАНДА, Руандийская Республика" ~ 646, 
                       country == "РУМЫНИЯ" ~ 642, 
                       country == 
                         "САУДОВСКАЯ АРАВИЯ, Королевство Саудовская Аравия" ~ 682,
                       # No North Macedonia
                       country == "СЕЙШЕЛЫ, Республика Сейшелы" ~ 690, 
                       country == "СЕНЕГАЛ, Республика Сенегал" ~ 686, 
                       country == "СЕРБИЯ" ~ 688, 
                       country == "СИНГАПУР, Республика Сингапур" ~ 702, 
                       country == "СИРИЙСКАЯ АРАБСКАЯ РЕСПУБЛИКА" ~ 760, 
                       country == "СЛОВАКИЯ, Словацкая Республика" ~ 703, 
                       country == "СЛОВЕНИЯ, Республика Словения" ~ 705, 
                       country == "СОЕДИНЕННЫЕ ШТАТЫ, Соединенные Штаты Америки" ~ 840,
                       country == "СУДАН, Республика Судан" ~ 729, 
                       country == 
                       "ТАНЗАНИЯ, ОБЪЕДИНЕННАЯ РЕСПУБЛИКА, Объединенная Республика Танзания" ~ 834,
                       country == "ТАДЖИКИСТАН, Республика Таджикистан" ~ 762, 
                       country == "ТАИЛАНД, Королевство Таиланд" ~ 764, 
                       country == "ТУНИС, Тунисская Республика" ~ 788, 
                       country == "ТУРКМЕНИЯ, Туркменистан" ~ 795, 
                       country == "ТУРЦИЯ, Турецкая Республика" ~ 792, 
                       country == "УГАНДА, Республика Уганда" ~ 800, 
                       country == "УЗБЕКИСТАН, Республика Узбекистан" ~ 860, 
                       country == "УРУГВАЙ, Восточная Республика Уругвай" ~ 858, 
                       country == "ФИЛИППИНЫ, Республика Филиппины" ~ 608, 
                       country == "ФИНЛЯНДИЯ, Финляндская Республика" ~ 246,
                       country == "ФРАНЦИЯ, Французская Республика" ~ 250, 
                       country == "ХОРВАТИЯ, Республика Хорватия" ~ 191, 
                       # No Cetral African Republic
                       country == "ЧАД, Республика Чад" ~ 148, 
                       country == "ЧЕРНОГОРИЯ" ~ 499, 
                       country == "ЧЕШСКАЯ РЕСПУБЛИКА" ~ 203, 
                       country == "ЧИЛИ, Республика Чили" ~ 152, 
                       country == "ШВЕЙЦАРИЯ, Швейцарская Конфедерация" ~ 756, 
                       country == "ШВЕЦИЯ, Королевство Швеция" ~ 752, 
                       country == 
                         "ШРИ-ЛАНКА, Демократическая Социалистическая Республика Шри-Ланка" ~ 144, 
                       country == "ЭКВАДОР, Республика Эквадор" ~ 218, 
                       country == "ЭСТОНИЯ, Эстонская Республика" ~ 233, 
                       country == 
                         "ЭФИОПИЯ, Федеративная Демократическая Республика Эфиопия" ~ 231, 
                       country == "ЭРИТРЕЯ" ~ 232,
                       country == "ЮЖНАЯ АФРИКА, Южно-Африканская Республика" ~ 710,
                       country == "ЯМАЙКА" ~ 388, 
                       country == "ЯПОНИЯ" ~ 392, 
                       country == "КАНАДА" ~ 124,
                       country == 
                         "СОЕДИНЕННОЕ КОРОЛЕВСТВО, Соединенное Королевство Великобритании и Северной Ирландии" ~ 826,
                       .default = NA),
           countrycode_c = countrycode(countrycode_n, origin = "iso3n", 
                                       destination = "iso3c"),
           across(c(3:58), ~ if_else(is.na(.), 0, .))) |> 
    drop_na(countrycode_n) |> 
    rename_with(.fn = ~ paste0(rep(1:4, 4), "_", rep(2010:2023, each = 4)), 
                .cols = c(3:58)) |> 
    pivot_longer(cols = 3:51, names_to = c("quart", "year"), 
                 names_sep = "_", values_to = "tourist_trips") |> 
    group_by(countrycode_n, countrycode_c) |> 
    summarize(mean_trips = mean(tourist_trips))
    
# No Burundi, Central African Republic, Paraguay, Guinea-Bissau, North Macedonia
#-------------------------------------------------------------------------------
# Data cleaning: religion
  
  ## Select variables, homogenize countrycodes
  wrp_select <- wrp |> 
    group_by(COUNTRY) |>
    filter(YEAR == 2010) |> 
    ungroup() |>
    mutate(countrycode_n = case_when(is.na(NUMISO) | NUMISO == 0
                                     ~ countrycode(COWCODE, origin = "cown", destination = "iso3n"),
                                     .default = countrycode(NUMISO, origin = "iso3n", destination = "iso3n")
    ),
    countrycode_c = countrycode(countrycode_n, origin = "iso3n", destination = "iso3c")) |> 
    select(countrycode_n, countrycode_c, orthodox_num = CHRSORTH,
           orthodox_share = CHORTPCT) |> 
    drop_na(countrycode_n, countrycode_c)
  
#-------------------------------------------------------------------------------
# Data cleaning: qog
  
  ## Select variables and years
  qog_select <- qog |> 
    select(ccode, year, cname, vdem_polyarchy, bmr_dem, mad_gdppc, 
           wdi_gdpcapcon2015) |> 
    filter(year >= 2018) |> 
    pivot_wider(names_from = year, 
                values_from = c(vdem_polyarchy, bmr_dem, mad_gdppc, 
                                wdi_gdpcapcon2015)) |> 
    select(ccode, cname, vdem_polyarchy_2022, bmr_dem_2020, 
           mad_gdppc_2018, wdi_gdpcapcon2015_2022) |> 
    mutate(countrycode_n = countrycode(ccode, origin = "iso3n", 
                                       destination = "iso3n"),
           countrycode_c = countrycode(countrycode_n, origin = "iso3n", 
                                       destination = "iso3c")) |> 
    drop_na(countrycode_n, countrycode_c)
  
#-------------------------------------------------------------------------------
# Data cleaning: atop
  
  ## Select variables
  atop_select <- atop |> 
    filter(year == 2018, mem1 == 365 | mem2 == 365) |> 
    mutate(countrycow = case_when(mem1 == 365 ~ mem2,
                                  mem2 == 365 ~ mem1),
           countrycode_n = countrycode(countrycow, origin = "cown", 
                                       destination = "iso3n"),
           countrycode_c = countrycode(countrycode_n, origin = "iso3n", 
                                       destination = "iso3c")) |> 
    right_join(distinct(off_res_select, countrycode_n, countrycode_c), 
               by = c("countrycode_n", "countrycode_c")) |> 
    transmute(countrycode_n, countrycode_c, 
              nonagg = case_when(is.na(nonagg) ~ 0, nonagg == 1 ~ 1),
              consul = case_when(is.na(consul) | consul == 0 ~ 0, 
                                 consul == 1 ~ 1),
              neutral = case_when(is.na(neutral) | neutral == 0 ~ 0, 
                                  neutral == 1 ~ 1),
              defense = case_when(is.na(defense) | defense == 0 ~ 0, 
                                  defense == 1 ~ 1),
              obl_type = factor(rowSums(pick(nonagg, consul, neutral, defense)),
                                levels = 0:4, 
                                labels = c("0", "1", "2", "3", "4")))

#-------------------------------------------------------------------------------
# Data cleaning: trade
  
  ## Homogenize countrycodes
  trade_select <- trade |> 
    transmute(countryname = case_when(`Partner Name` == 
                                     "Ethiopia(excludes Eritrea)" ~ "Ethiopia",
                                   `Partner Name` == 
                                     "United States Minor Outlying I" ~ NA,
                                   `Partner Name` == 
                                     "Serbia, FR(Serbia/Montenegro)" ~ NA,
                                   .default = `Partner Name`),
              countrycode_n = countrycode(countryname, 
                                          origin = "country.name", 
                                          destination = "iso3n"),
              countrycode_c = countrycode(countrycode_n, origin = "iso3n", 
                                          destination = "iso3c"),
              export_value = `Export (US$ Thousand)`,
              import_value = `Import (US$ Thousand)`,
              export_share = `Export Partner Share (%)`,
              import_share = `Import Partner Share (%)`) |> 
    drop_na(countrycode_n, countrycode_c)

#-------------------------------------------------------------------------------
# Data cleaning: geodist
  
  ## Homogenize countrycodes
  geodist_select <- geodist |> 
    filter(iso_o == "RUS") |> 
    mutate(countrycode_n = countrycode(iso_d, origin = "iso3c", 
                                      destination = "iso3n"),
           countrycode_c = countrycode(countrycode_n, origin = "iso3n", 
                                      destination = "iso3c")) |> 
    drop_na(countrycode_n, countrycode_c) |>
    select(countrycode_n, countrycode_c, dist, distcap, distw, distwces)

#-------------------------------------------------------------------------------
# Merge data
  
  ## Compile the final dataset
  data_built <- off_res_select |> 
    drop_na(countrycode_c, countrycode_n) |>
    full_join(ep_cn, by = c("voting_station", "countrycode_c",
                            "countrycode_n", 
                            "city_en", "city_ru")) |> 
    left_join(ep.raw_agg, by = c("voting_station", "countrycode_c",
                                 "countrycode_n")) |> 
    left_join(out_ru, by = join_by("countrycode_c" == "dest")) |> 
    left_join(intstock_ru, by = "countrycode_n") |> 
    left_join(wrp_select, by = c("countrycode_c", "countrycode_n")) |> 
    left_join(qog_select, by = c("countrycode_c", "countrycode_n")) |> 
    left_join(atop_select, by = c("countrycode_c", "countrycode_n")) |> 
    left_join(trade_select, by = c("countrycode_c", "countrycode_n")) |> 
    left_join(hc, by = c("countrycode_n", "countrycode_c")) |> 
    left_join(geodist_select, by = c("countrycode_c", "countrycode_n")) |> 
    left_join(off_migration, by = c("countrycode_c", "countrycode_n")) |> 
    mutate(putin_full = if_else(countrycode_c == "TUR", putin_cec, putin_full),
           davankov_full = if_else(countrycode_c == "TUR", davankov_cec, davankov_full),
           spoiled_full = if_else(countrycode_c == "TUR", spoiled_cec, spoiled_full),
           slutsky_full = if_else(countrycode_c == "TUR", slutskiy_cec, slutsky_full),
           haritonov_full = if_else(countrycode_c == "TUR", haritonov_cec, haritonov_full))
    
#-------------------------------------------------------------------------------
# Aggregate to country level
  
  # Also remove some of the variables
  data_country <- data_built |> 
    drop_na(countrycode_n, countrycode_c) |> 
    group_by(countrycode_n, countrycode_c) |> 
    summarize(
      # Different voting metrics from official results 
      # (Istanbul was damaged during parsing - confirmed by author)
      across(c(voters_in_list:ballots_uncounted), ~ sum(.)),
      # Official results - shares
      across(c(davankov_full, putin_full, slutsky_full, 
               haritonov_full, spoiled_full), ~ mean(.)*100),
      # Official results - absolute values
      across(c(davankov.abs_full, putin.abs_full, slutsky.abs_full, 
               haritonov.abs_full), ~ sum(.)),
      # Exit poll - metrics
      across(c(voters_surveyed, voters_counted, 
               # Ballots in boxes replaces the variable for Istanbul
               ballots_in_boxes, removed_or_destroyed.abs), ~ sum(.)),
      # Exit poll - shares
      across(c(removed_or_destroyed, putin_ep, davankov_ep, slutskiy_ep,
               haritonov_ep, spoiled_ep), ~ mean(., na.rm = T)*100),
      # Exit poll - absolute values
      across(c(putin.abs_ep, davankov.abs_ep, 
               slutskiy.abs_ep, haritonov.abs_ep), ~ sum(.)),
      # Raw variables - shares
      across(c(share.raw_female, share.raw_othergender, 
               share.raw_age1824, share.raw_age2444, 
               share.raw_age4564, share.raw_age65, 
               share.raw_tourist, share.raw_afterfeb, 
               share.raw_before2014, share.raw_after2014, 
               share.raw_2019, share.raw_afterfebref, 
               share.raw_after2014ref, share.raw_2019ref, 
               share.raw_timemore4h, share.raw_local, 
               share.raw_trustanyyes, share.raw_trustanyno), ~ mean(.)*100),
      # Country level variables
      across(c(mig_rate_1990:mean_trips),
             # Ensure that all variables are country-level
             ~ if_else(all(. == first(.)), first(.), NA))) |> 
    mutate(ep = if_else(!is.na(putin_ep), 1, 0),
           friendly_status = relevel(
             factor(friendly_status, levels = 0:2, 
                    labels = c("Unfriendly", "Neutral", "Friendly")),
             ref = "Neutral"),
           putin_dist = putin_full - putin_ep, 
           davankov_dist = davankov_full - davankov_ep,
           spoiled_dist = spoiled_full - spoiled_ep)
  
#-------------------------------------------------------------------------------
# Save data
  
  ## Save the final datasets
  write_rds(data_built, here("data", "data_built", "data_built_.rds"))
  write_rds(data_country, here("data", "data_built", "data_country.rds"))
  
  ## Save the raw dataset
  write_rds(ep_raw_clean, here("data", "data_built", "ep_raw_clean.rds"))
  
