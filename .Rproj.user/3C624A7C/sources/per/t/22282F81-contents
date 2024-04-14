# First look at data for the analysis of voting abroad
# Author: Stepan Polikanov
# Date-time created: 3/18/2024 11:40 Madrid time

# Manage packages

  ## Package list
  packages <- c("readxl", "tidyverse", "lubridate", "lme4", "here",
                "countrycode", "ggrepel", 
                "rnaturalearth", "rnaturalearthdata", "sf", "classInt")
  
  ## Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  ## Load packages
  invisible(lapply(packages, library, character.only = TRUE))
  
# Data
  
  ## exit poll 18-03-24 11:21 CET
  # ep <- read_excel("data/exitpoll_18-03-24_11-21CET.xlsx")
  ep <- read_excel(here("data", "exitpoll_18-03-24_20-29CET.xlsx"), sheet = 2)
  
  ## yandex search data
  yandex_weekly <- read_csv(here("data", "df_countries_weekly.csv"))
  
  ## Migration
  
    ### Bilateral migration dataset
    bilat_migration <- read_csv(here("data", "bilat_mig_sex.csv"))
    
    ### International migrant stock
      
      #### Main
      intstock_migration <- read_excel(
        here("data", 
             "undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx"),
        sheet = 2, skip = 9, guess_max = 37051
        )
  
  ## official election results
  
    ### 98% of processed ballots
    # off_res <- read_tsv("data/results-uik-20240318T1503UTC.tsv")
  
    ### Final data dumo (100 processed ballots)
    off_res <- read_tsv(here("data", "results-uik-20240320T0352UTC.tsv"))
    
  ## Supplementary data
    
    ### Voting station to country dictionary
    uik_dict <- read_excel(here("data", "uik_dictionary.xlsx"), sheet = 2)
    countrynameru_dict <- read_excel(here("data", "uik_dictionary.xlsx"), sheet = 3)
  
  
#------------------------------------------------------------------------------#
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
                             city_en))
  
# Data cleaning: exit poll raw
  
  ## Load data
  source(here("scripts", "raw_data_prep.R"))
  
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
                sum(out_of_Russia_time == "Tourist (lives in Russia)", na.rm = T)/n(),
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
                sum(time_to_vs == "> 4 hours (staying for the night)", na.rm = T)/n(),
              share.raw_local = 
                sum(time_to_vs %in% c("<30 minutes", "30 minutes - 1 hour"), 
                    na.rm = T)/n(),
              share.raw_trustanyyes = 
                sum(result_trust %in% c("Definitely yes", "Probably yes"), 
                    na.rm = T)/n(),
              share.raw_trustanyno = 
                sum(result_trust %in% c("Definitely no", "Probably no"), na.rm = T)/n())
  
# Data cleaning: uik dictionary 
  
  ## Convert countrynames in Russian provided by the Russian Ministry of Foreign Affairs 
  ## to match with those int the international code dictionary
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
           countrycode_c = if_else(tik == "Город Байконур (Республика Казахстан)", 
                                   "KAZ", countrycode_c),
           countryname_ru = if_else(tik == "Город Байконур (Республика Казахстан)", 
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
                                           + ballots_lost + ballots_uncounted))
  
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
  
  # Data cleaning: bilateral migration
  
  ## Isolate RUS as origin, select countries with an exit poll (not good)
  out_ru <- bilat_migration |> 
    filter(dest %in% unique(off_res_select$countrycode_c),
           orig == "RUS", year0 %in% c(1990, 2000, 2005, 2010, 2015)) |> 
    group_by(dest, year0) |> 
    summarise(mig_rate = mean(mig_rate)) |> 
    pivot_wider(id_cols = dest, names_from = year0, 
                values_from = mig_rate, names_prefix = "mig_rate_")
  
  # Data cleanin: international migrant stock
  
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
  
#------------------------------------------------------------------------------#
# Merge data
  
  ## Attach exit poll results, raw exit poll variable aggregates, migration measures
  merged <- off_res_select |> 
    full_join(ep_cn, by = c("voting_station", "countrycode_c",
                            "countrycode_n", "countryname_ru", 
                            "countryname_en", "city_en", "city_ru")) |> 
    left_join(ep.raw_agg, by = c("voting_station", "countrycode_c",
                                 "countrycode_n")) |> 
    left_join(out_ru, by = join_by("countrycode_c" == "dest")) |> 
    left_join(intstock_ru, by = "countrycode_n")
