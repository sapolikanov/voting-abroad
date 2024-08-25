## Prepping raw exit poll data
## Dependencies: exitpoll_rawdata.xlsx

# Packages
source(here::here("utilities", "check_packages.R"))

# Load exit poll raw data
ep_raw <- read_excel(here("data", "data_raw", "exitpoll_individual", 
                          "exitpoll_rawdata.xlsx"), 
                     sheet = 4, guess_max = 69262)

# 
ep_raw_clean <- ep_raw |> 
  separate(col = "Страна", into = c("countryname_ru", "countryname_en"),
           sep = " / ") |> 
  separate(col = "Город", into = c("city_ru", "city_en"), 
           sep = " / ") |> 
  mutate(countryname_en = if_else(countryname_en == "Czech", 
                                  "Czechia", countryname_en),
         countrycode_n = countrycode(countryname_en, 
                                     origin = "country.name", 
                                     destination = "iso3n"),
         countrycode_c = countrycode(countrycode_n, 
                                     origin = "iso3n", 
                                     destination = "iso3c"),
         voting_station = as.character(`УИК`),
         city_en = if_else(str_detect(city_ru, "№") == T, 
                           paste(city_en, str_sub(city_ru, -1, 
                                                  nchar(city_ru))),
                           city_en)) |> 
  transmute(volunteer_id = `ID волонтера`,
            data_source = case_when(
            `Источник данных` == "Электронная анкета избирателя по одноразовым QR кодам"
            ~ "One-off QR e-form",
            `Источник данных` == "Общая электронная анкета" ~ "General e-form",
            `Источник данных` == "Электронная анкета - Токио" ~ "Tokio e-form",
            `Источник данных` == "Электронная анкета - Стокгольм" ~ "Stockholm e-form",
            `Источник данных` == "Электронная анкета - Прага" ~ "Prague e-form",
            `Источник данных` == "Электронная анкета - Дубай" ~ "Dubai e-form",
            `Источник данных` == "Статистика ответов - Веллингтон" ~ "Wellington e-form",
            `Источник данных` == "Статистика ответов - Сидней" ~ "Sydney e-form"),
            upload_time = ymd_hms(`Отметка времени`),
            countryname_en, countryname_ru, city_ru, city_en, countrycode_c, countrycode_n,
            voting_station, 
            vote = case_when(
              `За какого кандидата вы проголосовали?` == "В. А. Даванков" ~ "Davankov",
              `За какого кандидата вы проголосовали?` == "Л.В. Слуцкий" ~ "Slutsky",
              `За какого кандидата вы проголосовали?`
              == "Недействительный бюллетень (ни одной галочки, несколько галочек)"
              ~ "Spoiled ballot",
              `За какого кандидата вы проголосовали?` == "Бюллетень унесён/выброшен/порван"
              ~ "Tore up/took",
              `За какого кандидата вы проголосовали?` == "В.В. Путин" ~ "Putin",
              `За какого кандидата вы проголосовали?` == "Не хочу отвечать"
              ~ "Declined to answer",
              `За какого кандидата вы проголосовали?` == "Н.М. Харитонов" ~ "Haritonov"
              ),
            sex = case_when(`Ваш пол` == "Мужской" ~ "Male",
                            `Ваш пол` == "Женский" ~ "Female",
                            `Ваш пол` == "Другое" ~ "Other",
                            `Ваш пол` == "Не хочу отвечать" ~ "Declined to answer",
                            .default = NA),
            age_bin = case_when(
              `Возрастная группа` == "Не хочу отвечать" ~ "Declined to answer",
              .default = `Возрастная группа`),
            out_of_Russia_time = case_when(
              `Как давно вы не живете в России?` == "Меньше 6 месяцев"~ "< 6 months",
              `Как давно вы не живете в России?` == "6 месяцев - 2 года (после февраля 2022)"
              ~ "6 months - 2 years",
              `Как давно вы не живете в России?` == "Менее 2 лет" ~ "< 2 years",
              `Как давно вы не живете в России?` == "2 - 5 лет" ~ "2 - 5 years",
              `Как давно вы не живете в России?` == "Более 5 лет" ~ "> 5 years",
              `Как давно вы не живете в России?` == "6-10 лет"~ "6 - 10 years",
              `Как давно вы не живете в России?` == "Более 10 лет" ~ "> 10 years",
              `Как давно вы не живете в России?` == "Не хочу отвечать"
              ~ "Declined to answer",
              `Как давно вы не живете в России?` == "Турист/ка (живу в РФ)"
              ~ "Tourist (lives in Russia)", .default = NA),
            time_to_vs = case_when(
              `Как долго вы добирались до избирательного участка?` == "<30 минут" 
              ~ "<30 minutes" ,
              `Как долго вы добирались до избирательного участка?` == "30 мин - 1 час" 
              ~ "30 minutes - 1 hour" ,
              `Как долго вы добирались до избирательного участка?` == "1 - 2 часа"
              ~ "1 - 2 hours",
              `Как долго вы добирались до избирательного участка?` == "2 - 3 часа"
              ~ "2 - 3 hours",
              `Как долго вы добирались до избирательного участка?` == "3 - 4 часа"
              ~ "3 - 4 hours",
              `Как долго вы добирались до избирательного участка?` == "Более 2 часов"
              ~ "> 2 hours" ,
              `Как долго вы добирались до избирательного участка?`
              == "Более 4 часов (остаюсь ночевать в этом городе)"
              ~ "> 4 hours (staying for the night)",
              `Как долго вы добирались до избирательного участка?` == "Не хочу отвечать"
              ~ "Declined to answer", .default = NA),
            result_trust = case_when(
              `Вы доверяете результатам выборов в России?` %in% c("Точно нет", "Точно НЕТ")
              ~ "Definitely no",
              `Вы доверяете результатам выборов в России?` == "Скорее НЕ доверяю"
              ~ "Probably no",
              `Вы доверяете результатам выборов в России?` == "Скорее доверяю"
              ~ "Probably yes",
              `Вы доверяете результатам выборов в России?` %in% c("Точно да", "Точно ДА")
              ~ "Definitely yes",
              `Вы доверяете результатам выборов в России?` == "Затрудняюсь ответить"
              ~ "Don't know",
              `Вы доверяете результатам выборов в России?` == "Не хочу отвечать"
              ~ "Declined to answer", .default = NA),
            uik_closetime = `УИК - Время закрытия участка (CET)`,
            uik_nvoters = `УИК - Человек посчитано на выходе`,
            uik_didntvote_closed = `УИК - Человек не успели голосовать до закрытия`,
            id = row_number())

#-------------------------------------------------------------------------------
# Save data

write_rds(ep_raw_clean, file = here("data", "data_built", "ep_raw_clean.rds"))
