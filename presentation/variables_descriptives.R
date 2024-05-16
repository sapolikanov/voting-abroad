## -----------------------------------------------------------------------------
#| label: sourcing base dataframe

source(here::here("scripts", "raw_data_prep.R"))


## -----------------------------------------------------------------------------
#| label: setup chunk

  ## Required packages
  packages <- c("here", "kableExtra", "stargazer", "summarytools",
                "tidyverse", "modelsummary", 
                "readxl", "lubridate", "countrycode")
    
  ## Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
    if (any(installed_packages == FALSE)) {
      install.packages(packages[!installed_packages])
    }
    
  ## Load package
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Define global functions

  ## Fitting stargazer onto the page
  resizebox.stargazer = function(..., tab.width = "!", tab.height = "!")
      {
    require(stringr) 
    res = capture.output(stargazer::stargazer(...))
    tab.width = tab.width
    tab.height = tab.height
    res = prepend(res, "}", before = length(res))
  
    res = c(res[1:str_which(res, "^\\\\begin\\{tabular\\}.*")-1],
        paste0("\\resizebox*{",tab.width,"}{",tab.height,"}{%"),
        res[str_which(res, "^\\\\begin\\{tabular\\}.*"):length(res)]
        )
    cat(res, sep = "\n")
  }

# Define functions
  
  ## Package detacher
  detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", 
                        "package:grDevices", "package:utils", 
                        "package:datasets","package:methods","package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                    TRUE, FALSE)]
    package.list <- setdiff(package.list,basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
    
  }
  
# Define options
options(scipen = 999)


## -----------------------------------------------------------------------------
#| label: demography

# Demographic

## Original data
ep_raw_clean |> 
  mutate(sex = if_else(is.na(sex), "No Data", sex), 
         age_bin = if_else(is.na(age_bin), "No Data", age_bin)) |> 
  group_by(sex, age_bin) |> 
  summarize(n = n()) |> 
  mutate(pct = n/sum(n)*100) |> 
  ggplot(aes(x = pct, y = age_bin, fill = age_bin)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
    facet_wrap(~ sex, nrow = 5) +
    scale_fill_manual(values = c("#FFC374", "#F9E897", "#7F9F80", 
                                 "#124076", "#bf212f", "#bf212f")) +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10),
                       label = scales::label_number(suffix = "%")) +
    labs(x = "\nPercent of questionnaires",
         y = "",
         title = "Demographic composition of poll questionnaires across all observations",
         subtitle = paste("Including missing data patterns, `No Data` = NA in the data,\nN =", nrow(ep_raw_clean))) +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: tbl-example # WHAT? Only works with this label
#| layout-ncol: 2
#| layout-nrow: 1
#| layout-valign: top
#| tbl-cap: "`Other` gender in Argentina"
#| tbl-subcap: true

# Oh my god pdf with quarto table options is such a mess
# Reminder to self: see if this was reported on quarto github

kable(table(ep_raw_clean$countryname_en, ep_raw_clean$sex), booktabs = T,
      label = "t1a") |> 
  kable_styling(latex_options = c("scale_down", "hold_position"))

kable(table(ep_raw_clean$volunteer_id[ep_raw_clean$countryname_en == "Argentina"], 
            ep_raw_clean$sex[ep_raw_clean$countryname_en == "Argentina"]), booktabs = T)|> 
  kable_styling(latex_options = c("hold_position"))


## -----------------------------------------------------------------------------
#| label: adjusting demography

ep_raw_dem <- ep_raw_clean |> 
  mutate(across(c(age_bin, sex), ~ if_else(. == "Declined to answer" | is.na(.), "No Data", .)),
         sex = if_else(volunteer_id == "8023_2", "No Data", sex))

ep_raw_dem |> 
  group_by(sex, age_bin) |> 
  summarize(n = n()) |> 
  mutate(pct = n/sum(n)*100) |> 
  ggplot(aes(x = pct, y = age_bin, fill = age_bin)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
    facet_wrap(~ sex, nrow = 5) +
    scale_fill_manual(values = c("#FFC374", "#F9E897", "#7F9F80", 
                                 "#124076", "#bf212f")) +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 10),
                       label = scales::label_number(suffix = "%")) +
    labs(x = "\nPercent of questionnaires",
         y = "",
         title = "Demographic composition of poll questionnaires, adjusted",
         subtitle = paste("`No Data` is modified to include all missing data,\nN =", nrow(ep_raw_clean))) +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: time to the voting station

ep_raw_clean |> 
  group_by(time_to_vs, .drop = F) |> 
  summarise(n = length(time_to_vs)) |>
  mutate(pct = (n/sum(n)),
         lbl = if_else(pct > 0.02, scales::percent(pct), NA),
         time_to_vs = if_else(is.na(time_to_vs), "No Data", time_to_vs),
         time_to_vs = factor(time_to_vs, 
                               levels = c("<30 minutes", "30 minutes - 1 hour",
                                          "1 - 2 hours", "2 - 3 hours", "> 2 hours",
                                          "3 - 4 hours", "> 4 hours (staying for the night)",
                                          "Declined to answer", "No Data"))) |> 
  ggplot(aes(x = n, y = time_to_vs,
             fill = time_to_vs)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#FFC374", "#F9E897", "#b3c58b", "#7F9F80", "#007682", 
                                 "#124076", "#872c76", "#bf212f", "#bf212f")) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Time to voting station across all observations",
         subtitle = paste("N =", nrow(ep_raw_clean))) +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: adjusting time to the voting station

ep_raw_tvs <- ep_raw_dem |> 
  mutate(time_to_vs.less_than_hour = case_when(
    time_to_vs %in% c("<30 minutes", "30 minutes - 1 hour") ~ "Yes",
    time_to_vs %in% c("1 - 2 hours", "2 - 3 hours", "> 2 hours",
                      "3 - 4 hours", "> 4 hours (staying for the night)") ~ "No",
    .default = "No Data"),
         time_to_vs.more_than_4hours = case_when(
    time_to_vs == "> 4 hours (staying for the night)" ~ "Yes",
    time_to_vs %in% c("<30 minutes", "30 minutes - 1 hour",
                      "1 - 2 hours", "2 - 3 hours", "> 2 hours",
                      "3 - 4 hours") ~ "No",
    .default = "No Data"))

ep_raw_tvs |> 
  filter(!countryname_en %in% c("Australia", "New Zealand")) |> 
  pivot_longer(cols = c(time_to_vs.less_than_hour,
                        time_to_vs.more_than_4hours)) |> 
  group_by(name, value) |> 
  summarise(n = length(value)) |> 
  mutate(pct = n/sum(n)*100,
         name = case_when(name == "time_to_vs.less_than_hour"
                          ~ "Less than an hour to get to the voting station",
                          name == "time_to_vs.more_than_4hours"
                          ~ "More than 4 hours to get to the voting station")) |> 
  ggplot(aes(x = pct, y = value,
             fill = name)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = c("#FFC374", "#7F9F80")) +
      scale_x_continuous(limits = c(0, 80),
                         breaks = seq(0, 80, 10),
                         label = scales::label_number(suffix = "%")) +
      facet_wrap(~ name, nrow = 2) +
      labs(x = "\nPercent of respondents",
           y = "",
           title = "Time to voting station, adjusted",
           subtitle = paste(
             "N =", nrow(filter(ep_raw_clean,!countryname_en %in% c("Australia",
                                                               "New Zealand")))
                            ),
           caption = 
             "Note: Australia and New Zealand not included in the count as there are no data for them") +
      theme_minimal() +
      theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: time out of russia

ep_raw_clean |> 
  group_by(out_of_Russia_time) |> 
  summarise(n = n()) |> 
  mutate(pct = round(n/sum(n), 4),
         lbl = if_else(pct < 0.01, NA, scales::percent(pct)),
         out_of_Russia_time = if_else(is.na(out_of_Russia_time), "No Data", out_of_Russia_time),
         out_of_Russia_time = factor(out_of_Russia_time,
                                     levels = c("< 6 months", "6 months - 2 years", "< 2 years", 
                                                "2 - 5 years", "> 5 years", "6 - 10 years", "> 10 years", 
                                                "Tourist (lives in Russia)", "Declined to answer", "No Data"))) |> 
  ggplot(aes(x = n, y = out_of_Russia_time,
             fill = out_of_Russia_time)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#E6A167", "#FFC374", "#F9E897", "#b3c58b", "#7F9F80", "#007682", 
                                 "#124076", "#872c76", "#bf212f", "#bf212f")) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Time living outside of Russia across all observations",
         subtitle = paste("N =", nrow(ep_raw_clean))) +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: adjusting time out of Russia

ep_raw_out <- ep_raw_tvs |> 
  mutate(out_of_Russia_time = case_when(
    out_of_Russia_time %in% c("< 6 months", "6 months - 2 years", "< 2 years") ~ "After invasion",
    out_of_Russia_time %in% c("> 5 years", "6 - 10 years") ~ "After annexation",
    out_of_Russia_time == "> 10 years" ~ "Before annexation", 
    out_of_Russia_time == "Declined to answer" | is.na(out_of_Russia_time) ~ "No Data",
    .default = out_of_Russia_time))

ep_raw_out |> 
  filter(!countryname_en %in% c("Australia", "New Zealand")) |> 
  group_by(out_of_Russia_time) |> 
  summarise(n = n()) |> 
  mutate(pct = round(n/sum(n), 4),
         lbl = if_else(pct < 0.01, NA, scales::percent(pct)),
         out_of_Russia_time = factor(out_of_Russia_time,
           levels = c("After invasion", "2 - 5 years", "After annexation",
                      "Before annexation", "Tourist (lives in Russia)", "No Data"))) |> 
  ggplot(aes(x = n, y = out_of_Russia_time,
             fill = out_of_Russia_time)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#FFC374", "#b3c58b", "#7F9F80", 
                                 "#124076", "#872c76", "#bf212f", "#bf212f")) +
    scale_x_continuous(limits = c(0, 30000),
                       breaks = seq(0, 30000, 5000)) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Time living outside of Russia, adjusted",
         subtitle = "N = 68.593 with Australia and NZ excluded") +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: trust in the result

ep_raw_clean |> 
  group_by(result_trust) |> 
  summarise(n = n()) |> 
  mutate(pct = round(n/sum(n), 4),
         lbl = if_else(pct < 0.03, NA, scales::percent(pct)),
         result_trust = if_else(is.na(result_trust), "No Data", 
                                      result_trust),
         result_trust = factor(result_trust,
                                     levels = c("Definitely no", "Probably no", 
                                                "Don't know", "Probably yes", 
                                                "Definitely yes", "> 10 years",
                                                "Declined to answer", "No Data"))) |> 
  ggplot(aes(x = n, y = result_trust,
             fill = result_trust)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#E6A167", "#FFC374", "#F9E897", "#b3c58b", "#7F9F80", 
                                 "#bf212f", "#bf212f")) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Trust in the outcome of the election across all observations",
         subtitle = paste("N =", nrow(ep_raw_clean))) +
    theme_minimal() +
    theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: adjusting trust in the result

ep_raw_trst <- ep_raw_out |> 
  mutate(result_trust_bin = case_when(result_trust %in% c("Probably no", "Definitely no") ~ "No",
                                      result_trust %in% c("Probably yes", "Definitely yes") ~ "Yes",
                                      result_trust == "Declined to answer" | is.na(result_trust) ~ "No Data",
                                      .default = result_trust))

ep_raw_trst |> 
  filter(!countryname_en %in% c("Australia", "New Zealand")) |> 
  group_by(result_trust_bin) |> 
  summarise(n = n()) |> 
  mutate(pct = round(n/sum(n), 4),
         lbl = if_else(pct < 0.01, NA, scales::percent(pct)),
         result_trust_bin = factor(result_trust_bin,
                                   levels = c("No", "Yes", "Don't know", "No Data"))) |> 
  ggplot(aes(x = n, y = result_trust_bin,
             fill = result_trust_bin)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#FFC374", "#F9E897", "#7F9F80", 
                                 "#bf212f", "#bf212f")) +
    scale_x_continuous(limits = c(0, 50000),
                       breaks = seq(0, 50000, 5000)) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Trust in the outcome of the election",
         subtitle = "N = 68.593 with Australia and NZ excluded") +
    theme_minimal() +
    theme(legend.position = "none")
  


## -----------------------------------------------------------------------------
#| label: building dependents

ep_raw_dep <- ep_raw_trst |> 
  mutate(vote_putin = if_else(vote == "Putin", 1, 0),
         vote_davankov = if_else(vote == "Davankov", 1, 0),
         vote_spoiled = if_else(vote == "Spoiled ballot", 1, 0),
         vote_opposition = if_else(
           vote %in% c("Davankov", "Spoiled ballot"), 1, 0),
         vote_declined = if_else(vote == "Declined to answer", 1, 0),
         vote_putin_declined = if_else(
           vote %in% c("Putin", "Declined to answer"), 1, 0),
         vote = factor(vote))

results1 <- ep_raw_dep |> 
  filter(!countryname_en %in% c("Australia", "New Zealand")) |> 
  group_by(vote) |> 
  summarise(n = n()) |> 
  mutate(pct = round(n/sum(n), 4),
         lbl = if_else(pct < 0.02, NA, scales::percent(pct)),
         result_trust_bin = factor(vote,
                                   levels = c("Putin", "Davankov", 
                                              "Spoiled ballot", "Slutsky", 
                                              "Haritonov", "Tore up/took", 
                                              "Declined to answer"))) |> 
  ggplot(aes(x = n, y = reorder(vote, n),
             fill = vote)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = lbl), size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#b3c58b", "#bf212f",   
                                 "#FFC374","#124076", "#E6A167", "#7F9F80","#872c76")) +
    scale_x_continuous(limits = c(0, 35000),
                       breaks = seq(0, 35000, 5000)) +
    labs(x = "\nNumber of respondents",
         y = "",
         title = "Voting choices",
         subtitle = "N = 68.593 with Australia and NZ excluded") +
    theme_minimal() +
    theme(legend.position = "none")

results1


## -----------------------------------------------------------------------------
#| label: exportin

## Detach all packages
# detachAllPackages()

## Remove all objects not equal to ``ep_raw_clean``
# rm(list = setdiff(ls(), c("ep_raw_dep", "results1", "merged")))

