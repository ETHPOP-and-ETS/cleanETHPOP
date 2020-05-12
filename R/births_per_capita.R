
#' births_per_capita
#'
#' includes denominator for
#'
#'  - whole population
#'  - women only
#'  - women of childbearing age 15 to 45 years old
#'
#' @param births data
#' @param pop data
#' @param save_to_file TRUE/FALSE
#' @param save_folder string
#'
#' @import dplyr
#'
#' @return
#' @export
#'
births_per_capita <- function(births = NA,
                              pop = NA,
                              save_to_file = TRUE,
                              save_folder = here::here("output_data")) {

  if (is.na(births)) {
    births <- read.csv(here::here("output_data", "clean_births.csv"))
  }
  if (is.na(pop)) {
    pop <- read.csv(here::here("output_data", "clean_pop.csv"))
  }

  births <-
    births %>%
    select(-X) %>%
    as_tibble()

  out <-
    pop %>%
    group_by(ETH.group, year) %>%
    summarise(total_pop = sum(pop)) %>%
    merge(births, by = c("year", "ETH.group")) %>%
    mutate(births_per_capita = births/total_pop) %>%
    select(-total_pop)

  # women only
  out <-
    pop %>%
    filter(sex == "F") %>%
    group_by(ETH.group, year) %>%
    summarise(total_pop = sum(pop)) %>%
    merge(out, by = c("year", "ETH.group")) %>%
    mutate(births_per_capita_F = births/total_pop) %>%
    select(-total_pop)

  # childbearing age women only
  out <-
    pop %>%
    filter(sex == "F",
           age >= 15 & age <= 45) %>%
    group_by(ETH.group, year) %>%
    summarise(total_pop = sum(pop)) %>%
    merge(out, by = c("year", "ETH.group")) %>%
    mutate(births_per_capita_15_45 = births/total_pop) %>%
    select(-total_pop)

  if (save_to_file) {

    message(paste("Output saved to", here::here("output_data", "births_per_capita.csv")))
    write.csv(out, file = paste0(save_folder, "/births_per_capita.csv"))
  }

  as_tibble(out)
}
