
#' deaths_per_capita
#'
#' doesnt join on year 2061
#'
#' @param deaths
#' @param pop
#' @param save_to_file
#'
#' @import dplyr
#'
#' @return
#' @export
#'
deaths_per_capita <- function(deaths = NA,
                              pop = NA,
                              save_to_file = TRUE,
                              save_folder = here::here("output_data")) {

  if (is.na(deaths)) {
    deaths <- read.csv(here::here("output_data", "clean_deaths.csv"))
  }
  if (is.na(pop)) {
    pop <- read.csv(here::here("output_data", "clean_pop.csv"))
  }

  out <-
    deaths %>%
    select(-X) %>%
    as_tibble() %>%
    merge(pop,
          by = c("sex", "age", "ETH.group", "year")) %>%
    mutate(deaths_per_capita = deaths/pop) %>%
    select(-pop, -deaths, -X)

  if (save_to_file) {

    message(paste("Output saved to", here::here("output_data", "deaths_per_capita.csv")))
    write.csv(out, file = paste0(save_folder, "/deaths_per_capita.csv"))
  }

  as_tibble(out)
}
