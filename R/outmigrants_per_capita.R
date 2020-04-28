
#' outmigrants_per_capita
#'
#' @param outmigrants
#' @param pop
#' @param save_to_file
#'
#' @import dplyr
#'
#' @return
#' @export
#'
outmigrants_per_capita <- function(outmigrants = NA,
                                   pop = NA,
                                   save_to_file = TRUE,
                                   save_folder = here::here("output_data")) {

  if (is.na(outmigrants)) {
    outmigrants <- read.csv(here::here("output_data", "clean_outmigrants.csv"))
  }
  if (is.na(pop)) {
    pop <- read.csv(here::here("output_data", "clean_pop.csv"))
  }

  out <-
    outmigrants %>%
    select(-X) %>%
    as_tibble() %>%
    mutate(ETH_fac = as.factor(ETH.group),
           outmigrants = as.numeric(outmigrants),
           age = as.numeric(age)) %>%
    merge(pop,
          by = c("sex", "age", "ETH.group", "year")) %>%
    mutate(outmigrants_per_capita = outmigrants/pop) %>%
    select(-pop, -outmigrants, -X)

  if (save_to_file) {

    message(paste("Output saved to", here::here("output_data", "outmigrants_per_capita.csv")))
    write.csv(out, file = paste0(save_folder, "/outmigrants_per_capita.csv"))
  }

  as_tibble(out)
}
