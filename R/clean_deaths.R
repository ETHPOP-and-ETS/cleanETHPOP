
#' clean_deaths
#'
#' @param dir_path location of ETHPOP data
#' @param rtn TRUE/FALSE
#' @param save_to_file TRUE/FALSE
#' @param save_name output data file
#' @param save_path output data location
#' @param save_format e.g. csv or RData
#'
#' @return
#' @export
#' @import dplyr, reshape2, tidyr, readr
#'
#' @examples
#'   clean_deaths()
#'
clean_deaths <- function(dir_path = system.file("extdata", "Leeds1", "Deaths", package = "cleanETHPOP"),
                         rtn = TRUE,
                         save_to_file = TRUE,
                         save_name = "clean_deaths",
                         save_path = here::here("output_data"),
                         save_format = "csv") {

  file_names <- list.files(dir_path)

  ##TODO: this isnt the correct lookup table
  # ethnic_groups <- read_csv("~/data/ethnic_groups.csv")

  deaths_year <- list()

  for (i in seq_along(file_names)) {

    dat <- read_csv(paste(dir_path, file_names[i], sep = "/"))
    year_name  <- substr(file_names[i], start = 8, stop = 11)

    deaths_year[[i]] <-
      dat %>%
      mutate(M0.1 = MB.0 + M0.1,
             F0.1 = FB.0 + F0.1) %>%
      select(-MB.0, -FB.0) %>%
      filter(grepl('E', LAD.code)) %>%
      group_by(ETH.group) %>%
      summarise_at(.vars = vars(M0.1:F100.101p),
                   sum) %>%
      melt(id.vars = "ETH.group") %>%

      # clean age variable
      separate(variable, c("age", NA)) %>%
      separate(age, c("sex", "age"), sep = 1) %>%
      mutate(age = gsub("p", "", age)) %>%
      rename(deaths = value) %>%
      # mutate(age = gsub(x = age, 'M|F', '')) %>%  # combine sex

      # group ages in to 5 year ranges
      mutate(agegrp = cut(as.numeric(age),
                          breaks = seq(0, 105, by = 5),
                          right = FALSE)) %>%
      # group_by(ETH.group,
      #          agegrp,
      #          sex) %>%
      # summarise(deaths = sum(deaths)) %>%
      # ungroup() %>%
      mutate(year = year_name)

      #%>%
      # add ethnicity description
      #merge(ethnic_groups, all = TRUE)
  }

  deaths_dat <- do.call(rbind, deaths_year)

  if (save_to_file) {

    message(paste("Output saved to", paste0(save_path, "/", save_name, ".csv")))
    write.csv(deaths_dat, file = paste0(save_path, "/", save_name, ".csv"))
  }

  if (rtn)
    return(deaths_dat)
}
