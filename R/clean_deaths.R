
#' clean_deaths
#'
#' @param dir_path
#' @param rtn
#' @param save_to_file
#' @param save_name
#' @param save_path
#' @param save_format
#'
#' @return
#' @export
#' @import dplyr, reshape2, tidyr
#'
#' @examples
#'   clean_deaths()
#'
clean_deaths <- function(dir_path = "C:/Users/ngreen1/Documents/data/Respository/Leeds1/Deaths",
                         rtn = TRUE,
                         save_to_file = TRUE,
                         save_name = "clean_deaths",
                         save_path = "output_data",
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
      # mutate(age = gsub(x = age, 'M|F', '')) %>%  # combine sex

      # group ages in to 5 year ranges
      mutate(agegrp = cut(as.numeric(age),
                          breaks = seq(0, 105, by = 5),
                          right = FALSE)) %>%
      group_by(ETH.group,
               agegrp,
               sex) %>%
      summarise(deaths = sum(value)) %>%
      mutate(year = year_name)

      #%>%
      # add ethnicity description
      #merge(ethnic_groups, all = TRUE)
  }

  deaths_dat <- do.call(rbind, deaths_year)

  if (save_to_file) {
    write.csv(deaths_dat, file = paste0(save_name, ".csv"))
  }

  if (rtn)
    return(deaths_dat)
}
