
#' clean_births
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
#' @import dplyr
#' @importFrom readr read_csv
#' @examples
#'   clean_births()
#'
clean_births <- function(dir_path = "C:/Users/ngreen1/Documents/data/Respository/Leeds1/Births",
                         rtn = TRUE,
                         save_to_file = TRUE,
                         save_name = "clean_births",
                         save_path = "output_data",
                         save_format = "csv") {

  file_names <- list.files(dir_path)

  ##TODO: this isnt the correct lookup table
  # ethnic_groups <- read_csv("~/data/ethnic_groups.csv")

  births_year <- list()

  for (i in seq_along(file_names)) {

    dat <- read_csv(paste(dir_path, file_names[i], sep = "/"))
    year_name  <- substr(file_names[i], start = 8, stop = 11)

    births_year[[i]] <-
      dat %>%
      filter(grepl('E', LAD.code)) %>%
      group_by(ETH.group) %>%
      summarise(births = sum(BirthsAll)) %>%
      mutate(year = year_name) #%>%
    # merge(ethnic_groups, all = TRUE)
  }

  births_dat <- do.call(rbind, births_year)

  if (save_to_file) {
    write.csv(births_dat, file = paste0(save_name, ".csv"))
  }

  if (rtn)
    return(births_dat)
}
