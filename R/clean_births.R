
#' clean_births
#'
#' @param dir_path raw data location
#' @param rtn TRUE/FALSE
#' @param save_to_file TRUE/FALSE
#' @param save_name string
#' @param save_folder output data location
#' @param save_format csv or RData
#' @param m_to_100_f relative population sizes at birth from ONS
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#' @examples
#'   clean_births()
#'
clean_births <- function(dir_path = system.file("extdata", "Leeds1", "Births", package = "cleanETHPOP"),
                         m_to_100_f = 105,
                         rtn = TRUE,
                         save_to_file = TRUE,
                         save_name = "clean_births",
                         save_folder = here::here("output_data"),
                         save_format = "csv") {

  file_names <- list.files(dir_path)

  ##TODO: this isnt the correct lookup table
  # ethnic_groups <- read_csv("~/data/ethnic_groups.csv")

  births_year <- list()

  for (i in seq_along(file_names)) {

    dat <- read_csv(paste(dir_path, file_names[i], sep = "/"))
    year_name <- unlist(strsplit(file_names[i], "_"))[2]

    births_year[[i]] <-
      dat %>%
      filter(grepl('E', LAD.code)) %>%
      group_by(ETH.group) %>%
      summarise(tot_births = sum(BirthsAll),
                mbirths = tot_births*m_to_100_f/(m_to_100_f + 100),
                fbirths = tot_births*100/(m_to_100_f + 100)) %>%
      mutate(year = year_name) #%>%
    # merge(ethnic_groups, all = TRUE)
  }

  births_dat <-
    do.call(rbind, births_year) %>%
    melt(id.vars = c("ETH.group", "year"),
         variable.name = "sex") %>%
    rename(births = value) %>%
    mutate(sex = ifelse(sex == "mbirths",
                        "M",
                        ifelse(sex == "fbirths",
                               "F",
                               "person")))
  if (save_to_file) {

    save_fn <-
      if(save_format == "csv") {write.csv}
      else if(save_format == "RData") {save}

    save_path <- paste0(save_folder, "/", save_name, ".", save_format)
    write.csv(births_dat, file = save_path)
  }

  if (rtn)
    as_tibble(births_dat)
}
