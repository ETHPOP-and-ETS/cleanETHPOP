
#' clean_pop
#'
#' @param dir_path
#' @param rtn
#' @param save_to_file
#' @param save_name
#' @param save_folder
#' @param save_format
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom readr read_csv
#' @examples
#'   clean_pop()
#'
clean_pop <- function(
  dir_path = system.file("extdata", "Leeds1", "Population", package = "cleanETHPOP"),
  rtn = TRUE,
  age_grp = FALSE,
  save_to_file = TRUE,
  save_name = "clean_pop",
  save_folder = here::here("output_data"),
  save_format = "csv") {

  file_names <- list.files(dir_path)

  pop_year <- list()

  for (i in seq_along(file_names)) {

    dat <- read_csv(paste(dir_path, file_names[i], sep = "/"))
    year_name  <- substr(file_names[i], start = 11, stop = 14)

    pop_year[[i]] <-
      dat %>%
      filter(grepl('E', LAD.code)) %>%
      group_by(ETH.group) %>%
      summarise_at(.vars = vars(M0:F100p),
                   sum) %>%
      melt(id.vars = "ETH.group") %>%

      # clean age variable
      separate(variable, c("sex", "age"), sep = 1) %>%
      mutate(age = gsub("p", "", age)) %>%
      group_by(age)

    if (age_grp) { # group ages in to 5 year ranges
      pop_year[[i]] <-
        pop_year[[i]] %>%
        ungroup(age) %>%
        mutate(agegrp = cut(as.numeric(age),
                            breaks = seq(0, 105, by = 5),
                            right = FALSE)) %>%
        group_by(agegrp)
    }

    pop_year[[i]] <-
      pop_year[[i]] %>%
      group_by(ETH.group,
               sex,
               add = TRUE) %>%
      summarise(pop = sum(value)) %>%
      mutate(year = year_name) %>%
      ungroup()
  }

  pop_dat <- do.call(rbind, pop_year)

  if (save_to_file) {

    save_fn <-
      if(save_format == "csv") {write.csv}
    else if(save_format == "RData") {save}

    save_path <- paste0(save_folder, "/", save_name, ".", save_format)
    write.csv(pop_dat, file = save_path)
  }

  if (rtn)
    return(pop_dat)
}
