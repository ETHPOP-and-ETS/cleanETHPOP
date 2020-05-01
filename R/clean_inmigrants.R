
#' clean_inmigrants
#'
#' @param dir_path string file address. this is either Leeds1 or Leeds2.
#' @param rtn
#' @param save_to_file
#' @param save_name
#' @param save_folder
#' @param save_format
#' @param age_grp
#'
#' @return
#' @export
#' @import dplyr, reshape2, tidyr
#' @importFrom readr read_csv
#'
#' @examples
#'   clean_inmigrants()
#'
clean_inmigrants <- function(
  dir_path,
  rtn = TRUE,
  age_grp = FALSE,
  save_to_file = TRUE,
  save_name = "clean_inmigrants",
  save_folder = here::here("output_data"),
  save_format = "csv") {

  file_names <- list.files(dir_path)

  inmigrants_year <- list()

  for (i in seq_along(file_names)) {

    dat <- read_csv(paste(dir_path, file_names[i], sep = "/"))
    year_name  <- substr(file_names[i], start = 7, stop = 10)

    inmigrants_year[[i]] <-
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
      group_by(age)

    if (age_grp) { # group ages in to 5 year ranges
      inmigrants_year[[i]] <-
        inmigrants_year[[i]] %>%
        ungroup(age) %>%
        mutate(agegrp = cut(as.numeric(age),
                            breaks = seq(0, 105, by = 5),
                            right = FALSE)) %>%
        group_by(agegrp)
    }

    inmigrants_year[[i]] <-
      inmigrants_year[[i]] %>%
      group_by(ETH.group,
               sex,
               add = TRUE) %>%
      summarise(inmigrants = sum(value)) %>%
      mutate(year = year_name) %>%
      ungroup()
  }

  inmigrants_dat <- do.call(rbind, inmigrants_year)

  if (save_to_file) {

    save_fn <-
      if(save_format == "csv") {write.csv}
    else if(save_format == "RData") {save}

    save_path <- paste0(save_folder, "/", save_name, ".", save_format)
    write.csv(inmigrants_dat, file = save_path)
  }

  if (rtn)
    return(inmigrants_dat)
}
