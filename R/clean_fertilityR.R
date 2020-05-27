
#' clean_fertility
#'
#' @param dir_path raw data location
#' @param rtn TRUE/FALSE
#' @param save_to_file TRUE/FALSE
#' @param save_name string
#' @param save_folder output data location
#' @param save_format csv or RData
#'
#' @return
#' @export
#' @import dplyr, purrr, reshape
#' @importFrom readr read_csv
#' @examples
#'   clean_fertility()
#'
clean_fertility <- function(dir_path = "D:/data/ETHPOP/Leeds2/Fertility",
                            m_to_100_f = 105,
                            rtn = TRUE,
                            save_to_file = TRUE,
                            save_name = "clean_fertility",
                            save_folder = here::here("output_data"),
                            save_format = "csv") {

  pop_dir = system.file("extdata", "Leeds1", "Population",
                        package = "cleanETHPOP")

  ffile_names <- list.files(dir_path)
  pfile_names <- list.files(pop_dir)

  fertility_year <- list()

  for (i in seq_along(file_names)) {

    fdat <- read_csv(paste(dir_path, ffile_names[i], sep = "/"))
    pdat <- read_csv(paste(pop_dir, pfile_names[i], sep = "/"))

    year_name <- substr(file_names[i], start = 10, stop = 13)

    fdat <-
      fdat %>%
      select_at(vars(-contains("X1"))) %>%
      mutate(M0.1 = M0.1 + MB.0,
             F0.1 = F0.1 + FB.0) %>%
      select(-MB.0, -FB.0)

    pdat <-
      pdat %>%
      select_at(vars(-contains("X1")))

    # from per-capita to counts
    births_dat <- keep(fdat, is.numeric) * keep(pdat, is.numeric)

    fertility_year[[i]] <-
      births_dat %>%
      mutate(LAD.name = fdat$LAD.name,
             LAD.code = fdat$LAD.code,
             ETH.group = fdat$ETH.group) %>%
      filter(grepl('E', LAD.code)) %>%
      group_by(ETH.group) %>%
      summarise_at(.vars = vars(M0.1:F100.101p),
                   sum) %>%
      melt(id.vars = "ETH.group",
           value.name = "person") %>%
      separate(variable, c("age", NA)) %>%
      separate(age, c("sex", "age"), sep = 1) %>%
      mutate(age = gsub("p", "", age)) %>%
      filter(sex == "F") %>%
      select(-sex) %>%
      mutate(`M` = person*m_to_100_f/(m_to_100_f + 100),
             `F` = person*100/(m_to_100_f + 100)) %>%
      melt(measure.vars = c("person", "M", "F"),
           value.name = "births",
           variable.name = "sex") %>%
      mutate(year = year_name)
  }

  fertility_dat <-
    do.call(rbind, fertility_year)

  if (save_to_file) {

    save_fn <-
      if(save_format == "csv") {write.csv}
    else if(save_format == "RData") {save}

    save_path <- paste0(save_folder, "/", save_name, ".", save_format)
    write.csv(fertility_dat, file = save_path)
  }

  if (rtn)
    as_tibble(fertility_dat)
}
