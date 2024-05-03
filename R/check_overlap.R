#' @export
check_overlap <- function(df_key ) {

  test <- df_key %>%
    dplyr::group_by(Gcluster_id, year) %>%
    dplyr::mutate(
      n_Mun = length(unique(Ccluster_id))
    ) %>%
    dplyr::ungroup()

  candidates <- unique(test$Gcluster_id[test$n_Mun > 1])

  if (length(candidates) == 0) {

    empty_df <- data.frame()
    empty_vector <- vector()

    cat("\nNo Overlapping Clusters found!\n")

    return(list(candidates = empty_vector, details = empty_df))

  } else {

  num_candidates <- length(candidates)

  test <- test %>%
    dplyr::filter(n_Mun != 1) %>%
    dplyr::select(c(Gcluster_id, Ccluster_id, Cname, year)) %>%
    dplyr::distinct(Gcluster_id, Ccluster_id , Cname, .keep_all = TRUE) %>%
    dplyr::group_by(Gcluster_id, Ccluster_id) %>% mutate(max_year = max(year)) %>% ungroup() %>%
    dplyr::filter(max_year == year) %>%
    dplyr::select(-c(year, max_year)) %>%
    dplyr::group_by(Gcluster_id) %>%
    dplyr::mutate(
      name = c(seq(1,n()))
    ) %>%
    dplyr::ungroup()

  test <- tidyr::pivot_wider(test, names_from = name, values_from = c(Ccluster_id, Cname))

  # Print the wide-format data frame
  d <- Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8") # System has to be able to read the UTF-8 characters

  cat(paste("\n",num_candidates,
  " BSU-cluster found that are spanning across the border of multiple Municipality-Clusters!", sep = ""),
  "\n\nThe following locations are affected and require manual assignment to a municipality:\n\n")

  print(test, encoding = "UTF-8")

  return(list(candidates = candidates, details = test))

  }
}

