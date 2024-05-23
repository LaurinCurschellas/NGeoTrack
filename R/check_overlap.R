#' Identifies Overlapping Administrative Units
#'
#' @description
#' Identifies and reports which (if any) lower-level administrative units overlap the boundaries
#' of top-level administrative units.
#'
#' There identification and adjustment is crucial if the user is interested in top-level fixed effects.
#' Otherwise a BSU cluster could be assigned to 2 municipalities simoultaniously.
#'
#' The function takes any jointly estimated key from [NGeoTrack::EtE_changes()] and returns a list object.
#'
#'
#' @usage check_overlap(df_key)
#'
#' @param df_key A data.frame: Only the joint data.frame from this package can be processed.
#'
#'
#' @example man/exmpl_ovrlp.R
#'
#' @returns A 'list' object, with four 2 elements `candidates`, `details`
#' \itemize{
#'   \item `candidates` - a vector with the low-level cluster id that overlap
#'   \item `details` - a data frame with a detailed overview
#'   \itemize{
#'      \item `Gcluster_id` - low-level cluster id that overlaps
#'      \item `Ccluster_id_1` - First top-level cluster assigned to
#'      \item `Ccluster_id_2` - Second top-level cluster assigned to
#'      \item `Cname_1` - name of First top-level cluster
#'      \item `Cname_2` - name of Second top-level cluster
#'   }
#' }
#' @export
check_overlap <- function(df_key ) {

  # Recognise the supplied types:
  constr <- attributes(df_key)$comment
  components <- strsplit(constr, "-")[[1]]

  bot_type <- components[1]
  top_type <- components[2]

  # Unify the names of the columns across different types of keys
  # The names are added back in to the output at the end
  internal <- df_key
  colnames(internal) <- c("geoID", "botName", "botID", "year", "topgeoID", "topName", "topID")

  test <- internal %>%
    dplyr::group_by(botID, year) %>%
    dplyr::mutate(
      n_Top = length(unique(topID))
    ) %>%
    dplyr::ungroup()

  candidates <- unique(test$botID[test$n_Top > 1])

  if (length(candidates) == 0) {

    empty_df <- data.frame()
    empty_vector <- vector()

    cat("\nNo Overlapping Clusters found!\n")

    return(list(candidates = empty_vector, details = empty_df))

  } else {

  num_candidates <- length(candidates)

  test <- test %>%
    dplyr::filter(n_Top != 1) %>%
    dplyr::select(c(botID, topID, topName, year)) %>%
    dplyr::distinct(botID, topID , topName, .keep_all = TRUE) %>%
    dplyr::group_by(botID, topID) %>% mutate(max_year = max(year)) %>% ungroup() %>%
    dplyr::filter(max_year == year) %>%
    dplyr::select(-c(year, max_year)) %>%
    dplyr::group_by(botID) %>%
    dplyr::mutate(
      name = c(seq(1,n()))
    ) %>%
    dplyr::ungroup()

  test <- tidyr::pivot_wider(test, names_from = name, values_from = c(topID, topName))

  # Redefine the Column names of the output, based on the supplied case:
  botID <- switch(
    bot_type,
    grunnkrets = "Gcluster_id",
    kommune = "Ccluster_id"
  )

  top_prefix <- switch(
    top_type,
    kommune = "C",
    fylket = "F",
  )

  topID <- paste(top_prefix, "cluster_id", sep = "")
  topName <- paste(top_prefix, "name", sep = "")
  NewColnames <- c(botID, paste(topID, "_1", sep = ""), paste(topID, "_2", sep = ""),
                   paste(topName, "_1", sep = ""), paste(topName, "_2", sep = ""))
  colnames(test) <- NewColnames

  # Print the wide-format data frame
  d <- Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8") # System has to be able to read the UTF-8 characters

  cat(paste("\n",num_candidates,
  " BSU-cluster found that are spanning across the border of multiple Municipality-Clusters!", sep = ""),
  "\n\nThe following locations are affected and require manual assignment to a municipality:\n\n")

  print(test, encoding = "UTF-8")

  return(list(candidates = candidates, details = test))

  }
}

