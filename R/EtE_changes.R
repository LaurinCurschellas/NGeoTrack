#' Creates Key of Harmonized Adm. Units
#'
#' @description
#' Takes the data frames created by [status_quo()] and [gather_change()], returns a key that maps each identifier
#' to their traceable cluster.
#'
#' Clusters are defined as the least common unit which can be uniquely distinguished at any given point in time
#' from all other adm. units.
#'
#' Keys can be generated for each type of adm. unit independently or jointly. The use-case for joint harmonization
#' can be the need to include fixed-effects of the top-level administrative unit.
#'
#'
#'
#' @usage EtE_changes(df_status, df_change, from , to , jointly = FALSE)
#'
#'
#' @param df_status A data object: Panel of status (object of type: `data.frame` or `list`)
#' @param df_change A data object: Panel of changes (object of type: `data.frame` or `list`)
#' @param from   An integer: Start year of panel
#' @param to   An integer: End year of panel
#' @param jointly   A Boolean: Request of harmonizing the types of adm. units jointly. Default `FALSE`.
#' Important Note: If ` jointly == TRUE ` status and change information have to be supplied as a list.
#' The function is agnostic to the ordering of the elements in the list.
#'
#'
#' @example man/exmpl_EtE.R
#'
#' @returns A `data.frame` object with 4 or 7 (joint harmonization) columns. Allows for a 1:1 mapping of all active identifiers in a year to their cluster.
#' \itemize{
#'   \item `geoID` - original numeric identifier
#'   \item `name` - name of adm. unit
#'   \item `cluster_id` - cluster identifier the unit is assigned to
#'   \item `year` - year of the mapping
#' }
#' For joint harmonization the lower level adm. unit is mapped to corresponding
#' \itemize{
#'  \item `Gcluster_id` - same-level cluster identifier
#'  \item `Ccluster_id` - top-level cluster identifier
#' }
#'
#' @export
EtE_changes <- function(df_status ,df_change ,from , to, jointly = FALSE) {


  # Ensure User supplies the data and runs jointly with intent:
  if((is.data.frame(df_status) || is.data.frame(df_change)) && jointly) {
    cat("\nIf the user wants to establish tractability of 'kommmuner' and 'grunnkrets' jointly,",
        "\nthe data needs to be supplied as a list.",
        "\n\nFor example:",
        "\n\n\tdf_status <- list(df_status_grunnkrets, df_status_municipality)\n\n")
    stop("Data not supplied as a list. Function execution halted.")
  }

  if((!is.data.frame(df_status) || !is.data.frame(df_change)) && !jointly) {
    cat("\nIf the user wants to establish tractability of 'kommmuner' and 'grunnkrets' jointly,",
        "\nthe option 'jointly' has to be turned on",
        "\n\nFor example:",
        "\n\n\tdf_key <- EtE_changes(df_status_list, df_change_list, from = 1990, to = 2022, jointly = TRUE)\n\n")
    stop("Data supplied as list, but 'jointly' option negated. Function execution halted.")
  }


  # Handle case of no date supplied
  if (is.null(from))
    from <- as.integer(format(Sys.Date(), "%Y"))

  if (is.null(to))
    to <- as.integer(format(Sys.Date(), "%Y"))




  # Individual Case -----------------------------------------------
  if (!jointly) {

    # Reveal the supplied type
    #
    # If type unrecognised, error message with fix suggestion, if different type error message
    # Else parameter 'type' defined.
    type_chg <- attributes(df_change)$comment
    type_stat <- attributes(df_status)$comment

    if (is.null(type_stat) | is.null(type_chg) ){
      stop(simpleError(paste("\nThe type cannot be recognised, check the data frame attributes:",
                           "\n\nExample: \t'attributes(df_change)$comment'",
                        "\n\nSupply the correct type with 'attr(df_change, \"comment\") <- \"kommune\"")))
    } else if (type_stat == type_chg) {
      type <- type_stat
      rm(type_chg, type_stat)
    } else {stop(simpleError("\nThe supplied data is not of the same type ('grunnkrets','kommune' or 'fylket')")) }

    # Restrict time period of the status to match the function definition
    df_status <- df_status[df_status$from <= ({{to}} - 1), ]
    # Re-order the df_change, such that the network is identified on from and to
    df_change <- df_change[, c("from", "to", "oldName", "newName", "year")]


    # Handle Case of no changes - in such a period the key is equal to the status
    if (nrow(df_change) != 0) {

      # Use the undirected networks to identify the clusters of mergers/splits
      graph <- igraph::graph_from_data_frame(df_change, directed = FALSE , vertices = NULL)
      clusters <- igraph::components(graph, mode = c("strong"))


      # Extract all the clusters formed over the time period and the name of the nodes
      # Update this iterations' changes with the cluster id and unify format
      # Y: Cluster_id (allows for up to 99 thousand changes))

      df_members <- data.frame(
        name = as.integer(igraph::V(graph)$name),
        cluster_id = as.integer(1*10^5+clusters$membership),
        row.names = NULL
      )

      dupl <- duplicated(df_members[, c("name", "cluster_id")])
      df_members <- df_members[!dupl, ]

      EtE_key <- dplyr::left_join(df_status , df_members, dplyr::join_by("geoID" == "name"))
      EtE_key <- EtE_key |>
        dplyr::mutate(
          cluster_id = ifelse(is.na(cluster_id) , geoID , cluster_id ),
          year = from
        ) |>
        dplyr::select(-c(from,to))

    } else {

      # This else statement refers to an empty df_change dataframe, in periods with no change
      EtE_key <- data.frame(
        geoID = df_status$geoID,
        name  = df_status$name,
        cluster_id = df_status$geoID,
        year = df_status$from
      )

    }

    # Type specific Column name for the cluster
      colType <- switch (type,
        grunnkrets = "Gcluster_id",
        kommune    = "Ccluster_id",
        fylket     = "Fcluster_id"
      )

      names(EtE_key) <- c("geoID", "name", colType, "year")
    # Indicate the harmonization level in attr(df_key, "comment")
      comment(EtE_key) <- paste("Harmonized '", type ,"'", sep = "")

    EtE_key <- EtE_key |>
      dplyr::arrange(year)

  }

  if (jointly) {

    # --------- Define Helper Function to extract the Correct bot- and top-level type

    identify_hierarchy <- function(list_in) {

      # Check the validity of the supplied datasets
      type_pos1 <- attr(list_in[[1]], "comment")
      type_pos2 <- attr(list_in[[2]], "comment")

      if (is.null(type_pos1) | is.null(type_pos1) ) {
        stop(simpleError(paste("\nThe type cannot be recognised, check the data frame attributes inside the list:",
                               "\n\nExample: \t'attributes(df_change)$comment'",
                               "\n\nSupply the correct type with 'attr(df_change, \"comment\") <- \"kommune\"")))
      } else if (type_pos1 == type_pos2) {
        stop(simpleError(paste("\nThe list supplied does not contain unique types")))
      }


      # Extract the type from list -one of them will be empty
      grunnkrets_type <- Filter(function(x) attr(x, "comment") == "grunnkrets", list_in)
      kommune_type <- Filter(function(x) attr(x, "comment") == "kommune", list_in)
      fylket_type <- Filter(function(x) attr(x, "comment") == "fylket", list_in)

      # Order the list according to hierarchy
      ordered_list <- list(grunnkrets_type, kommune_type, fylket_type)
      # Indicies of the non-empty list
      indices <- which(sapply(ordered_list, length) > 0)

      # Assign first non-empty to bot_level , second non-empty to top_level
      bot_level <- ordered_list[[indices[1]]]
      top_level <- ordered_list[[indices[2]]]

      bot_level <- bot_level[[1]]
      top_level <- top_level[[1]]

      list_out <- list()

      list_out[["bot_level"]] <- bot_level
      list_out[["top_level"]] <- top_level

      return(list_out)

    }

    # Order the incoming list and extract the type from the attributes table of the data frame
    ordered_chg <- identify_hierarchy(df_change)
    ordered_stat <- identify_hierarchy(df_status)

    df_chg_bot <- ordered_chg[["bot_level"]]
    df_chg_top <- ordered_chg[["top_level"]]

    df_stat_bot <- ordered_stat[["bot_level"]]
    df_stat_top <- ordered_stat[["top_level"]]

    # Reveal type:
    if( attr(df_chg_bot, "comment") == attr(df_stat_bot, "comment") ) {
      bot_type <- attr(df_chg_bot, "comment")
    } else {stop(simpleError("Mismatch of type supplied in status list and change list!"))}

    if( attr(df_chg_top, "comment") == attr(df_stat_top, "comment") ) {
      top_type <- attr(df_chg_top, "comment")
    } else {stop(simpleError("Mismatch of type supplied in status list and change list!"))}

    # Type-specific column name for cluster_id and name columns
    clstr_col_bot <- switch (bot_type,
                           grunnkrets = "Gcluster_id",
                           kommune    = "Ccluster_id"
    )
    name_col_bot <- switch (bot_type,
                             grunnkrets = "Gname",
                             kommune    = "Cname"
    )
    clstr_col_top <- switch (top_type,
                       kommune    = "Ccluster_id",
                       fylket     = "Fcluster_id"
    )
    name_col_top <- switch (top_type,
                             kommune    = "Cname",
                             fylket     = "Fname"
    )

    # Restrict time period of the status to match the function definition
    df_stat_bot <- df_stat_bot[df_stat_bot$from <= ({{to}} - 1), ]
    df_stat_top <- df_stat_top[df_stat_top$from <= ({{to}} - 1), ]
    # Order the change data
    df_chg_bot <- df_chg_bot[, c("from", "to", "oldName", "newName", "year")]
    df_chg_top <- df_chg_top[, c("from", "to", "oldName", "newName", "year")]

    # Handle period of no change in either BSU or municipality codes
    # Have to be handled seperately
    if (nrow(df_chg_bot) == 0) {

      EtE_key_bot <- data.frame(
        geoID = df_stat_bot$geoID,
        name  = df_stat_bot$name,
        cluster_id = df_stat_bot$geoID,
        year = df_stat_bot$from
      )

    } else {

      # Use the undirected networks to identify the clusters of mergers/splits
      graph_bot <- igraph::graph_from_data_frame(df_chg_bot, directed = FALSE , vertices = NULL)
      clusters_bot <- igraph::components(graph_bot, mode = c("strong"))

      df_members_bot <- data.frame(
        name = as.integer(igraph::V(graph_bot)$name),
        cluster_id = as.integer(1*10^5+clusters_bot$membership),
        row.names = NULL
      )
      # Remove the duplicated rows
      dupl <- duplicated(df_members_bot[, c("name", "cluster_id")])
      df_members_bot <- df_members_bot[!dupl, ]

      EtE_key_bot <- dplyr::left_join(df_stat_bot , df_members_bot, dplyr::join_by("geoID" == "name"))
      EtE_key_bot <- EtE_key_bot |>
        dplyr::mutate(
          cluster_id = ifelse(is.na(cluster_id) , geoID , cluster_id ),
          name = name,
          year = from
        ) |>
        dplyr::select(-c(from,to))
    }

    if (nrow(df_chg_top) == 0) {

       EtE_key_top <- data.frame(
          geoID = df_stat_top$geoID,
          name  = df_stat_top$name,
          cluster_id = df_stat_top$geoID,
          year = df_stat_top$from
        )

    } else {

      graph_top <- igraph::graph_from_data_frame(df_change_top, directed = FALSE , vertices = NULL)
      clusters_top <- igraph::components(graph_top, mode = c("strong"))

      # Extract all the clusters formed over the time period and the name of the nodes
      # Update this iterations' changes with the cluster id and unify format
      # Y: Cluster_id (allows for up to 99 thousand changes))

      df_members_top <- data.frame(
        name = as.integer(igraph::V(graph_top)$name),
        cluster_id = as.integer(1*10^5+clusters_top$membership),
        row.names = NULL
      )

      dupl <- duplicated(df_members_top[, c("name", "cluster_id")])
      df_members_top <- df_members_top[!dupl, ]

      EtE_key_top <- dplyr::left_join(df_stat_top , df_members_top, dplyr::join_by("geoID" == "name"))
      EtE_key_top <- EtE_key_top |>
        dplyr::mutate(
          cluster_id = ifelse(is.na(cluster_id) , geoID , cluster_id ),
          name = name,
          year = from
        ) |>
        dplyr::select(-c(from,to))
    }

    names(EtE_key_bot) <- c("geoID", name_col_bot, clstr_col_bot, "year")
    names(EtE_key_top) <- c("geoID", name_col_top, clstr_col_top, "year")

    ## Merge the two keys into one joint key on a case-by-case basis
    # Case 1 - Bot: Grunnkrets  , Top: Fylket
    # Case 2 - Bot: Grunnkrets  , Top: Kommune
    # Case 3 - Bot: Kommune     , Top: Fylket

    if (bot_type == "grunnkrets" & top_type == "fylket") {

      EtE_key_bot <- EtE_key_bot |>
        dplyr::mutate(
          str_l = as.integer(nchar(geoID)),
          # County id are either the first 1, or 2 digits depending on the length
          # of the municipality code (and thus the length of the entire grunnkrets code)
          flk_id = ifelse(str_l != 7 , as.integer(substr(geoID, 0, 2)),
                          as.integer(substr(geoID, 0, 1)))
        ) |>
        dplyr::select(-c(str_l))

      EtE_key <- dplyr::left_join(EtE_key_bot , EtE_key_top , dplyr::join_by("flk_id" == "geoID", "year" == "year"))

      comment(EtE_key) <- "grunnkrets-fylket"

    } else if (bot_type == "grunnkrets" & top_type == "kommune") {

      EtE_key_bot <- EtE_key_bot |>
        dplyr::mutate(
          str_l = as.integer(nchar(geoID)),
          # Municipality id are either the first 3, or 4 digits depending on the length
          # of the municipality code (and thus the length of the entire grunnkrets code)
          mun_id = ifelse(str_l != 7 , as.integer(substr(geoID, 0, 4)),
                          as.integer(substr(geoID, 0, 3)))
        ) |>
        dplyr::select(-c(str_l))


      EtE_key <- dplyr::left_join(EtE_key_bot , EtE_key_top , dplyr::join_by("mun_id" == "geoID", "year" == "year"))

      comment(EtE_key) <- "grunnkrets-kommune"

    } else if (bot_type == "kommune" & top_type == "fylket") {

      EtE_key_bot <- EtE_key_bot |>
        dplyr::mutate(
          str_l = as.integer(nchar(geoID)),
          # Municipality id are either the first 1, or 2 digits depending on the length
          # of the municipality code
          flk_id = ifelse(str_l != 3 , as.integer(substr(geoID, 0, 2)),
                          as.integer(substr(geoID, 0, 1)))
        ) |>
        dplyr::select(-c(str_l))

      EtE_key <- dplyr::left_join(EtE_key_bot , EtE_key_top , dplyr::join_by("flk_id" == "geoID", "year" == "year"))

      comment(EtE_key) <- "kommune-fylket"

    } else {stop(simpleError("Mismatch of type supplied in status list and change list!"))}
  }

  EtE_key <- EtE_key |>
    dplyr::arrange(year)

  return(EtE_key)

}
