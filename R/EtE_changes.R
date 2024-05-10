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
#' @usage EtE_changes(df_status = x, df_change = y , from = startYear, to = endYear, jointly = FALSE)
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

    # Extract from supplied list (municipal data is always smaller than the grunnkrets) and coerse into shape
    df_change_bsu <- NGeoTrack::coerce_shape(NGeoTrack::nrow_find(df_change , max = TRUE), change = TRUE)
    df_change_mun <- NGeoTrack::coerce_shape(NGeoTrack::nrow_find(df_change , max = FALSE), change = TRUE)
    df_status_bsu <- NGeoTrack::coerce_shape(NGeoTrack::nrow_find(df_status , max = TRUE), change = FALSE) |>
      dplyr::filter(from <= ({{to}}-1))
    df_status_mun <- NGeoTrack::coerce_shape(NGeoTrack::nrow_find(df_status , max = FALSE), change = FALSE) |>
      dplyr::filter(from <= ({{to}}-1))


    # Handle period of no change in either BSU or municipality codes
    # Have to be handled seperately
    if (nrow(df_change_bsu) == 0) {

      EtE_key_bsu <- df_status_bsu |>
        dplyr::mutate(
          geoID = geoID,
          # Name and code have to be labelled for grunnkrets and municipalities
          Gname = name,
          Gcluster_id = geoID,
          year = from
        )|>
        dplyr::select(-c(from,to))

    } else {

      # Use the undirected networks to identify the clusters of mergers/splits
      graph_bsu <- igraph::graph_from_data_frame(df_change_bsu, directed = FALSE , vertices = NULL)
      clusters_bsu <- igraph::components(graph_bsu, mode = c("strong"))

      df_members_bsu <- data.frame(
        Gname = as.integer(igraph::V(graph_bsu)$name),
        cluster = clusters_bsu$membership,
        row.names = NULL
      ) |>
        dplyr::mutate(
          Gcluster_id = as.integer(1*10^5+cluster)
        ) |>
        dplyr::select(-cluster) |>
        dplyr::distinct()

      EtE_key_bsu <- dplyr::left_join(df_status_bsu , df_members_bsu, dplyr::join_by("geoID" == "Gname"))
      EtE_key_bsu <- EtE_key_bsu |>
        dplyr::mutate(
          Gcluster_id = ifelse(is.na(Gcluster_id) , geoID , Gcluster_id ),
          Gname = name,
          year = from
        ) |>
        dplyr::select(-c(from,to,name))
    }

    if (nrow(df_change_mun) == 0) {

      EtE_key_mun <- df_status_mun |>
        dplyr::mutate(
          geoID = geoID,
          Cname = name,
          Ccluster_id = geoID,
          year = from
        )|>
        dplyr::select(-c(from,to))

    } else {

      graph_mun <- igraph::graph_from_data_frame(df_change_mun, directed = FALSE , vertices = NULL)
      clusters_mun <- igraph::components(graph_mun, mode = c("strong"))


      # Extract all the clusters formed over the time period and the name of the nodes
      # Update this iterations' changes with the cluster id and unify format
      # Y: Cluster_id (allows for up to 99 thousand changes))

      df_members_mun <- data.frame(
        Cname = as.integer(igraph::V(graph_mun)$name),
        cluster = clusters_mun$membership,
        row.names = NULL
      ) |>
        dplyr::mutate(
          Ccluster_id = as.integer(5*10^5+cluster)
        ) |>
        dplyr::select(-cluster) |>
        dplyr::distinct()

      EtE_key_mun <- dplyr::left_join(df_status_mun , df_members_mun, dplyr::join_by("geoID" == "Cname"))
      EtE_key_mun <- EtE_key_mun |>
        dplyr::mutate(
          Ccluster_id = ifelse(is.na(Ccluster_id) , geoID , Ccluster_id ),
          Cname = name,
          year = from
        ) |>
        dplyr::select(-c(from,to,name))

    }

    ## Merge the two keys into one joint key

    EtE_key_bsu <- EtE_key_bsu |>
      dplyr::mutate(
        str_l = as.integer(nchar(geoID)),
        # Municipality id are either the first 3, or 4 digits depending on the length
        # of the municipality code (and thus the length of the entire code)
        mun_id = ifelse(str_l != 7 , as.integer(substr(geoID, 0, 4)),
                        as.integer(substr(geoID, 0, 3)))
      ) |>
      dplyr::select(-c(str_l))


    EtE_key <- dplyr::left_join(EtE_key_bsu , EtE_key_mun , dplyr::join_by("mun_id" == "geoID", "year" == "year"))


  }

  EtE_key <- EtE_key |>
    dplyr::arrange(year)

  return(EtE_key)

}
