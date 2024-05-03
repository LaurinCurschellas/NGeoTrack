

nrow_find <- function(df_list, max = TRUE) {

  index <- ifelse(
    max == TRUE, which.max(sapply(df_list, nrow)) ,
    which.min(sapply(df_list, nrow))
  )

  return(df_list[[index]])

}


coerce_shape <- function(df, change = TRUE) {

  if (change) {

    df <- df %>%
      dplyr::select(from, to, oldName , newName , year) %>%
      mutate(
        from = as.integer(from),
        to = as.integer(to),
        year = as.integer(year)
      )

  } else {

    df <- df %>%
      mutate(
        geoID = as.integer(geoID)
      )
  }

  return(df)
}

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
    # Coerce into form:
    df_change <- coerce_shape(df_change, change = TRUE)
    # Filter based on paramter 'to' and NOT the variable 'to'
    df_status <- coerce_shape(df_status, change = FALSE) %>%
      filter(from <= ({{to}}-1))

    # Handle Case of no changes - in such a period the key is equal to the status
    if (nrow(df_change) != 0) {

      # Use the undirected networks to identify the clusters of mergers/splits
      graph <- graph_from_data_frame(df_change, directed = FALSE , vertices = NULL)
      clusters <- igraph::components(graph, mode = c("strong"))


      # Extract all the clusters formed over the time period and the name of the nodes
      # Update this iterations' changes with the cluster id and unify format
      # Y: Cluster_id (allows for up to 99 thousand changes))

      df_members <- data.frame(
        name = as.integer(V(graph)$name),
        cluster = clusters$membership,
        row.names = NULL
      ) %>%
        mutate(
          cluster_id = as.integer(1*10^5+cluster)
        ) %>%
        dplyr::select(-cluster) %>%
        distinct()



      EtE_key <- left_join(df_status , df_members, join_by("geoID" == "name"))
      EtE_key <- EtE_key %>%
        mutate(
          cluster_id = ifelse(is.na(cluster_id) , geoID , cluster_id ),
          year = from
        ) %>%
        dplyr::select(-c(from,to))

    } else {

      # This else statement refers to an empty df_change dataframe, in periods with no change
      EtE_key <- df_status %>%
        mutate(
          geoID = geoID,
          name = name,
          cluster_id = geoID,
          year = from
        )%>%
        dplyr::select(-c(from,to))

    }

    EtE_key <- EtE_key %>%
      arrange(year)

  }

  if (jointly) {

    # Extract from supplied list (municipal data is always smaller than the grunnkrets) and coerse into shape
    df_change_bsu <- coerce_shape(nrow_find(df_change , max = TRUE), change = TRUE)
    df_change_mun <- coerce_shape(nrow_find(df_change , max = FALSE), change = TRUE)
    df_status_bsu <- coerce_shape(nrow_find(df_status , max = TRUE), change = FALSE) %>%
      filter(from <= ({{to}}-1))
    df_status_mun <- coerce_shape(nrow_find(df_status , max = FALSE), change = FALSE) %>%
      filter(from <= ({{to}}-1))


    # Handle period of no change in either BSU or municipality codes
    # Have to be handled seperately
    if (nrow(df_change_bsu) == 0) {

      EtE_key_bsu <- df_status_bsu %>%
        mutate(
          geoID = geoID,
          # Name and code have to be labelled for grunnkrets and municipalities
          Gname = name,
          Gcluster_id = geoID,
          year = from
        )%>%
        dplyr::select(-c(from,to))

    } else {

      # Use the undirected networks to identify the clusters of mergers/splits
      graph_bsu <- graph_from_data_frame(df_change_bsu, directed = FALSE , vertices = NULL)
      clusters_bsu <- igraph::components(graph_bsu, mode = c("strong"))

      df_members_bsu <- data.frame(
        Gname = as.integer(V(graph_bsu)$name),
        cluster = clusters_bsu$membership,
        row.names = NULL
      ) %>%
        mutate(
          Gcluster_id = as.integer(1*10^5+cluster)
        ) %>%
        dplyr::select(-cluster) %>%
        distinct()

      EtE_key_bsu <- left_join(df_status_bsu , df_members_bsu, join_by("geoID" == "Gname"))
      EtE_key_bsu <- EtE_key_bsu %>%
        mutate(
          Gcluster_id = ifelse(is.na(Gcluster_id) , geoID , Gcluster_id ),
          Gname = name,
          year = from
        ) %>%
        dplyr::select(-c(from,to,name))
    }

    if (nrow(df_change_mun) == 0) {

      EtE_key_mun <- df_status_mun %>%
        mutate(
          geoID = geoID,
          Cname = name,
          Ccluster_id = geoID,
          year = from
        )%>%
        dplyr::select(-c(from,to))

    } else {

      graph_mun <- graph_from_data_frame(df_change_mun, directed = FALSE , vertices = NULL)
      clusters_mun <- igraph::components(graph_mun, mode = c("strong"))


      # Extract all the clusters formed over the time period and the name of the nodes
      # Update this iterations' changes with the cluster id and unify format
      # Y: Cluster_id (allows for up to 99 thousand changes))

      df_members_mun <- data.frame(
        Cname = as.integer(V(graph_mun)$name),
        cluster = clusters_mun$membership,
        row.names = NULL
      ) %>%
        mutate(
          Ccluster_id = as.integer(5*10^5+cluster)
        ) %>%
        dplyr::select(-cluster) %>%
        distinct()

      EtE_key_mun <- left_join(df_status_mun , df_members_mun, join_by("geoID" == "Cname"))
      EtE_key_mun <- EtE_key_mun %>%
        mutate(
          Ccluster_id = ifelse(is.na(Ccluster_id) , geoID , Ccluster_id ),
          Cname = name,
          year = from
        ) %>%
        dplyr::select(-c(from,to,name))

    }

    ## Merge the two keys into one joint key

    EtE_key_bsu <- EtE_key_bsu %>%
      mutate(
        str_l = as.integer(nchar(geoID)),
        # Municipality id are either the first 3, or 4 digits depending on the length
        # of the municipality code (and thus the length of the entire code)
        mun_id = ifelse(str_l != 7 , as.integer(substr(geoID, 0, 4)),
                        as.integer(substr(geoID, 0, 3)))
      ) %>%
      dplyr::select(-c(str_l))


    EtE_key <- left_join(EtE_key_bsu , EtE_key_mun , join_by("mun_id" == "geoID", "year" == "year"))


  }

  EtE_key <- EtE_key %>%
    arrange(year)

  return(EtE_key)

}
