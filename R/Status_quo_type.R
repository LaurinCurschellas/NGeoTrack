#' Create Panel of Valid Administrative Identifiers
#'
#' @description
#' Returns a data frame with the full panel of each years' valid administrative identifiers
#' for the chosen unit-type and time-period.
#'
#' The function calls the API of Statistics Norway (SSB) and returns the pre-processed information.
#'
#'
#' @usage status_quo(type = "kommune", from = startYear , to = endYear)
#'
#' @param type A string: `"kommune"` or `"grunnkrets"`
#' @param from An integer: Start year of panel
#' @param to   An integer: End year of panel
#'
#'
#' @examples
#' expl <- status_quo(type = "kommune", from = 1999, to = 2018)
#'
#' @returns A 'data.frame' object, with four columns `geoID`, `name`, `from`, `to`
#' \itemize{
#'   \item `geoID` - current numeric identifier
#'   \item `name` - current name of adm. unit
#'   \item `from` - startYear of validity (always year on year)
#'   \item `to` - endYear
#' }
#' @export
status_quo <- function(type ,from , to) {

  # Generate a list of all year-on-year pairings, as specified by from and to parameter
  AllPeriods <- list()

  for(year in from:to) {
    AllPeriods[[length(AllPeriods) + 1]] <- list(year, year + 1)
  }


  # Set the parameters for the URL Query & check validity of time parameters
  klass <- switch(type,
                  kommune = 131,
                  grunnkrets = 1
  )

  if (is.null(to)) {
    to <- as.integer(format(Sys.Date(), "%Y"))
  }

  if (type == "grunnkrets" & from < 1980){
    stop(simpleError("The basic statistical unit codes only traces back to 1980, consult SSB: \"https://www.ssb.no/klass/klassifikasjoner/1/versjoner\""))
  }

  if (type == "kommune" & from < 1950){
    stop(simpleError("The municipality codes only traces back to 1950, consult SSB: \"https://www.ssb.no/klass/klassifikasjoner/131/versjoner\""))
  }

  if (is.null(from)) {
    stop(simpleError("Specify a start date from 1980-present"))
  }

  Url_qry <- paste("http://data.ssb.no/api/klass/v1/classifications/",klass,"/codes.json", sep = "")



  outDAT <- data.frame( )

  for (i in seq_along(1:length(AllPeriods))) {

    # Sequence through the list of year-on-year to extract the status_quo

    dateFrom <- paste(AllPeriods[[i]][[1]],"-01-01",sep="")
    dateTo   <- paste(AllPeriods[[i]][[2]],"-01-02",sep="")


    date_qry <- list(from = dateFrom, to = dateTo)

    koReg <- httr2::request(Url_qry) |>
      httr2::req_url_query(!!!date_qry) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_perform()

    chgDT <- koReg %>% httr2::resp_body_json(simplifyDataFrame = TRUE)
    chgDT <- data.table::as.data.table(chgDT)



    # Coerce into the desired shape and encoding
    if (type == "grunnkrets") {

    chgDT <- chgDT |>
      dplyr::filter(codes.level == 2) |>
      dplyr::mutate(
        geoID = as.integer(codes.code),
        name = as.character(codes.name),
        from = as.integer(substr(codes.validFromInRequestedRange, 0 , 4)),
        to = as.integer(substr(codes.validToInRequestedRange, 0 , 4))
      ) %>%
      dplyr::select(c(geoID , name , from , to))


    outDAT <- rbind(outDAT,chgDT)

    } else if (type == "kommune") {

      chgDT <- chgDT |>
        dplyr::mutate(
          geoID = as.integer(codes.code),
          name = as.character(codes.name),
          from = as.integer(substr(codes.validFromInRequestedRange, 0 , 4)),
          to = as.integer(substr(codes.validToInRequestedRange, 0 , 4))
        ) %>%
        dplyr::select(c(geoID , name , from , to))

      outDAT <- rbind(outDAT,chgDT)
    } else {cat("No Valid Type: choose \"kommune\" or \"grunnkrets\"")}

  }


  # Ensure that each ID can only be observed once per year
  outDAT <- outDAT %>%
    dplyr::filter(from != to) |>
    dplyr::group_by(to) %>%
    dplyr::distinct(geoID, name , .keep_all = TRUE) |>
    dplyr::ungroup()

  # Ensure that Codes for "Unknown Statistical Unit" are removed
  outDAT <- outDAT |>
    dplyr::mutate(
      # Identify the Unspecified ones and remove them as they are not part of an actual cluster
      end_9 = ifelse(grepl("9999$", as.character(geoID)), 1, 0)
    ) %>%
    dplyr::filter(end_9 != 1) |>
    dplyr::select(-end_9)

  return(outDAT)

}

#
#
#
# df_changes <- gather_change(type = "grunnkrets", from = 2002, to = 2020)
#
# key_years_2002_2020 <- identifier_key %>%
#   filter(year %in% c(seq(2002,2020)))
#
# key <- EtE_changes(df_changes = df_changes , df_key = key_years_2002_2020 , from = 2002, to = 2020)
#
#
# url <- "http://data.ssb.no/api/klass/v1/classifications/1/codes.json"
# baseUrl <- paste0(url,klass)
#
#
# year_from <- 2002
# year_to <- 2020
# dateFrom <- paste(year_from,"-01-01",sep="")
# dateTo <- paste(year_to,"-02-01",sep="")
#
# ## specify query
# date_qry <- list(from = dateFrom, to = dateTo)
#
#
# koReg <- httr2::request(url) |>
#   httr2::req_url_query(!!!date_qry) |>
#   httr2::req_retry(max_tries = 5) |>
#   httr2::req_perform()
#
# chgDT <- koReg |> httr2::resp_body_json(simplifyDataFrame = TRUE)
# chgDT <- data.table::as.data.table(chgDT)
#
# chgDT <- chgDT %>%
#   filter(codes.level == "2") %>%
#   select(c(codes.code,codes.name,codes.validFromInRequestedRange)) %>%
#   mutate(
#     name = codes.name ,
#     year = as.integer(substr(codes.validFromInRequestedRange,0,4)),
#     grunnkrets = as.integer(codes.code)
#   )
#
# chgDT_1 <- chgDT %>%
#   mutate(
#     name = codes.name ,
#     year = as.integer(substr(codes.validFromInRequestedRange,0,4)),
#     grunnkrets = as.integer(codes.code)
#   ) %>%
#   select(c(name,year,grunnkrets))
#
# identifier_key_ident <- identifier_key %>%
#   filter(year == 2002) %>%
#   mutate(
#     year = as.integer(year)
#   )
#
# identical(identifier_key_ident, chgDT_1)
#
# differing_rows <- setdiff(identifier_key_ident, chgDT_1)
# nrow(differing_rows)
# identical(identifier_key_ident, chgDT_1)
#





