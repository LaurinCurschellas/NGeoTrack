#' Create Panel of Changes to Administrative Identifiers
#'
#' @description
#' Returns a data frame with the all officially reported changes to administrative identifiers,
#' for the type and time period selected by the user.
#'
#' The function calls the API of Statistics Norway (SSB) and returns the pre-processed information.
#'
#' @usage gather_change(type , from , to )
#'
#' @param type A string: `"kommune"`, `"grunnkrets"` or `"fylket"`
#' @param from An integer: Start year of panel
#' @param to   An integer: End year of panel
#'
#' @examples
#' expl <- gather_change(type = "kommune", from = 1999, to = 2018)
#'
#' @returns A 'data.frame' object, with five columns `from`, `oldName`, `to`, `newName`, `year`
#' \itemize{
#'   \item `from` - originating numeric identifier
#'   \item `oldName` - originating name of adm. unit
#'   \item `to` - new numeric identifier
#'   \item `newName` - new name of adm. unit
#'   \item `year` - year, by which the change is enforced (01. January `year`)
#' }
#' @export
gather_change <- function(type , from , to ) {

# The class is probably defined to the values that the API recognises for each level of administrative unit.
klass <- switch(type,
                kommune = 131,
                grunnkrets = 1,
                fylket = 104
)

if (is.null(to)) {
  to <- as.integer(format(Sys.Date(), "%Y"))
}

if (type == "grunnkrets" & from < 1980){
  stop(simpleError("The basic statistical unit code only trace back to 1980, consult SSB: \"https://www.ssb.no/klass/klassifikasjoner/1/versjoner\""))
}

if (type == "kommune" & from < 1950){
  stop(simpleError("The municipality codes only trace back to 1950, consult SSB: \"https://www.ssb.no/klass/klassifikasjoner/131/versjoner\""))
}

if (type == "fylket" & from < 1842){
  stop(simpleError("The fylket codes only trace back to 1842, consult SSB: \"https://www.ssb.no/klass/klassifikasjoner/104/versjoner\""))
}

if (is.null(from)) {
  stop(simpleError("Specify a start date from 1842-present"))
}

vYear <- from:to
nYear <- length(vYear)


# The json URL that we will be looking for to track the changes is
# For municipality: "http://data.ssb.no/api/klass/v1/classifications/131/changes.json"
# For grunnkrets:   "http://data.ssb.no/api/klass/v1/classifications/1/changes.json"
Url_qry <- paste("http://data.ssb.no/api/klass/v1/classifications/",klass,"/changes.json", sep = "")


  # Needed to get the colname format we want
  # Define a function to map old column names to new column names
  map_col_names <- function(old_name) {
    switch(old_name,
           "codeChanges.oldCode" = "from",
           "codeChanges.oldName" = "oldName",
           "codeChanges.oldShortName" = "oldShortName",
           "codeChanges.newCode" = "to",
           "codeChanges.newName" = "newName",
           "codeChanges.newShortName" = "newShortName",
           "codeChanges.changeOccurred" = "year",
           old_name)
  }

  indices <- data.frame(
    V1 = seq(1,(nYear-1)),
    V2 = seq(2,nYear)
  )
  ## Create empty list - to fill the yearly changes
  listDT <- vector(mode = "list", length = nYear)

  for (i in seq_len(nrow(indices))) {

    indFrom <- indices$V1[i]
    indTo <- indices$V2[i]
    yrFrom <- vYear[indFrom]
    yrTo <- vYear[indTo]

    dateFrom <- paste(yrFrom,"-01-01",sep="")
    dateTo <- paste(yrTo,"-01-02",sep="")

    ## specify query
    date_qry <- list(from = dateFrom, to = dateTo)


    koReg <- httr2::request(Url_qry) |>
      httr2::req_url_query(!!!date_qry) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_perform()

    chgDT <- koReg |> httr2::resp_body_json(simplifyDataFrame = TRUE)
    chgDT <- data.table::as.data.table(chgDT)

    if (nrow(chgDT) != 0){
      colx <- names(chgDT)
      cols <- sapply(colx, map_col_names)
      data.table::setnames(chgDT, colx, cols)
    }

    if (nrow(chgDT) == 0) chgDT <- NULL

    listDT[[i]] <- chgDT
  }

  data_frame <- data.table::rbindlist(listDT, fill = TRUE, use.names = TRUE)

  ## need to create empty data.table when it's empty data from API
  if (nrow(data_frame) == 0) {
    colNs <- c(
      "from",
      "oldName",
      "oldShortName",
      "to",
      "newName",
      "newShortName",
      "year"
    )

    data_frame <- data.table::data.table(matrix(nrow = 0, ncol = 7))
    data.table::setnames(data_frame, new = colNs)
  }


  if (nrow(data_frame) != 0) {
    data_frame[, year := format(as.Date(year), "%Y")]
  }

  keepCol <- base::setdiff(names(data_frame) , c("oldShortName", "newShortName"))
  data_frame <- data_frame[, ..keepCol]

  data_frame[, c("from", "to", "year")] <- base::lapply(data_frame[, c("from", "to", "year")], as.integer)

  ## Assign a comment attribute to the data.frame object based on the type
  ## Will make process in EtE_changes() , easier.
  data_frame <- as.data.frame(data_frame, row.names = NULL)
  names(data_frame) <- c("from", "oldName", "to" , "newName", "year")
  attr(data_frame, "comment") <- {{type}}

  ## ------------------------- Prepare information for print -------------

  label <- switch(type,
                  kommune = "Municipality",
                  grunnkrets = "Grunnkrets (BSU)",
                  fylket = "County (\"Fylket\")"
  )

  Chg_Type <- data_frame |>
    dplyr::group_by(from, year) |>
    dplyr::mutate(
      n_from = n(),
      dist_from_from = n_distinct(from),
      dist_to_from   = n_distinct(to)
    )  |>
    dplyr::group_by(to , year) |>
    dplyr::mutate(
      n_to = n(),
      dist_from_to = n_distinct(from),
      dist_to_to = n_distinct(to)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      OtO = ifelse(n_from == 1 & n_to == 1, 1, 0),
      Split = ifelse(dist_from_from == 1 & dist_to_from > 1 , 1, 0),
      Merger = ifelse(dist_from_to  > 1 & dist_to_to == 1, 1, 0)
    ) |>
    dplyr::select(-c(n_from, dist_from_from, dist_to_from, n_to, dist_from_to, dist_to_to))



  Total_changes <- sum(sum(Chg_Type$OtO) + sum(Chg_Type$Split) + sum(Chg_Type$Merger))
  OtO_changes   <- sum(Chg_Type$OtO)
  Merger   <- sum(Chg_Type$Merger[Chg_Type$Split == 0])
  Split   <- sum(Chg_Type$Split[Chg_Type$Merger == 0])
  Split_Merge <- sum(Chg_Type$Split[Chg_Type$Merger == 1])




  ## ---------------- Return Information to Console and return data frame ------------------

  cat(paste("\nInformation on Number and Type of Changes in ", label, " Codes:", sep = ""),
      paste("\n\nThere are a total of ", Total_changes, " in the time period: ", from, " - ", to, sep = ""),
      "\n\nThese are composed of:",
      paste("\n\n\t", OtO_changes, " One-to-One Changes:",sep = ""),
      paste("\n\t", Merger, " Mergers",sep = ""),
      paste("\n\t", Split, " Splits",sep = ""),
      paste("\n\t", Split_Merge , " Split-Mergers\n\n", sep = "")
  )

  return(data_frame)

}


