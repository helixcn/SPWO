# library(here)
# library(DBI)
# library(openxlsx)

#' Formatting a scientific name
#'
#' Formatting a scientific name: replacing the multiple white spaceas,
#' trim the leading the white space, capitalise the first letter
#' @param x A vector string representing the name of the family
#' @return A dataframe containing the results
#' @examples
#' form_input("   Anemone Amurensis   ")
#'
#' @export
form_input <- function(x) {
  Cap <- function(x) {
    paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), sep = "")
  }

  REPLACE <- function(x) {
    temp <- gsub(" +", " ", gsub(",+", ", ", gsub(", +", ",", x)))
    res <- gsub("^[[:space:]]+|[[:space:]]+$", "", temp)
    return(res)
  }
  x <- REPLACE(x)
  x <- Cap(x)
  return(x)
}

#' Search the WFO Database
#'
#' Search the WorldFloraOnline Database
#' @param x A vector string representing the name of the family
#' @param show_message Logical, indicating whether the validity checking should be performed and displayed
#' @return A dataframe containing the results
#' @details
#' If the input is not an accepted name, the accepted will be provided.
#'
#' @examples
#' show_detail("Tsoongiodendron odorum") # A synonym
#' show_detail("Anemone amurensis") # A synonym
#' show_detail("Machilus") # A genus
#' show_detail("Machilus gamblei") # An accepted name
#' show_detail("Machilus wangchiana") # An accepted name
#' show_detail("Persea kadooriei") # A synonym
#' show_detail("Magnolia odora") # An accepted name
#' show_detail("Magnolia odo") # Spelling error
#' show_detail(c("Magnolia odo", "aaa", "Machilus gamblei")) # Spelling error
#' show_detail("Cyclobalanopsis neglecta")
#' show_detail(c(
#'   "Cyclobalanopsis neglecta", "Lithocarpus quercifolius",
#'   "Lithocarpus quercifoli", "Apple"
#' )) # Multiple names
#' show_detail("Quercus glauca") # Multiple matches
#' @export
show_detail <- function(x = NULL, show_message = TRUE) {
  if (any(is.null(x)) | any(is.na(x)) | any(x %in% "")) {
    stop("Input name should not contain NULL, NA or ''")
  }

  YOUR_SEARCH <- form_input(x)

  ## This block keeps the order of user's input if it contains more than one name
  if (length(x) > 1) {
    temp_id <- 1:length(x)
    temp_df <- data.frame(temp_id, YOUR_SEARCH)
  }

  # Connect to DB
  con1 <- DBI::dbConnect(
    RSQLite::SQLite(),
    system.file("extdata", "WFO.db", package = "SWFO")
  )

  ### Search the input names (could be synonym)
  res0 <-
    DBI::dbSendQuery(
      con1,
      "SELECT * FROM classification WHERE scientificName in (?)",
      params = list(YOUR_SEARCH)
    )
  out0 <- DBI::dbFetch(res0)
  DBI::dbClearResult(res0)

  if (nrow(out0) > 0) {
    ###
    if (any(out0$taxonomicStatus %in% "Synonym")) {
      ### Search for the accepted names
      res1 <-
        DBI::dbSendQuery(
          con1,
          "SELECT * FROM classification WHERE taxonID in (?)",
          params = list(out0$acceptedNameUsageID)
        )

      out1 <- DBI::dbFetch(res1)
      DBI::dbClearResult(res1)
      out <- rbind(out0, out1)

      out_synnonyms <- out0[out0$taxonomicStatus %in% "Synonym", ] # Extract the synonyms
    } else {
      out <- out0
    }

    DBI::dbDisconnect(con1)

    if (show_message) {
      for (i in 1:nrow(out0)) {
        status <- out[i, "taxonomicStatus"]
        if (status %in% "Accepted") {
          message(paste(
            "Note: The accepted name for",
            paste(out[i, c("scientificName", "scientificNameAuthorship")], collapse = " "),
            "is",
            paste(out[i, c("scientificName", "scientificNameAuthorship")], collapse = " "),
            collapse = " \n"
          ))
        } else {
          message(paste(
            "Note: The accepted name for",
            paste(out[i, c("scientificName", "scientificNameAuthorship")], collapse = " "),
            "is",
            paste(paste(out[out$taxonID %in% out$acceptedNameUsageID[i], c("scientificName", "scientificNameAuthorship")]), collapse = " \n")
          ))
        }
      }
    }
  }

  if (length(x) > 1 & nrow(out0) > 0) {
    out_final <- merge(temp_df,
      out0,
      by.x = "YOUR_SEARCH",
      by.y = "scientificName",
      all = TRUE
    )
    out_final$scientificName <- out_final$YOUR_SEARCH
    out_final_sorted <- out_final[order(out_final$temp_id), ]
    out_final_sorted_reselect <- out_final_sorted[, c("YOUR_SEARCH", colnames(out))]
    return(out_final_sorted_reselect)
  } else {
    return(out0)
  }
}


#' Show synonyms of a scientific name
#'
#' Fetch synonyms of a scientific name from the WFO database
#' @param x A vector string representing the name of the family
#' @return A dataframe containing the results
#' @details
#' If the input name is not an accepted name, the accepted name will be provided.
#' Always pay attention to the Note.
#'
#' @examples
#' show_synonyms("Machilus wangchiana")
#' show_synonyms("Anemone amurensis")
#' show_synonyms("Tsoongiodendron odorum")
#' show_synonyms("Magnolia odora")
#' show_synonyms("Machilus chekiangensis")
#' show_synonyms("Lithocarpus longanoides")
#' show_synonyms("Quercus glauca")
#'
#' @export
show_synonyms <- function(x) {
  if (length(x) > 1) {
    stop("Please search one name one time")
  }
  con1 <- DBI::dbConnect(
    RSQLite::SQLite(),
    system.file("extdata", "WFO.db", package = "SWFO")
  )
  res_show_detail <- show_detail(x, show_message = FALSE)
  temp_dat <- res_show_detail[res_show_detail$taxonomicStatus %in% "Accepted", ]

  ### Search the input names (could be synonym)
  res <-
    DBI::dbSendQuery(
      con1,
      "SELECT * FROM classification WHERE acceptedNameUsageID in (?)",
      params = list(temp_dat$taxonID)
    )
  out <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  ### If nrow(out) < 1, no synonym
  ### if nrow(res_show_detail) > 0, the name is accepted and at least one entry is returned
  if (nrow(out) < 1 & nrow(res_show_detail) > 0) {
    message(
      paste(
        temp_dat$scientificName,
        temp_dat$scientificNameAuthorship,
        "is accepted. There is no synonym for it\n",
        collapse = ""
      )
    )
  }

  DBI::dbDisconnect(con1)
  return(out)
}
