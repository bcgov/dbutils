#### conversion functions ----


#### conversionTables ----
#' Pull list of conversion tables
#'
#' @description
#' Pull a list of all conversion tables. Conversion tables represent the proportional relationship
#' ("split") between source and destination geographies, and are named as \strong{Table-FROM-TO-YEAR}.
#' For example, the table that converts from LHA to SD based on 2019 matches is called Table-LHA-SD-2019.
#' (Historical APL naming was similar, but had the word TABLE before the year (e.g., LHA-SD-TABLE2019).)
#'
#' Conversion tables are in the "I:/ConversionTables/" folder, to ensure consistency across
#' population systems, and because \code{\link{dbConvert}} uses that hardcoded path to find the
#' required conversion table. Conversion tables are expected to be .xlsx files with three unnamed
#' columns with the proportional relationship (aka, "split"), source and destination geographies.
#'
#' @param geog A character vector of one or more geographies to list. Default = NULL. If NULL, all
#' conversion tables will be listed. If not NULL, only those conversion tables with that geog(s)
#' as either Source or Destination geography will be listed.
#' @param year A vector of one or more years of conversion tables to list. Default = NULL. If NULL,
#' all conversion tables will be listed. If not NULL, only those conversion tables of that Year(s)
#' will be listed.
#' @return A data.frame object with variables: Table, Source, Destination, and Year.
#' @examples
#' conversionTables()                          ## lists all conversion tables
#' conversionTables(geog = NULL, year = NULL)  ## lists all conversion tables
#' conversionTables(geog = c("RD", "HA"))      ## lists only select conversion tables
#' conversionTables(year = "2020")             ## lists only 2020 conversion tables
#' conversionTables(year = c(2020, 2021))      ## lists only select conversion tables
#' @family conversion helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @export
conversionTables <- function(geog = NULL, year = NULL) {

  ## list of all files (with "Table-" in name) within ConversionTables folder
  tbls <- list.files(dbPaths$conv_tbl_path, pattern = "Table")
  tables <- stringr::str_split(tbls, pattern = "-", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(Table = tbls,
                  Year = stringr::str_sub(X4, end = stringr::str_locate(X4, pattern = "[.]")[,"start"]-1)) %>%
    dplyr::select(Table, Source = X2, Destination = X3, Year); rm(tbls)

  ## if subset of geographies (i.e., "geog" argument not NULL)
  if(!is.null(geog)) {
    if(length(geog) > 1) {
      temp <- tables %>% dplyr::filter(is.na(Table))
      for(i in seq_along(geog)) {
        temp <- temp %>%
          dplyr::bind_rows(dplyr::filter(tables, stringr::str_detect(Table, pattern = paste0("-", geog[i]))))
      }
      tables <- temp; rm(temp, i)
    } else {
      tables <- tables %>% dplyr::filter(stringr::str_detect(Table, pattern = paste0("-", geog)))
    }
  }

  ## if subset of years (i.e., "year" argument not NULL)
  if(!is.null(year)) {
    tables <- tables %>% dplyr::filter(Year %in% year)
  }

  return(tables)

}


#### conversionRead ----
#' Read population conversion table
#'
#' @description
#' Read a conversion table so it can be examined. Conversion tables represent the proportional
#' relationship ("split") between source and destination geographies, and are named as \strong{FROM_TO_YEAR}.
#' For example, the table that converts from LHA to SD based on 2019 matches is called LHA_SD_2019.
#' (Historical APL naming was similar, but used dashes in place of underscores, and included the word
#' TABLE before the year (e.g., LHA-SD-TABLE2019).)
#'
#' Conversion tables are in the "I:/ConversionTables/" folder, to ensure consistency across
#' population systems, and because \code{\link{dbConvert}} uses that hardcoded path to find the
#' required conversion table. Conversion tables are expected to be .xlsx files with three unnamed
#' columns with the proportional relationship (aka, "split"), source and destination geographies.
#'
#' @details
#' Data is available at varying levels of detail, mostly dependent on some form of geography. For
#' example, Census-related data may be accessible for every Dissemination Block, while
#' health-related data may only be available at the Local Health Area (LHA) level. As such,
#' there exists a need to transfer, or convert, information from one geography to another. This is
#' achieved using a conversion table that converts data from Geography A (source) to B (destination).
#'
#' A conversion table is based on population counts at a very high level of detail. BC Stats uses
#' Dissemination Block-level population totals from the most recent Census. Additionally, one would
#' need geographic software (like \href{http://desktop.arcgis.com/en/arcmap/}{ArcMap} or
#' \href{http://qgis.org/en/site/}{QGIS}) to establish the allocation of Dissemination Blocks to the
#' conversion geographies. While the following mentions the underlying linkage between geographies
#' being established via Dissemination Blocks, one could technically use any (small) geography to
#' establish this relationship.
#'
#' Essentially, the (total) population at the Dissemination Block-level is required to provide the
#' fundamental relationship between the allocation of Geography A (source) and the allocation to
#' Geography B (destination).
#'
#' @param conv_path The full path to the conversion table Excel file.
#' @return A data.frame object with three variables: split, source, and destination.
#' @examples
#' \dontrun{  conversionRead(conv_path = "I:/ConversionTables/Table-LHA-HSDA-2019.xlsx")  }
#' \dontrun{  conversionRead(conv_path = "I:/ConversionTables/Table-LHA-SD-2019.xlsx")  }
#' @family conversion helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @export
conversionRead <- function(conv_path) {

  if(length(conv_path) == 1) { # User entered full path
    tbl <- openxlsx::readWorkbook(xlsxFile = conv_path, colNames = FALSE)
  } else {
    stop("Error: Incorrect conv_path format. Enter full path name.")
  }

  if(dim(tbl)[2] == 3 ) {
    names(tbl) <- c("split", "source", "destination")
  } else {
    stop("Error: Database does not have the correct number (3) of columns.")
  }

  if(class(tbl$split) != "numeric") {
    stop("Error: Database column, 'split', must be numeric.")
  }

  return(tbl)

}


#### dbConvert ----
#' Convert population database
#'
#' @description
#' Converts a population database to another geography using a conversion table and, possibly,
#' raking.
#'
#' @details
#' Conversion tables should be in the "I:/ConversionTables/" folder so that they can be found, and
#' are expected to be .xlsx files with three unnamed columns with the proportional relationship
#' (aka, "split"), source and destination geographies.
#'
#' The conversion process is as follows:
#' \enumerate{
#'  \item Evaluate the conversion table between Geography A (source)and B (destination) for regions
#'  that require no conversion (i.e., regions are converted 1-to-1 from Geography A to B). Remove
#'  these regions from the conversion process.
#'  \item Sequentially share out the data relating to the region in Geography A using the
#'  conversion factor to the respective region(s) in Geography B.
#'  \item Aggregate Geography B components that received contributions from multiple regions in
#'  Geography A.
#'  \item If not all regions are 1-to-1, "split" destination geographies must conform to source
#'  totals (aka, "control population totals"), achieved using raking (\code{\link{dbRake}}).
#'  If Step (1) removed some 1-to-1 regions, the control totals will be adjusted downward. Destination
#'  control totals (aka, "control region totals", or dbRake's "CtrlRegionTotals") are set as NULL and
#'  will be generated during the raking process from the converted destination data (aka, "InputData").
#'  \item Any 1-to-1 regions removed during Step (1) are added back to Geography B (under the
#'  Geography B naming convention).
#' }
#'
#' @param db Data variable containing the source database to be converted. Expects data to be in
#' data.frame with columns: Year, Type, TypeID, Age, Male, Female, Total.
#' @param conv_table Name of conversion table to be read in to provide the proportional
#' relationship ("split") between source and destination geographies. Conversion table path is
#' hardcoded to ensure consistency across population systems, and will find conversion table in
#' "I:/ConversionTables/" folder.
#' @param years Vector of one or more years to be included in the converted destination data.
#' Default = NULL. If NULL, the function will convert for all years in the source data.
#' @param rake Whether raking is required (default) or not. Default = TRUE.
#' @param change_rake_args Logical value whether raking argument defaults need to be changed.
#' Default = FALSE. If set to TRUE, user will be asked to set the following arguments:
#' \strong{CtrlAgeGrpsTotals} (default = NULL);
#' \strong{VarRegion} (otherwise pre-specified as From geography found in name of the conversion
#' table, e.g., HA);
#' \strong{VarSex} (otherwise pre-specified as "Sex");
#' \strong{VarSexTotal} (otherwise pre-specified from data's column names);
#' \strong{AgeGrpMax} (default = NULL which would trigger \code{\link{dbRake}} to use age 75 if
#' exists; however, dbConvert sets this to the strongly recommended age 75);
#' \strong{allowNegatives} (default = FALSE, should only be TRUE for migration data);
#' \strong{saveInterimFiles} (default = FALSE);
#' \strong{writeOutputFile} (default = FALSE);
#' \strong{writeRakingLog} (default = FALSE).
#' @param full_BC Logical value whether the region covers all of BC. Default = TRUE. Those regions
#' (e.g., VI, CMAs) that do not cover all of BC have full_BC = FALSE, and their sum is not checked
#' against the BC total. As well, during conversion, source geographies not included in the
#' destination geographies are dropped from the From and To working datafiles.
#'
#' Regardless of `change_rake_args`'s value, \code{\link{dbRake}}'s argument `readFiles` will be
#' set to FALSE because the input files will be created during the conversion process.
# @param control_totals Name of .xlsx or .csv file that contains overall control totals
# (e.g., "BC AS TOTALS.xlsx") to be used in \code{\link{dbRake}} for argument "CtrlPopTotals".
# This file is assumed to have Sex (e.g., 1, 2, 3) as rows and Ages (e.g., 0, 1, 2, ..., TOTAL) as
# columns. Values are population counts. This file typically has dimensions of 3 (obs) by 103 variables.
# @param region_totals Name of .xlsx or .csv file that contains overall control totals
# (e.g., "LHA TOTALS.xlsx") to be used in \code{\link{dbRake}} for argument "CtrlRegionTotals".
# Default = NULL. This file is assumed to have Region (e.g., 89 LHAs) as the first column and
# TOTAL (population counts) as the second column; this file is not broken out by Sex or Age. This
# file typically has dimensions of n (obs) by 2 variables, where "n" is the number of individual
# regions (e.g., 89 for LHA). If no name is provided (i.e., NULL), then region control totals are
# not used. Instead, the InputData will be used to generate "control" totals.
#' @return Database converted from source to destination geography. If not all allocations were 100,
#' (i.e., some splits < 100), then raking was also done (unless explicitly set to FALSE). Note that
#' Age can be a positive age (e.g., 0, 1, 2, ...), a 5-year age group (e.g., 0-4) or TOTAL.
#' @examples
#' \dontrun{ dbConvert(db = "POPHAP20", conv_table = "Table-LHA-HSDA-2019.xlsx", rake = FALSE) } ## all 100%
#' \dontrun{ dbConvert(db = "POPHAP20", conv_table = "Table-LHA-SD-2019") }          ## some split
#' \dontrun{ dbConvert(db = "POPCSE21", conv_table = "Table-CSD-VI_NO_CRD-2020", full_BC = FALSE) }
#' @family conversion helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @export
dbConvert <- function(db, conv_table, years = NULL, rake = TRUE, change_rake_args = FALSE, full_BC = TRUE) {

  ## PREP ----

  ## 1. Read in `FromDB`, the input/source database with population counts that needs to be converted
  temp <- stringr::str_sub(db, start = 4, end = -3)  ## remove "POP" from beginning and YY from end
  temp <- stringr::str_sub(temp, start = -1)         ## database type (E = estimates, P = projections)
  if(temp == "E") {
    db_path <- paste0(dbPaths$est_path, db, ".csv")  ## e.g., I:/PopulationR/Database/Estimates/POPRREYY.csv
  } else if(temp == "P") {
    db_path <- paste0(dbPaths$proj_path, db, ".csv") ## e.g., I:/PopulationR/Database/Projections/POPRRPYY.csv
  } else {
    db_path <- paste0(dbPaths$pop_path, db, ".csv")  ## e.g., I:/PopulationR/Database/POPRRYY.csv
  }; rm(temp)
  FromDB <- dbRead(db_path, full_BC = full_BC)
  if(is.null(years)) {
    ## use all years in source data
    years <- unique(FromDB$Year)
  } else {
    ## use year(s) set by user; check that they are all in source data
    if(!all(years %in% unique(FromDB$Year))) {
      stop(paste0("Error: Not all year(s) of data you requested are in the source data. ",
                  "You asked for year(s): ", years, ". The source data has the following year(s): ",
                  sort(unique(FromDB$Year))))
    }
  }
  if(class(FromDB$Age) == "character") {
    FromDB <- FromDB %>% dplyr::mutate(Age = dplyr::case_when(Age == "TOTAL" ~ "-999",
                                                              TRUE ~ Age))
  }
  ages <- unique(FromDB$Age)

  ## 2. Read in conversion table
  if(stringr::str_detect(conv_table, ".xlsx")) {
    conv_path <- paste0(dbPaths$conv_tbl_path, conv_table)
  } else {
    conv_path <- paste0(dbPaths$conv_tbl_path, conv_table, ".xlsx")
  }
  tbl <- conversionRead(conv_path)

  ## some sources go to multiple destinations, and some destinations come from multiple sources
  ## e.g., SD 27 comes from LHA 25 (98%) + 27 (100%) + 49 (21%)
  ##   while LHA 25 -> SD 27 (98%) + LHA 74 (2%)
  ##   while LHA 27 -> SD 27 (100%)
  ##   while LHA 49 -> SD 27 (21%) + LHA 49 (79%)
  temp <- tbl %>% dplyr::group_by(destination) %>% dplyr::summarize(mn = mean(split)) %>%
    dplyr::filter(mn != 100) %>% dplyr::select(destination) %>% dplyr::mutate(flag = 1)
  tbl <- tbl %>% dplyr::left_join(temp, by = "destination"); rm(temp)
  tblSplits <- tbl %>% dplyr::filter(flag == 1)

  ## 3. Get conversion table's region Types (from table NAME)
  parts <- stringr::str_split(stringr::str_replace(toupper(conv_table), "TABLE", ""), pattern = "-", simplify = T)
  parts <- parts[which(nchar(parts) > 1)]   ## Note: 1=FromDB; 2=ToDB; 3=year
  TypeTo <- parts[2]
  ## get proper 2-character region code ID
  if(nchar(TypeTo) > 2) {
    TypeTo <- FrankNames %>% dplyr::filter(ID_F == TypeTo) %>% dplyr::select(ID) %>% dplyr::pull()
  }

  ## 4. Get Sexes, from column names
  Sexes <- names(FromDB)[-(1:4)]

  ## 5. Determine whether all 1-to-1 or if some source geogs split across multiple destinations
  if(all(tbl$split == 100)) {
    split <- FALSE
  } else {  split <- TRUE  }

  ## CONVERSION ----

  ## 6. Create output file placeholder and full_join conversion info (full to get info from both geogs)
  ToDB <- FromDB %>% dplyr::full_join(tbl, by = c("TypeID" = "source"))
  if(full_BC == TRUE & any(is.na(ToDB$destination))) {
    stop("Some destination geographies are unfound. Check that the correct conversion table is being used.")
  }

  ## 6b. if full_BC = FALSE (destination geographies are of only a portion of BC (e.g., VI, CMA))
  if(full_BC == FALSE) {
    keepGeogs <- ToDB %>% dplyr::filter(!is.na(destination)) %>% dplyr::select(TypeID) %>%
      dplyr::pull() %>% unique()
    ## when destination geog(s) do not cover all of BC, drop unneeded (NA) geogs
    ToDB <- ToDB %>% dplyr::filter(!is.na(destination))
    FromDB <- FromDB %>% dplyr::filter(TypeID %in% keepGeogs)
  }

  if(any( sort(unique(tbl$source)) == sort(unique(FromDB$TypeID)) ) != TRUE) {
    stop(paste0("Error: There is a mismatch in region codes between the source in conversion table ",
                conv_table, " and the ", db, " db. Conversion will not proceed."))
  }

  ## 7. Apply proportions to counts and aggregate by destination geography
  ToDB <- ToDB %>%
    dplyr::mutate(Male = Male * split/100,            ## multiply by split to allocate proportionally
                  Female = Female * split/100,
                  Total = Total * split/100) %>%
    dplyr::select(-split, -Type, -TypeID) %>%         ## drop no-longer-needed vars (FromDB Type & TypeID)
    dplyr::group_by(Year, Age, destination) %>%
    dplyr::summarize(Male = rounded(sum(Male)),       ## aggregate splits by ToDB TypeID (destination)
                     Female = rounded(sum(Female)),   ## round values (b/c may be multiplying by percentages)
                     Total = rounded(sum(Total))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Type = TypeTo) %>%                  ## set ToDB's Type
    dplyr::select(Year, Type, TypeID = destination, Age, tidyselect::everything()) %>%
    dplyr::arrange(Year, TypeID, Age)

  ## RAKING ----

  ## 8. Raking is needed when splits exist, but NOT needed when all are 100
  if(rake == TRUE & split == TRUE) {

    ## A. get/set raking arguments
    if(change_rake_args == TRUE) {
      ## ask user for raking arguments
      message("You set change_raking_args to TRUE. Please set them now. What do you want to use for: ")
      CtrlAgeGrpsTotals <- readline(prompt = "CtrlAgeGrpsTotals: (NULL or name of .xlsx or .csv file of initial 5 year age group totals.) ");
      VarRegion <- readline(prompt = "VarRegion: (Region Type (e.g., HA)) ");
      VarSex <- readline(prompt = "VarSex: (Name of Sex variable in database) ");
      VarSexTotal <- readline(prompt = "VarSexTotal: (Value of Sex Total (e.g., 3)) ")
      AgeGrpMax <- readline(prompt = "AgeGrpMax: (NULL or an age ending in 0 or 5; recommend 75) ")
      allowNegatives <- readline(prompt = "allowNegatives: (TRUE or FALSE) ")
      saveInterimFiles <- readline(prompt = "saveInterimFiles: (TRUE or FALSE) ")
      writeOutputFile <- readline(prompt = "writeOutputFile: (TRUE or FALSE) ")
      writeRakingLog <- readline(prompt = "writeRakingLog: (TRUE or FALSE) ")
    } else {
      ## set raking arguments (use dbRake() defaults)
      CtrlAgeGrpsTotals <- NULL
      VarRegion <- TypeTo
      VarSex <- "Sex"
      VarSexTotal <- length(Sexes)
      AgeGrpMax <- 75
      allowNegatives <- FALSE
      saveInterimFiles <- FALSE
      writeOutputFile <- FALSE
      writeRakingLog <- FALSE
    }

    ## B. get any age groupings (5 yrs grps or "and over" grps) other than total (-999) and last age (e.g., -100)
    ageGrps <- ages[stringr::str_detect(ages, "-") & ages != -999]
    if(length(ageGrps) == 0) {
      ## find ageOldest when NO ages have a "-" in them
      ageOldest <- max(as.numeric(ages[ages != "TOTAL" & ages != 999 & ages != "999"]))
    } else {
      ageOldest <- ageGrps[ageGrps == min(ageGrps)]
      ageEnds <- ageGrps[ageGrps != ageOldest]
      ageDivs <- ageEnds[which(as.numeric(ageEnds) %% 5 == 0)]
      ageEnds <- sort(abs(ageEnds[!ageEnds %in% ageDivs]))  ## drops nums ending in 0 or 5 numbers evenly divisible by 5 (i.e., end in 0 or 5)
      ageGrps5Yr <- rep(NA, length(ageEnds))
      for (i in 1:length(ageGrps5Yr)) { ageGrps5Yr[i] <- paste0((ageEnds[i]-4), "-", ageEnds[i]) }
      agesTemp <- data.frame(Ends = ageDivs) %>% dplyr::mutate(Grps = paste0(abs(Ends), "+"))
      ageGrpsLookup <- data.frame(Ends = (-1*ageEnds)) %>%
        dplyr::mutate(Grps = ageGrps5Yr) %>%
        dplyr::bind_rows(agesTemp) %>%
        tibble::add_row(Ends = -999, Grps = "Total") %>%
        #tibble::add_row(Ends = -100, Grps = "100") %>%
        tibble::add_row(Ends = ageOldest, Grps = as.character(abs(ageOldest))) %>%
        dplyr::arrange(dplyr::desc(Ends)) %>%
        dplyr::mutate(order = 200 + dplyr::row_number(),
                      order = dplyr::case_when(Ends == ageOldest ~ as.numeric(abs(ageOldest)),
                                               TRUE ~ as.numeric(order)))
      rm(i, ageDivs, ageEnds, agesTemp)
    }

    ## C. hold apart (remove) 1-to-1 regions from raking process for now (will put back at end)
    ##    *** this is where Age becomes character, ageOldest becomes pos, and -999 becomes "TOTAL" ***
    hold_1to1s <- ToDB %>%
      dplyr::filter(!(TypeID %in% unique(tblSplits$destination))) %>%
      ## wrangle to get nice age group names ("0-4" instead of "-4", etc.)
      tidyr::pivot_longer(!c(Year, Type, TypeID, Age), names_to = "Sex", values_to = "value") %>%
      dplyr::mutate(Age = dplyr::case_when(Age == ageOldest ~ as.character(abs(as.numeric(Age))),
                                           Age == -999 ~ "TOTAL", TRUE ~ as.character(Age))) %>%
      tidyr::pivot_wider(names_from = "Age", values_from = "value") %>%
      rename.age.grps(VarRegion, VarSex) %>%
      tidyr::pivot_longer(!c(Year, Type, TypeID, Sex), names_to = "Age", values_to = "value") %>%
      tidyr::pivot_wider(names_from = "Sex", values_from = "value")

    ## D. format ToDB_rake as needed (get only destinations in tblSplits)
    ##    *** this is where Age becomes character, ageOldest becomes pos, and -999 becomes "TOTAL" ***
    ToDB_rake <- ToDB %>%
      dplyr::filter(TypeID %in% unique(tblSplits$destination)) %>%
      tidyr::pivot_longer(tidyselect::all_of(Sexes), names_to = "Sex", values_to = "value") %>%
      dplyr::mutate(Sex = dplyr::case_when(Sex == "Male" ~ 1, Sex == "Female" ~ 2, Sex == "Total" ~ 3),
                    Age = dplyr::case_when(Age == -999 ~ 999, Age == ageOldest ~ abs(as.numeric(Age)),
                                           TRUE ~ as.numeric(Age))) %>%
      dplyr::arrange(Year, Age, Sex) %>%
      dplyr::mutate(Age = dplyr::case_when(Age == 999 ~ "TOTAL", TRUE ~ as.character(Age))) %>%
      tidyr::pivot_wider(names_from = "Age", values_from = "value") %>%
      dplyr::select(Year, {{TypeTo}} := TypeID, Sex, tidyselect::everything(), -Type)

    ## E. get control_totals (SOURCE regions, only sources in tblSplits (remove 1to1s), sum Age and Sex across regions)
    FromCtrls <- FromDB %>%
      dplyr::filter(TypeID %in% unique(tblSplits$source)) %>%  ## !(Age %in% ageGrps)
      dplyr::group_by(Year, Age) %>%
      dplyr::summarize(Male = sum(Male), Female = sum(Female), Total = sum(Total)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(tidyselect::all_of(Sexes), names_to = "Sex", values_to = "value") %>%
      dplyr::mutate(Sex = dplyr::case_when(Sex == "Male" ~ 1, Sex == "Female" ~ 2, Sex == "Total" ~ 3),
                    Age = dplyr::case_when(Age == -999 ~ 999, Age == ageOldest ~ abs(as.numeric(Age)),
                                           TRUE ~ as.numeric(Age))) %>%
      dplyr::arrange(Year, Age, Sex) %>%
      dplyr::mutate(Age = dplyr::case_when(Age == 999 ~ "TOTAL", TRUE ~ as.character(Age))) %>%
      tidyr::pivot_wider(names_from = "Age", values_from = "value")

    ## F1. iterate dbutils::dbRake() over years
    raked_all <- vector(mode = "list", length = length(years))
    raking_log_all <- vector(mode = "list", length = length(years))
    for(yr in seq_along(years)) {
      InputData <- ToDB_rake %>% dplyr::filter(Year == years[yr]) %>% dplyr::select(-Year)
      control_totals <- FromCtrls %>% dplyr::filter(Year == years[yr]) %>% dplyr::select(-Year)
      if(length(ageGrps) > 1) {
        CtrlAgeGrpsTotals <- FromCtrls %>%
          dplyr::filter(Year == years[yr]) %>%
          dplyr::select(Sex, sort(tidyselect::contains("-"), decreasing = TRUE),
                        names(FromCtrls)[names(FromCtrls) == abs(ageOldest)], TOTAL)
      }
      message(paste0("Year ", years[yr]))
      raked <- dbRake(InputData, CtrlPopTotals = control_totals, CtrlRegionTotals = NULL,
                      CtrlAgeGrpsTotals, VarRegion, VarSex, VarSexTotal, AgeGrpMax = 75,
                      allowNegatives, saveInterimFiles, writeRakingLog, writeOutputFile, readFiles = FALSE)
      raked_all[[yr]] <- raked[["RakedData"]]
      if(writeRakingLog == TRUE) {  raking_log_all[[yr]] <- raked[["RakingLog"]]  }
      rm(InputData, control_totals, raked)
    }

    ## F2. add back Year, merge all Years of now-raked data
    ToDB_done <- purrr::map(.x = 1:length(years), ~ dplyr::mutate(raked_all[[.]], Year = years[.x]))
    ToDB_done <- purrr::map_dfr(.x = 1:length(years), ~ dplyr::bind_rows(ToDB_done[[.]])) %>%
      dplyr::select(Year, tidyselect::everything())

    if(writeRakingLog == TRUE) {
      raking_log <- purrr::map(.x = 1:length(years), ~ dplyr::mutate(raking_log_all[[.]], Year = years[.x]))
      raking_log <- purrr::map_dfr(.x = 1:length(years), ~ dplyr::bind_rows(raking_log[[.]])) %>%
        dplyr::select(Year, message)
      readr::write_csv(raking_log, here::here("outputs", "raking_log.csv"))
    }

    ## G. flip raked data, put back all held out 1-to-1 regions removed during raking (step 8C)
    ToDB_done <- ToDB_done %>%
      tidyr::pivot_longer(!c(Year, {{TypeTo}}, Sex), names_to = "Age", values_to = "value") %>%
      dplyr::mutate(Age = dplyr::case_when(Age == "999" ~ "TOTAL",
                                           Age == ageOldest ~ as.character(ageOldest), TRUE ~ Age),
                    # Age = as.numeric(Age),
                    Sex = dplyr::case_when(Sex == 1 ~ "Male", Sex == 2 ~ "Female", Sex == 3 ~ "Total"),
                    Type = {{TypeTo}}) %>%
      tidyr::pivot_wider(names_from = "Sex", values_from = "value") %>%
      dplyr::rename(TypeID = {{TypeTo}}) %>%
      dplyr::select(Year, Type, tidyselect::everything())

    ## add back hold_1to1s, if any
    if(dim(hold_1to1s)[1] > 0) {
      ToDB_done <- ToDB_done %>% dplyr::bind_rows(hold_1to1s)
    }

    ## H. add any missing age groups to ToDB_done (most likely age -90, meaning 90+)
    if(length(ageGrps) != 0) {
      ageNeed <- unique(FromDB$Age)
      temp <- ageNeed[ageNeed < 0]
      temp <- data.frame(Ends = temp) %>% dplyr::left_join(ageGrpsLookup, by = "Ends") %>%
        dplyr::select(Grps) %>% dplyr::pull()
      ageNeed <- setdiff(c(stringr::str_replace(temp, "Total", "TOTAL"), ageNeed[ageNeed >= 0]),
                         unique(ToDB_done$Age)); rm(temp)
      for(a in seq_along(ageNeed)) {
        if(stringr::str_detect(ageNeed[a], "[+]")) {
          colStart <- as.numeric(stringr::str_replace(ageNeed[a], "[+]", ""))
          colEnd <- abs(ageOldest)
          ## create aggregate age group and add to ToDB_done
          temp <- ToDB_done %>%
            dplyr::filter(Age != "TOTAL" & stringr::str_detect(Age, "-", negate = TRUE)) %>%
            dplyr::mutate(Age = as.numeric(Age)) %>%
            dplyr::filter(Age %in% colStart:colEnd) %>%
            dplyr::group_by(Year, Type, TypeID) %>%
            dplyr::summarize(dplyr::across(!c(Year, Type, TypeID, Age), sum)) %>%
            dplyr::mutate(Age = "90+")
          ToDB_done <- dplyr::bind_rows(ToDB_done, temp)
          rm(temp, colStart, colEnd)
        }
      }; rm(a)

      ## sort properly
      ageGrpsLookup$Grps <- stringr::str_replace(ageGrpsLookup$Grps, "Total", "TOTAL")
      ToDB_done <- ToDB_done %>%
        dplyr::left_join(ageGrpsLookup %>% dplyr::select(Age = Grps, order), by = "Age") %>%
        dplyr::mutate(order = suppressWarnings(dplyr::case_when(is.na(order) ~ (as.numeric(Age)/100),
                                                                TRUE ~ as.numeric(order)))) %>%
        dplyr::arrange(Year, TypeID, order) %>%
        dplyr::select(-order)
    }

    # readr::write_csv(ToDB_done, here::here("outputs", "RakedData.csv"))

    rkg <- "Raking was also done. "

  }
  if(rake == FALSE & split == TRUE) {
    rkg <- "Even though not all geographies were 1-to-1, raking was not done, as per your specification. "
  }
  if(split == FALSE) {
    ageGrps <- ages[stringr::str_detect(ages, "-") & ages != -999]
    if(length(ageGrps) == 0) {
      ## find ageOldest when NO ages have a "-" in them
      ageOldest <- max(as.numeric(ages[ages != "TOTAL" & ages != 999 & ages != "999"]))
    } else {
      ageOldest <- ageGrps[ageGrps == min(ageGrps)]
    }
    ##    *** this is where Age becomes character, ageOldest becomes pos, and -999 becomes "TOTAL" ***
    ToDB_done <- ToDB %>%
      dplyr::mutate(Age = dplyr::case_when(Age == -999 ~ 999,
                                           Age == ageOldest ~ abs(as.numeric(Age)),
                                           TRUE ~ as.numeric(Age))) %>%
      dplyr::arrange(TypeID, Year, Age) %>%
      dplyr::mutate(Age = dplyr::case_when(Age == 999 ~ "TOTAL", TRUE ~ as.character(Age)))

    rkg <- "All geographies were 1-to-1, so raking was not done. "
  }

  ## OUTPUTS ----
  ## Let user know what was converted, and whether or not raking was done.
  rgsFrom <- paste0(sort(unique(FromDB$TypeID)), collapse = ", ")
  rgsTo <- paste0(sort(unique(ToDB$TypeID)), collapse = ", ")
  message(paste0("Conversion is done. ", rkg))
  message(paste0("Years are ", paste0(sort(years), collapse = ", "), ". "))
  message(paste0("Regions FROM are ", rgsFrom, ". "))
  message(paste0("Regions TO are ", rgsTo, ". "))
  message(paste0("Ages are ", paste0(sort(ages), collapse = ", "), ". "))
  if(!all(ages == unique(ToDB_done$Age))) {
    message(paste0("Ages TO were transformed to ", paste0(unique(ToDB_done$Age), collapse = ", "), ". "))
  }

  return(ToDB_done)

}
