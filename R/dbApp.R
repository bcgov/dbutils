#### shiny app prep functions ----


#### updateAppData ----
#' Update population app data
#'
#' @description
#' Read multiple databases, of the same year, to create a combined updated database for the Population
#' \href{https://bcstats.shinyapps.io/popApp/}{Estimates} and
#' \href{https://bcstats.shinyapps.io/popProjApp/}{Projections} apps.
#'
#' @details
#' The Population Estimates and Projections apps provide corresponding data for the region type,
#' region(s), year(s), gender(s), and age(s) selected by the app's user. Once the user chooses one
#' of the 8 available Region Types, they then select whichever of that Region Type's Regions they
#' want, for whichever Year(s) of data are available, for any combination of Males, Females, Totals.
#' They then choose either Single Year Age Groups (0, 1, 2, ..., 88, 89, 90+), 5-Year Age Groups
#' (LT1, 1-4, 5-9, ..., 80-84, 85-89, 90+), Totals, or create Custom Age Groups. They can then
#' generate the output on-screen and/or download the data as a csv.
#'
#' A lookup table of full text region names for region types must be in the "I:/ConversionTables/"
#' folder, as either a csv or xlsx file type, with columns "TypeID", "Type" and "Region.Name".
#'
#' The app takes this function's return output and calculates 5-year age groups or custom age groups
#' as requested by the user. Therefore, the return output need only include individual and total age
#' data (i.e., 0, 1, 2, ..., 88, 89, 90+).
#'
#' This function replaces the analysis portion of popApp and popPropjApp. Instead, call this
#' function, save the resulting output as data1.rds in the corresponding .../app/data folder, then
#' open app.R to deploy the updated app. (The popApp and popProjApp Rprojects are at
#' 'I:/PEOPLEPROJECTIONS/00 - R_code/shiny_apps/Production/'.)
#'
#' @param dbType Type of database being written. Possible values are "estimates" or "projections".
#' @param dbYear Two-digit year of the data being saved. Based on July 1st reference date,
#' as character.
#' @param RegionTypes A dataframe with two character columns: ID and Region.Type. Default = "default"
#' which internally sets ID and Region.Type as follows:
#' \itemize{
#'  \item ID: Two-digit region code of the databases. Values include: "RD", "SD", "HY", "CF",
# "HS", "HA", "DR", "PS", "SR", "CH" as character. Note that "CF" and "PS" are only for projections.
#'  \item Region.Type: Full text name of region type. Values include: "Regional District",
#' "School District", "Health Authority", "Children and Family Development", "Health Service
#' Delivery Area", "Local Health Area", "Development Region", "College Region", "Special Regions
#' (CMAs and Vancouver Island)", and "Community Health Service Area"). Note that "Children and
#' Family Development" and "College Region" are only for projections.
#' }
#' @param RegionNamesFile The name (including extension, either .csv or .xlsx) of the lookupfile
#' of Region names (e.g., 'Lookup_Region_Names.csv'). This file must be located in the ConversionTables
#' folder (i.e., dbPaths$conv_tbl_path, "//SFP.IDIR.BCGOV/S152/S52004/ConversionTables/").
#' @return A data.frame object with variables: Region, Region.Name, Region.Type, Year, Gender,
#' Total, individual age columns (e.g., 0, 1, 2, ..., 89), age group columns (e.g., 0-4, 5-9, 90+).
#' Region.Name is the text version of the Region (e.g., "British Columbia" for Region 0), and Gender
#' is the first initial of the gender values (e.g., M, F, T). Data type is either projections or estimates.
#' @examples
#' \dontrun{  updateAppData(dbType = "estimates", dbYear = "19",
#'                          RegionTypes = data.frame(ID = c("RD", "DR"),
#'                                        Region.Type = c("Regional District", "Development Region"),
#'                                        stringsAsFactors = FALSE),
#'                          RegionNamesFile = "Lookup_Region_Names.csv")  }
#' \dontrun{  updateAppData(dbType = "projections", dbYear = "19", RegionTypes = "default",
#'                          RegionNamesFile = "Lookup_Region_Names.csv")  }
# @family app helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @export
updateAppData <- function(dbType, dbYear, RegionTypes = "default", RegionNamesFile) {

  ## if using default RegionTypes, create here (else, add in via RegionTypes argument)
  if(RegionTypes == "default") {
    RegionTypes <- data.frame(ID = c("RD", "SD", "HY", "CF", "HS", "HA", "DR", "PS", "SR", "CH"),
                              Region.Type = c("Regional District",                  ## RD
                                              "School District",                    ## SD
                                              "Health Authority",                   ## HY
                                              "Children and Family Development",    ## CF
                                              "Health Service Delivery Area",       ## HS
                                              "Local Health Area",                  ## HA
                                              "Development Region",                 ## DR
                                              "College Region",                     ## PS
                                              "Special Regions (CMAs and Vancouver Island)",  ## SR
                                              "Community Health Service Area"),     ## CH
                              stringsAsFactors = FALSE)
    if(dbType == "estimates") {
      ## estimates does NOT include CF (Children and Family Development) or PS (College Region)
      RegionTypes <- RegionTypes %>% dplyr::filter(ID != "CF" & ID != "PS")
    }
  }

  if(!all(c("ID", "Region.Type") %in% names(RegionTypes))) {
    stop("Error: One or both required columns ('ID', 'Region.Type') are missing from RegionTypes.")
  }

  ## Lookup_Region_Names.csv was created by opening Database work/WorkingFile.accdb and copying REGNAMES into Excel
  if(stringr::str_detect(RegionNamesFile, ".csv")) {            ## RegionNamesFile is csv
    LookupRegionNames <- readr::read_csv(paste0(dbPaths$conv_tbl_path, RegionNamesFile))
  } else if(stringr::str_detect(RegionNamesFile, ".xlsx")) {    ## RegionNamesFile is xlsx
    LookupRegionNames <- openxlsx::readWorkbook(xlsxFile = paste0(dbPaths$conv_tbl_path, RegionNamesFile),
                                                colNames = FALSE)
  } else {
    stop("Error: Incorrect RegionNamesFile format (must be either .csv or .xlsx).")
  }

  if(!all(c("TypeID", "Type", "Region.Name") %in% names(LookupRegionNames))) {
    stop("Error: One or more required columns ('TypeID', 'Type', 'Region.Name') are missing from LookupRegionNames.")
  }

  ## read in required databases
  if(!(dbType %in% c("projections", "estimates"))) {
    stop("Error: Database type ('dbType') must be either 'projections' or 'estimates'.")
  }
  for(i in seq_along(RegionTypes$ID)) {
    db <- dbRead(db_path = c(dbType, RegionTypes$ID[i], dbYear), db_check = FALSE)
    assign(paste0("db_", RegionTypes$ID[i]), value = db)
  }

  ## combine all databases into one
  db <- db %>% dplyr::filter(Year == 0)                         ## get empty db of correct structure
  for(i in seq_along(RegionTypes$ID)) {
    db <- dplyr::bind_rows(db, get(paste0("db_", RegionTypes$ID[i])))
  }

  ## make LookupAges (to order Age columns), nicer age group names (e.g., X1-Y1, X2-Y2 instead of -X1, -X2)
  ages <- sort(unique(db$Age))
  ageGrps <- ages[stringr::str_detect(ages, "-") & ages != -999]
  ageOldest <- ageGrps[ageGrps == min(ageGrps)]
  ageDivs <- ageGrps[which(as.numeric(ageGrps) %% 5 == 0 & ageGrps != ageOldest)]
  LookupAges <- data.frame(Ends = ages, Age = ages) %>%
    dplyr::mutate(Age = dplyr::case_when(Ends == -999 ~ "TOTAL",
                                         # Ends %in% ageOldest ~ as.character(abs(Ends)),
                                         Ends %in% ageOldest ~ paste0(abs(Ends), "+"),
                                         Ends %in% ageDivs ~ paste0(abs(Ends), "+"),
                                         Ends %in% ageGrps ~ paste0(abs(Ends)-4, "-", abs(Ends)),
                                         TRUE ~ as.character(Age)),
                  order = dplyr::case_when(stringr::str_detect(Age, "-") ~ as.numeric(200+abs(Ends)),
                                           stringr::str_detect(Age, "[+]") ~ as.numeric(300+abs(Ends)),
                                           TRUE ~ as.numeric(abs(Ends)))) %>%
    dplyr::arrange(order)

  ## pivot db long by Gender for all columns other than Year, Type, TypeID, Age
  db <- db %>%
    tidyr::pivot_longer(!c("Year", "Type", "TypeID", "Age"), names_to = "Gender", values_to = "value")

  ## drop ages other than 0-89, -90, -999 (i.e., all age groups, any individual ages above 90)
  db <- db %>% dplyr::filter(Age %in% ageLists$pop1yr90)

  ## Age needs to be numeric to run abs() mathematical function
  if(class(db$Age) == "integer") {
    db <- db %>% dplyr::mutate(Age = dplyr::case_when(Age == -999 ~ "Total",
                                                      # Age %in% ageOldest ~ as.character(abs(Age)),
                                                      Age %in% ageOldest ~ paste0(abs(Age), "+"),
                                                      Age %in% ageDivs ~ paste0(abs(Age), "+"),
                                                      TRUE ~ as.character(Age)))
  }

  ## run Age columns wide
  db <- db %>% tidyr::pivot_wider(names_from = "Age", values_from = "value")

  ## if any Age columns are an age group (start with "-"), rename them from X1, X2, ... to X1-Y1, X2-Y2, ...
  if(any(stringr::str_starts(names(db), "-"))) {
    db <- rename.age.grps(data = db, VarRegion = c("Year", "Type", "TypeID"), VarSex = "Gender")
  }

  ## add in Region.Name and Region.Type, re-name and order variables
  db <- db %>%
    dplyr::left_join(LookupRegionNames, by = c("TypeID", "Type")) %>%  ## add Region.Name from lookup file
    dplyr::left_join(RegionTypes, by = c("Type" = "ID")) %>%           ## add in Region.Type (long name for Type)
    dplyr::mutate(Gender = stringr::str_sub(Gender, start = 1, end = 1)) %>%  ## get first initial of each gender
    dplyr::select(Region = TypeID, Region.Name, Region.Type, -Type, Year, Gender, Total,
                  tidyselect::any_of(LookupAges$Age))                  ## re-name and order variables

  return(db)

}
