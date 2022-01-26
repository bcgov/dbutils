#### database access functions ----


#### getDBPath ----
#' Returns a correct full database path
#'
#' @description
#' Returns a population database file (population, births, deaths) path. Database paths are hardcoded
#' to ensure consistency across population systems. The script uses data type, region type and year
#' to return an appropriate full path.
#'
#' The complete list of region IDs and ages for each type of database (population, deaths, births)
#' can be found in \code{\link{dbutils}}. For historical reasons, population data is saved as
#' POPRREYY (population estimates), POPRRPYY (population projections), BIRRRYY (births) or
#' DEARRYY (deaths), where RR is the shorthand for the region code, and YY is the last two digits
#' of the year.
#'
#' @param dbType Type of database being written. Possible values are "estimates", "projections",
#' "deaths", "births". Default = "estimates".
#' @param dbRegion Two-digit region code of the database. Possible values: "RD", "DR", "HA", "HS",
#' "HY", "CH", "CF", "CA", "CL", "SD", "PS", "SR" as character. Default = NULL.
#' @param dbYear Two-digit year of the data being saved. Based on July 1st reference date,
#' as character. Default = NULL.
#' @examples
#' getDBPath(dbType = "estimates", dbRegion = "CA", dbYear = "17")  ## "//.../POPCAE17.csv"
#' getDBPath(dbType = "births", dbRegion = "RD", dbYear = "18")     ## "//.../BIRRD18.csv"
#' @family database access helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @author Sebastien Lavoie (formerly, BC Stats)
#' @export
getDBPath <- function(dbType, dbRegion, dbYear) {

  if(is.null(dbRegion)) {
    stop("Error: No region type specified.")
  #} else if(!(dbRegion %in% c("RD", "DR", "HA", "HS", "HY", "CF", "CA", "CL", "SD", "PS", "SR", "CH"))) {
  } else if(!(dbRegion %in% names(regionIDs) | dbRegion %in% FrankNames$ID)) {
    stop("Error: Unrecognized region type.")
  }

  if(is.null(dbYear)) {
    stop("Error: No year specified.")
  } else if(!is.character(dbYear) || stringr::str_length(dbYear) != 2) {
    stop("Error: Incorrect dbYear value. Must be two-digit year as character, e.g. '17'")
  }

  if(!(dbType %in% c("estimates", "projections", "births", "deaths"))) {
    stop("Error: Unrecognized database type.")
  }

  if(       dbType == "estimates") {
    db_path <- paste0(dbPaths$est_path, "POP", dbRegion, "E", dbYear, ".csv")
  } else if(dbType == "projections") {
    db_path <- paste0(dbPaths$proj_path, "POP", dbRegion, "P", dbYear, ".csv")
  } else if(dbType == "births") {
    db_path <- paste0(dbPaths$bir_path, "BIR", dbRegion, dbYear, ".csv")
  } else if(dbType == "deaths") {
    db_path <- paste0(dbPaths$dea_path, "DEA", dbRegion, dbYear, ".csv")
  } else {
    stop("Error: Unrecognized database type.")
  }

  return(db_path)
}


#### dbCheck ----
#' Check population database for errors
#'
#' @description
#' Checks a database for specific errors, at loading (\code{\link{dbRead}}, \code{\link{dbInfo}}),
#' after raking (\code{\link{dbRake}}), or before saving (\code{\link{dbWrite}}).
#' \enumerate{
#'  \item Database must have 7 columns: Year, Type, TypeID, Age, Male, Female, and Total.
#'  \item Each Year of data must have the same number of occurrences.
#'  \item Years of data must be continuous (no year can be missing).
#'  \item The Total column must equal the sum of the Male and Female columns.
#'  \item All combinations of region's TypeID, Age and sex must be included (even if N = 0).
#'  \item For Females: (a) the sum over all age groups must be equal (i.e., single age groups
#'  add to total (-999), (b) "ages and over" are sum of corresponding single ages, and, (c) 5-year
#'  age groups are sum of corresponding single ages).
#'  \item For Males: (a) the sum over all age groups must be equal (i.e., single age groups
#'  add to total (-999), (b) "ages and over" are sum of corresponding single ages, and, (c) 5-year
#'  age groups are sum of corresponding single ages).
#'  \item For Total: (a) the sum over all age groups must be equal (i.e., single age groups
#'  add to total (-999), (b) "ages and over" are sum of corresponding single ages, and, (c) 5-year
#'  age groups are sum of corresponding single ages).
#'  \item For database with full coverage of BC (i.e., full_BC = TRUE), the sum of all regions
#'  must equal region 0 (BC) for each age and gender.
#' }
#' Also, warns user if database has NO older age groups (i.e., negative ages ending in 0 or 5)
#' and/or, if database has NO 5-year age groups (i.e., negative ages ending in 4 or 9).
#'
#' @param db Data variable containing the database to be checked.
#' @param full_BC Logical value whether the region covers all of BC. Those regions (e.g., CMAs) that
#' do not cover all of BC have full_BC = FALSE, and their sum is not checked against the BC total.
#' Default = TRUE.
#' @return db_ok Logical value. If the database passes all checks, db_ok = TRUE. Otherwise, any
#' error(s) will be printed to screen and db_ok will be set to FALSE.
#' @examples
#' \dontrun{   dbCheck(db = "I:/VITAL/Database/Births/BIRHA19.csv", full_BC = TRUE)   }
#' \dontrun{   dbCheck("I:/PopulationR/Database/Projections/POPHAP19.csv")   }
#' @family database access helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @author Sebastien Lavoie (formerly, BC Stats); Julie Hawkins, BC Stats
#' @export
dbCheck <- function(db, full_BC = TRUE) {

  db_OK <- TRUE

  dbType <- stringr::str_sub(deparse(substitute(db)), start = 1, end = 3)

  ages <- unique(db$Age); ages <- ages[which(ages != -999)]
  ages_over <- ages[which(ages %in% seq(-5, -120, -5))]; ages_over <- ages_over[which(ages_over != min(ages))]
  ages_5yrs <- ages[which(ages %in% seq(-4, -124, -5))]

  if(length(colnames(db)) != 7) {
    db_OK <- FALSE
    print("Database error 1a: Database does not have the correct number of columns.")

  } else if(min(colnames(db) == c("Year", "Type", "TypeID", "Age", "Male", "Female", "Total")) != 1) {
    db_OK <- FALSE
    print("Database error 1b: Database does not have the correct columns. Make sure all columns are included as follows: Year, Type, TypeID, Age, Male, Female, Total.")

  } else if(min((db %>% dplyr::group_by(Year) %>% dplyr::summarize(count = dplyr::n()))$count) !=
             max((db %>% dplyr::group_by(Year) %>% dplyr::summarize(count = dplyr::n()))$count)) {
    db_OK <- FALSE
    print("Database error 2: One or more Year(s) has a different number of occurrences than the others. This could be due to duplication or missing region IDs.")

  } else if(max(unique(db$Year)) != max(seq(min(unique(db$Year)), length.out = length(unique(db$Year))))) {
    db_OK <- FALSE
    print("Database error 3: One or more Year(s) is missing from the database making it a non-continuous sequence.")

  } else if(min((db %>% dplyr::mutate(diff = Total - Male - Female))$diff == 0) != 1) {
    db_OK <- FALSE
    print("Database error 4: The Total column does not equal the sum of Male + Female columns.")

  } else if(!identical((length(unique(db$Year)) * length(unique(db$TypeID)) * length(unique(db$Age))),
                       length(db$Female[!is.na(db$Female)]), length(db$Male[!is.na(db$Male)]),
                       length(db$Total[!is.na(db$Total)]))) {
    db_OK <- FALSE
    print("Database error 5: One or more combinations of region ID, age and sex are missing from the database. Note that N may be 0.")

  } else if(length(ages_over) < 1 ) {
    warning("Be aware that the database has NO older age groups (i.e., negative ages ending in 0 or 5).")

  } else if(length(ages_5yrs) < 1 ) {
    warning("Be aware that the database has NO 5-year age groups (i.e., negative ages ending in 4 or 9).")

  } else if(any(names(db) == "Female")) {

    ### For Female:
    ## Does sum for Age -999 (total) equal the sum of individual ages?
    if((sum(db$Female[db$Age >= 0]) + sum(db$Female[db$Age == min(ages)])) != sum(db$Female[db$Age == -999])) {
      db_OK <- FALSE
      print("Database error 6a: The sum of population over all Female single age groups is not equal.")
    }

    ## Does sum of "age and over" group(s) equal sum of individual ages? (e.g., -90 = sum(90:99 + -100))
    if(length(ages_over) >= 1) {
      for(i in seq_along(ages_over)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= abs(ages_over[i]) | Age == min(ages)) %>% dplyr::select(Female) %>% sum()),
          (db %>% dplyr::filter(Age == ages_over[i]) %>% dplyr::select(Female) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 6b: The sum of population over one or more older Female age groups (i.e., negative ages ending in 0 or 5) is not equal to its individual ages.")
        }
      }; rm(i)
    }

    ## Does sum of "up to age" group equal sum of its five individual ages? (e.g., -4 = sum(0:4))
    if(length(ages_5yrs) >= 1) {
      for(i in seq_along(ages_5yrs)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= (abs(ages_5yrs[i])-4) & Age <= abs(ages_5yrs[i])) %>% dplyr::select(Female) %>% sum()),
          (db %>% dplyr::filter(Age == ages_5yrs[i]) %>% dplyr::select(Female) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 6c: The sum of population over one or more Female 5-year age groups (i.e., negative ages ending in 4 or 9) is not equal to its individual ages.")

        }
      }; rm(i)
    }

  } else if(any(names(db) == "Male")) {

    ### For Male:
    ## Does sum for Age -999 (total) equal the sum of individual ages?
    if((sum(db$Male[db$Age >= 0]) + sum(db$Male[db$Age == min(ages)])) != sum(db$Male[db$Age == -999])) {
      db_OK <- FALSE
      print("Database error 7a: The sum of population over all Male single age groups is not equal.")
    }

    ## Does sum of "age and over" group(s) equal sum of individual ages? (e.g., -90 = sum(90:99 + -100))
    if(length(ages_over) >= 1) {
      for(i in seq_along(ages_over)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= abs(ages_over[i]) | Age == min(ages)) %>% dplyr::select(Male) %>% sum()),
          (db %>% dplyr::filter(Age == ages_over[i]) %>% dplyr::select(Male) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 7b: The sum of population over one or more older Male age groups (i.e., negative ages ending in 0 or 5) is not equal to its individual ages.")
        }
      }; rm(i)
    }

    ## Does sum of "up to age" group equal sum of its five individual ages? (e.g., -4 = sum(0:4))
    if(length(ages_5yrs) >= 1) {
        for(i in seq_along(ages_5yrs)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= (abs(ages_5yrs[i])-4) & Age <= abs(ages_5yrs[i])) %>% dplyr::select(Male) %>% sum()),
          (db %>% dplyr::filter(Age == ages_5yrs[i]) %>% dplyr::select(Male) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 7c: The sum of population over one or more Male 5-year age groups (i.e., negative ages ending in 4 or 9) is not equal to its individual ages.")

        }
      }; rm(i)
    }

  } else if(any(names(db) == "Total")) {

    ### For Total (sex):
    ## Does sum for Age -999 (total) equal the sum of individual ages?
    if((sum(db$Total[db$Age >= 0]) + sum(db$Total[db$Age == min(ages)])) != sum(db$Total[db$Age == -999])) {
      db_OK <- FALSE
      print("Database error 8a: The sum of population over all single age groups is not equal.")
    }

    ## Does sum of "age and over" group(s) equal sum of individual ages? (e.g., -90 = sum(90:99 + -100))
    if(length(ages_over) >= 1) {
      for(i in seq_along(ages_over)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= abs(ages_over[i]) | Age == min(ages)) %>% dplyr::select(Total) %>% sum()),
          (db %>% dplyr::filter(Age == ages_over[i]) %>% dplyr::select(Total) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 8b: The sum of population over one or more older age groups (i.e., negative ages ending in 0 or 5) is not equal to its individual ages.")
        }
      }; rm(i)
    }

    ## Does sum of "up to age" group equal sum of its five individual ages? (e.g., -4 = sum(0:4))
    if(length(ages_5yrs) >= 1) {
      for(i in seq_along(ages_5yrs)) {
        if(!identical(
          (db %>% dplyr::filter(Age >= (abs(ages_5yrs[i])-4) & Age <= abs(ages_5yrs[i])) %>% dplyr::select(Total) %>% sum()),
          (db %>% dplyr::filter(Age == ages_5yrs[i]) %>% dplyr::select(Total) %>% sum()))) {
          db_OK <- FALSE
          print("Database error 8c: The sum of population over one or more 5-year age groups (i.e., negative ages ending in 4 or 9) is not equal to its individual ages.")

        }
      }; rm(i)
    }

  } else if(full_BC == TRUE & (db %>%
             dplyr::mutate(region = dplyr::case_when(TypeID != 0 ~ "Regions", TypeID == 0 ~ "BC")) %>%
             dplyr::group_by(region, Age) %>% dplyr::summarize(sum = sum(Total, na.rm = TRUE)) %>%
             tidyr::pivot_wider(names_from = "region", values_from = "sum") %>%
             dplyr::mutate(diff = BC - Regions) %>% dplyr::select(diff) %>% sum()) != 0 ) {
    db_OK <- FALSE
    print("Database error 9a: For one or more Age, the sum of all regions does not equal the BC (region = 0) total, but full_BC is set to TRUE.")

  } else if(full_BC == TRUE & (db %>%
             dplyr::mutate(region = dplyr::case_when(TypeID != 0 ~ "Regions", TypeID == 0 ~ "BC")) %>%
             tidyr::pivot_longer(c(Female, Male), names_to = "Sex", values_to = "N") %>%
             dplyr::group_by(region, Sex) %>% dplyr::summarize(sum = sum(Total, na.rm = TRUE)) %>%
             tidyr::pivot_wider(names_from = "region", values_from = "sum") %>%
             dplyr::mutate(diff = BC - Regions) %>% dplyr::select(diff) %>% sum()) != 0 ) {
    db_OK <- FALSE
    print("Database error 9b: For one or more Sex, the sum of all regions does not equal the BC (region = 0) total, but full_BC is set to TRUE.")

  }

  return(db_OK)

}


#### dbInfo ----
#' Population database dimension information
#'
#' @description
#' Reads a population database file (population, births, deaths) and returns list with unique
#' values for each of Year, Type, TypeID, Age and Sex. Database path can be either a full path
#' to a csv database file with the correct columns, or a vector including the data type, region type
#' and year of database to be used through \code{\link{getDBPath}}. For example,
#' db_path = c("estimates", "HA", "19") would return info for the July 1st 2019 population
#' estimates database for Local Health Areas.
#'
#' Note that \code{\link{dbCheck}} is run before pulling info. User will be warned if any database
#' checks fail.
#'
#' @details
#' The complete list of region IDs and ages for each type of database (population, deaths, births)
#' can be found in \code{\link{dbutils}}. For historical reasons, population data is saved as
#' POPRREYY (population estimates), POPRRPYY (population projections), BIRRRYY (births) or
#' DEARRYY (deaths), where RR is the shorthand for the region code, and YY is the last two digits
#' of the year.
#'
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#' region code and two-character year OR full path to database csv file.
#' @param full_BC Logical value whether the region covers all of BC. Those regions (e.g., CMAs) that
#' do not cover all of BC have full_BC = FALSE, and their sum is not checked against the BC total.
#' Default = TRUE.
#' @return List structure with unique values for Year, Type, TypeID, Age and Sex.
#' @examples
#' \dontrun{  dbInfo("I://PopulationR/Database/Estimates/POPHAE19.csv")  }
#' dbInfo(c("estimates", "HA", "19"))
#' dbInfo(c("projections", "RD", "19"))
#' @family database access helpers
#' @author Sebastien Lavoie (formerly, BC Stats)
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @export
dbInfo <- function(db_path, full_BC = TRUE) {

  if(length(db_path) == 1) {                # DB path is a full path
    data <- data.table::fread(db_path, header = TRUE,
                              stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else if(length(db_path) == 3) {         # DB path is type, region, year for getDBPath
    data <- data.table::fread(getDBPath(db_path[1], db_path[2], db_path[3]), header = TRUE,
                              stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else {                                  # Error unrecognized path
    stop("Error: Incorrect db_path format.")
  }

  db_ok <- dbCheck(data, full_BC = full_BC) # Database check
  if(db_ok == FALSE) {                      # Throw error
    warning("Warning: one or more database checks failed. Run `dbutils::dbCheck()` for more information.")
  }

  db_info <- list(unique(data$Year),        # Build list with unique elements
                  unique(data$Type),
                  unique(data$TypeID),
                  unique(data$Age),
                  names(data)[-(1:4)]       # Assumes all Sex cols at end; c("Male", "Female", "Total")
                  )
  names(db_info) <- c("Year", "Type", "TypeID", "Age", "Sex") # Name list elements

  return(db_info)                           # Return list structure

}


#### dbRead ----
#' Read population database
#'
#' @description
#' Reads a population database file (population, births, deaths) in default database format
#' (Year, Type, TypeID, Age, Male, Female, Total) and returns data. Database path can be either
#' a full path to a csv database file with the correct columns, or a vector including the type,
#' region and year of database to be used through \code{\link{getDBPath}}. For example,
#' db_path = c("estimates", "HA", "19") would read the July 1st 2019 population estimates database
#' for Local Health Areas. Format of loaded data can be adjusted to default, long or wide and
#' pre-filtered for specific age groups.
#'
#' Note that \code{\link{dbCheck}} is run before reading the file. User will be warned if any database
#' checks fail.
#'
#' @details
#' The complete list of region IDs and ages for each type of database (population, deaths, births)
#' can be found in \code{\link{dbutils}}. For historical reasons, population data is saved as
#' POPRREYY (population estimates), POPRRPYY (population projections), BIRRRYY (births) or
#' DEARRYY (deaths), where RR is the shorthand for the region code, and YY is the last two digits
#' of the year.
#'
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#'     region code and two-character year OR full path to database csv file.
#' @param return_format Format of returned data.frame. Default is the same columns as default
#'     database format (Year, Type, TypeID, Age, Male, Female, Total), long format gathers sex
#'     as a new column (Year, Type, TypeID, Age, Sex, N), wide format gathers sex as a new column
#'     and spreads ages as columns (Year, Type, TypeID, Sex, 0, 1,... 89, 90+, Total). Allowed
#'     values are "default", "long" or "wide". Default = "default".
#' @param age_filter Filters ages based on input vector before returning data in requested format.
#'     Must be a vector. Default = NULL.
#' @param db_check Logical value whether to run \code{\link{dbCheck}} on data. Default = TRUE. This
#'     should be set to FALSE only if specific database is known to be ok, or to troubleshoot database
#'     errors other than column names (i.e., NOT database error 1a or 1b. That is, database must
#'     have the 7 columns: Year, Type, TypeID, Age, Male, Female, and Total).
#' @param full_BC Logical value whether the region covers all of BC. Those regions (e.g., CMAs) that
#' do not cover all of BC have full_BC = FALSE, and their sum is not checked against the BC total.
#' Default = TRUE.
#' @return A data.frame object in the requested format.
#' @examples
#' dbRead(c("estimates", "HA", "19"), return_format = "wide", age_filter = c(0, 1, 2, 3, 4, -999))
#' \dontrun{   dbRead("I://PopulationR/Database/Estimates/POPHAE19.csv", return_format = "wide",
#'                    age_filter = c(0, 1, 2, 3, 4, -999))  }
#' dbRead(c("births", "HA", "19"), db_check = FALSE)
#' @family database access helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @author Sebastien Lavoie (formerly, BC Stats)
#' @export
dbRead <- function(db_path, return_format = "default", age_filter = NULL, db_check = TRUE, full_BC = TRUE) {

  if(length(db_path) == 1) {                # User entered full path
    data <- data.table::fread(db_path, header = TRUE,
                              stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else if(length(db_path) == 3) {         # Entered information for getDBPath
    data <- data.table::fread(getDBPath(db_path[1], db_path[2], db_path[3]), header = TRUE,
                              stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else {                                  # Error unrecognized path type
    stop("Error: Incorrect db_path format.")
  }

  # Arrange data by Year, ID, Age to ensure it's always the same
  data <- data %>% dplyr::arrange(Year, TypeID, Age)

  if(!is.null(age_filter)) {                # If age_filter is not null
    data_ages <- unique(data$Age)           # Vector of unique ages in database
    for(i in age_filter) {             # Check if ages in age_filter are actual ages in the database
      if(!(i %in% data_ages)) {        # At least one of the rquested ages is not in the database
        stop("Error: At least one of the age_filter ages is not in the database.") # Throw error
      }
    }
    data <- data %>% dplyr::filter(Age %in% age_filter) # Filter data with age_filter
  }

  if(db_check == TRUE) {
    db_ok <- dbCheck(data, full_BC = full_BC)  # Database check
  } else {
    db_ok <- TRUE                              # Set as TRUE to bypass dbCheck
  }

  if(db_ok == TRUE) {
    if(return_format == "long") {           # long format requested: gather value columns as Sex
      data <- data %>% tidyr::pivot_longer(c(Male, Female, Total), names_to = "Sex", values_to = "N")
    } else if(return_format == "wide") {    # wide format requested: gather value columns as Sex, spread Age
      data <- data %>% tidyr::pivot_longer(c(Male, Female, Total), names_to = "Sex", values_to = "N") %>%
        tidyr::pivot_wider(names_from = Age, values_from = N)
    } else if(return_format != "default") { # Unrecognized value
      stop("Error: incorrect return_format value. Argument must be default, long or wide.") # Throw error
    }
    return(data)                            # Return final data
  } else if(db_ok == FALSE) {               # Throw error
    # stop("Error: one or more database checks failed. Run `dbutils::dbCheck()` for more information.")
    warning("Error: one or more database checks failed. Run `dbutils::dbCheck()` for more information.")
  }

}


#### dbWrite ----
#' Write population database
#'
#' @description
#' Writes a population database file (population, births, deaths). Data format is kept consistent
#' with columns always including Year, Type, TypeID, Age, Male, Female, Total, in this order. As
#' well, population databases include all possible region IDs and ages, even if that means a
#' Male/Female/Total with N = 0.
#'
#' Database output path does not have to be specified. The paths are hard-coded to ensure data is
#' kept in the correct location. All that is needed is a database type (population estimates,
#' population projections, deaths or births), the two-digit region code and the last two-digits of
#' the database year based on a July 1st reference date. This is enough to figure out the full path
#' of the database through the \code{\link{getDBPath}} function.
#'
#' Note that \code{\link{dbCheck}} is run before writing the file. User will be warned if any database
#' checks fail.
#'
#' @details
#' The complete list of region IDs and ages for each type of database (population, deaths, births)
#' can be found in \code{\link{dbutils}}. For historical reasons, population data is saved as
#' POPRREYY (population estimates), POPRRPYY (population projections), BIRRRYY (births) or
#' DEARRYY (deaths), where RR is the shorthand for the region code, and YY is the last two digits
#' of the year.
#'
#' @param db Data variable containing the database to be written. Expects data to be in data.frame
#' with columns: Year, Type, TypeID, Age, Male, Female, Total.
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#' region code and two-character year OR full path to database csv file.
#' @param overwrite Whether to overwrite the file if a database already exists. Default = FALSE.
#' @param full_BC Logical value whether the region covers all of BC. Those regions (e.g., CMAs) that
#' do not cover all of BC have full_BC = FALSE, and their sum is not checked against the BC total.
#' Default = TRUE.
#' @examples
#' \dontrun{ dbWrite(data, c("estimates", "CF", "19"), overwrite = FALSE) }
#' \dontrun{ dbWrite(data, "I://PopulationR/Database/Estimates/POPCFE19.csv", overwrite = FALSE) }
#' @family database access helpers
#' @seealso Overall package documentation: \code{\link{dbutils}}()
#' @author Sebastien Lavoie (formerly, BC Stats)
#' @export
dbWrite <- function(db, db_path, overwrite = FALSE, full_BC = TRUE) {

  db_ok <- dbCheck(db, full_BC = full_BC)   # Database check

  if(db_ok == TRUE) {
    if(length(db_path) == 1) {              # DB path is a full path
      if(file.exists(db_path) == TRUE & overwrite == FALSE) {
        stop("Error: File exists and overwrite is FALSE.")
      } else {                              # Write csv database data
        print(paste0("Saving file...", db_path))
        data.table::fwrite(db, file = db_path, sep = ",")
      }
    } else if(length(db_path) == 3) {       # DB path is type, region, year for getDBPath
      db_path <- getDBPath(db_path[1], db_path[2], db_path[3])
      if (file.exists(db_path) == TRUE & overwrite == FALSE) {
        stop("Error: File exists and overwrite is FALSE.")
      } else {                              # Write csv database data
        print(paste0("Saving file...", db_path))
        data.table::fwrite(db, file = db_path, sep = ",")
      }
    } else {                                # Error unrecognized path
      stop("Error: Incorrect db_path format.")
    }
  } else if(db_ok == FALSE) {               # Throw error
    # stop("Error: one or more database checks failed. Run `dbutils::dbCheck()` for more information.")
    warning("Error: one or more database checks failed. Run `dbutils::dbCheck()` for more information.")
  }

}

