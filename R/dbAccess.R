#### database access functions ----


#### getDBPath ----
#' Returns a correct full database path
#'
#' @description
#' Returns a population database file (population, births, deaths) path. Database paths are hardcoded
#' to ensure consistency across population systems. The script uses data type, region type and year
#' to return an appropriate full path. The complete list of region IDs and ages for each type of
#' database (population, deaths, births) can be found in the dbutils.R script. For historical reasons,
#' population data is saved as POPXXEYY (population estimates), POPXXPYY (population projections),
#' BIRXXYY (births) or DEAXXYY (deaths), where YY is the last two digits of the year. XX represent
#' the shorthand for the region type. This was passed over from decades of BC Stats PEOPLE
#' development limited to 2-digit codes. Two-digits region codes are as follow:
#' \itemize{
#'  \item RD - Regional District, which is the same data as Census Division (CD)
#'  \item DR - Development region (DR)
#'  \item HA - Local Heath Area (LHA)
#'  \item HS - Health Service Delivera Area (HSDA)
#'  \item HY - Health Authority (HA)
#'  \item CH - Community Health Service Area (CHSA)
#'  \item CF - Ministry of Children and Family Development (MCFD)
#'  \item CA - MCFD Service Delivery Area (MCFD_SDA)
#'  \item CL - MCFD Local Service Area (MCFD_LSA)
#'  \item SD - School District (SD)
#'  \item PS - College Region, or Post-Secondary (CR)
#'  \item SR - Special Regions
#'}
#'
#' @param dbType Type of database being written. Possible values are "estimates", "projections",
#' "deaths", "births". Default = "estimates".
#' @param dbRegion Two-digit region code of the database. Possible values: "RD", "DR", "HA", "HS",
#' "HY", "CH", "CF", "CA", "CL", "SD", "PS", "SR" as character. Default = NULL.
#' @param dbYear Two-digit year of the data being saved. Based on July 1st reference date,
#' as character. Default = NULL.
#' @examples
#' getDBPath(dbType = "estimates", dbRegion = "CA", dbYear = "17") ## "//SFP.IDIR.BCGOV/S152/S52007/PopulationR/Database/Estimates/POPCAE17.csv"
#' getDBPath(dbType = "births", dbRegion = "RD", dbYear = "18") ## "//SFP.IDIR.BCGOV/S152/S52007/VITAL/Database/Births/BIRRD18.csv"
#' @family database access helpers
#' @author Sebastien Lavoie, (formerly, BC Stats)
#' @export
getDBPath <- function(dbType, dbRegion, dbYear) {
  if(is.null(dbRegion)) {
    stop("Error: No region type specified.")
  } else if(!(dbRegion %in% c("RD", "DR", "HA", "HS", "HY", "CF", "CA", "CL", "SD", "PS", "SR", "CH"))) {
    stop("Error: Unrecognized region type.")
  }

  if(is.null(dbYear)) {
    stop("Error: No year specified.")
  } else if(!is.character(dbYear) || stringr::str_length(dbYear) != 2) {
    stop("Error: Incorrect dbYear value. Must be two-digit year as character. e.g. '17'")
  }

  if(!(dbType %in% c("estimates", "projections", "births", "deaths"))) {
    stop("Error: Unrecognized database type.")
  }

  if(dbType == "estimates") {
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
#' Checks a database for specific errors. (1) Database must have 7 columns: Year, Type, TypeID, Age,
#' Male, Female, and Total. (2) Each Year of data must have the same number of occurrences.
#' (3) Years of data must be continuous (no year can be missing). (4) The Total column must equal
#' the sum of the Male and Female columns. (5) THIS FUNCTION IS INCOMPLETE.
#'
#' @param db Data variable containing the database to be checked.
#' @param full_BC WHAT THE HECK IS THIS? Default = TRUE.
#' @return db_ok, a logical. If the database passes all checks, db_ok = TRUE. Otherwise, any error(s)
#' will be printed to screen and db_ok will be set to FALSE.
# @examples
#' @family database access helpers
#' @author Sebastien Lavoie, (formerly, BC Stats); Julie Hawkins, BC Stats (remaining)
#' @export
dbCheck <- function(db, full_BC = TRUE) {

  db_OK <- TRUE

  if(length(colnames(db)) != 7) {
    db_OK <- FALSE
    print("Database error: Database does not have the correct number of columns.")
  } else if(min(colnames(db) == c("Year", "Type", "TypeID", "Age", "Male", "Female", "Total")) != 1) {
    db_OK <- FALSE
    print("Database error: Database does not have the correct columns. Make sure all columns are included as follows: Year, Type, TypeID, Age, Male, Female, Total.")
  } else if(min((db %>% dplyr::group_by(Year) %>% dplyr::summarize(count = dplyr::n()))$count) !=
             max((db %>% dplyr::group_by(Year) %>% dplyr::summarize(count = dplyr::n()))$count)) {
    db_OK <- FALSE
    print("Database error: One or more Year has a different number of occurrences than the others. This could be due to duplication or missing region IDs.")
  } else if(max(unique(db$Year)) != max(seq(min(unique(db$Year)), length.out = length(unique(db$Year))))) {
    db_OK <- FALSE
    print("Database error: One or more Year is missing from the database making it a non-continuous sequence.")
  } else if(min((db %>% dplyr::mutate(diff = Total - Male - Female))$diff == 0) != 1) {
    db_OK <- FALSE
    print("Database error: Total column does not equal the sum of Male + Female column.")
  }

  # all regions, ages and sex included even if N = 0,
  # Population DB: sum of 0-110+ = sum 0-90+ = sum -4 to -110 = sum -4 to -90 = -999 for Male and Female
  # Death DB: sum of 0-120+ = sum 0-90+ = sum -4 to -120 = sum -4 to -90 = -999 for Male and Female
  # Birth DB: sum of 15-65+ = sum -19 to -65 = -999 for Male and Female
  # Population, Death, Birth DB for region type with full coverage of BC: sum of all regions = region 0 (BC) for each age and gender

  return(db_OK)
}


#### dbInfo ----
#' Population database dimension information
#'
#' @description
#' Reads a population database file (population, births, deaths) and returns list with unique
#' values for each of Year, Type, TypeID, Age and Sex. Database path can be either a full path
#' to a csv database file with the correct columns, or a vector including the type, region and
#' year of database to be used through \code{\link{getDBPath}}. For example,
#' db_path = c("estimates", "HA", "18") would return info for the July 1st 2018 population
#' estimates database for Local Health Areas.
#'
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#' region code and two-character year OR full path to datbase csv file.
#' @return List structure with unique values for Year, Type, TypeID, Region, Age and Sex.
# @examples
# dbInfo("POPCAE18.csv")
# dbInfo(c("estimates", "HA", "18"))
#' @family database access helpers
#' @author Sebastien Lavoie, (formerly, BC Stats)
#' @export
dbInfo <- function(db_path) {
  if(length(db_path) == 1) {  # DB path is a full path
    data <- data.table::fread(db_path, header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else if(length(db_path) == 3) { # DB path is type, region, year for getDBPath
    data <- data.table::fread(getDBPath(db_path[1], db_path[2], db_path[3]), header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else { # Error unrecognized path
    stop("Error: Incorrect db_path format.")
  }

  db_ok <- dbCheck(data, full_BC = TRUE)  # database check

  if(db_ok == TRUE) {
    db_info <- list(unique(data$Year), # Build list with unique elements
                    unique(data$Type),
                    unique(data$TypeID),
                    unique(data$Age),
                    c("Male", "Female", "Total"))
    names(db_info) <- c("year", "type", "region", "age", "sex") # Name list elements

    return(db_info) # Return list structure
  } else if(db_ok == FALSE) {
    stop("Error: one or more database checks failed.") # Throw error
  }

}


#### dbRead ----
#' Read population database
#'
#' @description
#' Reads a population database file (population, births, deaths) in default database format
#' (Year, Type, TypeID, Age, Male, Female, Total) and returns data. Database path can be either
#' a full path to a csv database file with the correct columns, or a vector including the type,
#' region and year of database to be used through \code{\link{getDBPath}}. For example,
#' db_path = c("estimates", "HA", "18") would read the July 1st 2018 population estimates database
#' for Local Health Areas. Format of loaded data can be adjusted to default, long or wide and
#' pre-filtered for specific age groups.For historical reasons, population data is saved as
#' POPXXEYY (population estimates), POPXXPYY (population projections), BIRXXYY (births) or
#' DEAXXYY (deaths), where YY is the last two digits of the year. XX represent the shorthand for
#' the region type. This was passed over from decades of BC Stats PEOPLE development limited to
#' 2-digit codes. Two-digits region codes are as follow:
#' \itemize{
#'  \item RD - Regional District, which is the same data as Census Division (CD)
#'  \item DR - Development region (DR)
#'  \item HA - Local Heath Area (LHA)
#'  \item HS - Health Service Delivera Area (HSDA)
#'  \item HY - Health Authority (HA)
#'  \item CH - Community Health Service Area (CHSA)
#'  \item CF - Ministry of Children and Family Development (MCFD)
#'  \item CA - MCFD Service Delivery Area (MCFD_SDA)
#'  \item CL - MCFD Local Service Area (MCFD_LSA)
#'  \item SD - School District (SD)
#'  \item PS - College Region, or Post-Secondary (CR)
#'  \item SR - Special Regions
#'}
#'
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#'     region code and two-character year OR full path to datbase csv file.
#' @param return_format Format of returned data.frame. Default is the same columns as default
#'     database format (Year, Type, TypeID, Age, Male, Female, Total), long format gathers sex
#'     as a new column (Year, Type, TypeID, Age, Sex, N), wide format gathers sex as a new column
#'     and spreads ages as columns (Year, Type, TypeID, Sex, 0, 1,... 89, 90+, Total). Allowed
#'     values are "default", "long" or "wide". Default = "default".
#' @param age_filter Filters ages based on input vector before returning data in requested format.
#'     Must be a vector. Default = NULL.
#' @return A data.frame object in the requested format.
# @examples
# dbRead("POPCAE18.csv", return_format = "wide", age_filter = c(0, 1, 2, 3, 4, -999))
# dbRead(c("estimates", "HA", "18"), return_format = "wide", age_filter = c(0, 1, 2, 3, 4, -999))
#' @family database access helpers
#' @author Sebastien Lavoie, (formerly, BC Stats)
#' @export
dbRead <- function(db_path, return_format = "default", age_filter = NULL) {
  if(length(db_path) == 1) { # User entered full path
    data <- data.table::fread(db_path, header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else if(length(db_path) == 3) { # Entered information for getDBPath
    data <- data.table::fread(getDBPath(db_path[1], db_path[2], db_path[3]), header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) # Read csv database data
  } else { # Error unrecognized path type
    stop("Error: Incorrect db_path format.")
  }

  # Arrange data by Year, ID, Age to ensure it's always the same
  data <- data %>% dplyr::arrange(Year, TypeID, Age)

  if(!is.null(age_filter)) { # If age_filter is not null
    data_ages <- unique(data$Age) # Vector of unique ages in database
    for(i in age_filter) { # Check if ages in age_filter are actual ages in the database
      if(!(i %in% data_ages)) { # At least one of the rquested ages is not in the database
        stop("Error: At least one of the age_filter ages is not in the database.") # Throw error
      }
    }
    data <- data %>% dplyr::filter(Age %in% age_filter) # Filter data with age_filter
  }

  db_ok <- dbCheck(data, full_BC = TRUE)  # database check

  if(db_ok == TRUE) {
    if(return_format == "long") {  # long format requested
      # data <- data %>% gather("Sex", "N", Male, Female, Total) # Gather value columns as their respective sex
      data <- data %>% tidyr::pivot_longer(c(Male, Female, Total), names_to = "Sex", values_to = "N") # Gather value columns as their respective sex
    } else if(return_format == "wide") { # Wide format requested
      # data <- data %>% gather("Sex", "N", Male, Female, Total) %>% spread(Age, N)  # Gather value column as sex, spread ages
      data <- data %>% tidyr::pivot_longer(c(Male, Female, Total), names_to = "Sex", values_to = "N") %>%
        tidyr::pivot_wider(names_from = Age, values_from = N)  # Gather value column as sex, spread ages
    } else if(return_format != "default") { # Unrecognized value
      stop("Error: incorrect return_format value. Argument must be default, long or wide.") # Throw error
    }
    return(data)  # Return final data
  } else if(db_ok == FALSE) {
    stop("Error: one or more database checks failed.") # Throw error
  }

}


#### dbWrite ----
#' Write population database
#'
#' @description
#' Writes a population database file (population, births, deaths). Data format is kept consistent
#' with columns always including Year, Type, TypeID, Age, Male, Female, Total, in this order. As
#' well, population databases include all possible region IDs and ages, even if that means a
#' Male/Female/Total with N = 0. The complete list of region IDs and ages for each type of
#' database (population, deaths, births) can be found at the \code{\link{dbutils}}. For historical
#' reasons, population data is saved as POPXXEYY (population estimates), POPXXPYY (population projections),
#' BIRXXYY (births) or DEAXXYY (deaths), where YY is the last two digits of the year. XX represents
#' the shorthand for the region type. This was passed over from decades of BC Stats PEOPLE development
#' limited to 2-digit codes.
#' Two-digits region codes are as follow:
#' \itemize{
#'  \item RD - Regional District, which is the same data as Census Division (CD)
#'  \item DR - Development region (DR)
#'  \item HA - Local Heath Area (LHA)
#'  \item HS - Health Service Delivera Area (HSDA)
#'  \item HY - Health Authority (HA)
#'  \item CH - Community Health Service Area (CHSA)
#'  \item CF - Ministry of Children and Family Development (MCFD)
#'  \item CA - MCFD Service Delivery Area (MCFD_SDA)
#'  \item CL - MCFD Local Service Area (MCFD_LSA)
#'  \item SD - School District (SD)
#'  \item PS - College Region, or Post-Secondary (CR)
#'  \item SR - Special Regions
#'}
#' Database output path does not have to be specified. The paths are hard-coded to ensure data is
#' kept in the correct location. All that is needed is a database type (population estimates,
#' population projections, deaths or births), the two-digit region code and the last two-digits of
#' the database year based on a July 1st reference date. This is enough to figure out the full path
#' of the database through the \code{\link{getDBPath}} function.
#'
#' @param db Data variable containing the database to be written. Expects data to be in data.frame
#' with columns: Year, Type, TypeID, Age, Male, Female, Total.
#' @param db_path Either vector of database type ("estimates", "projections", "births", "deaths"),
#' region code and two-character year OR full path to datbase csv file.
#' @param overwrite Whether to overwrite the file if a database already exists. Default = FALSE.
# @examples
# dbWrite(data, c("estimates", "CA", "17"), overwrite = FALSE)
# dbWrite(data, "POPCAE18.csv", overwrite = FALSE)
#' @family database access helpers
#' @author Sebastien Lavoie, (formerly, BC Stats)
#' @export
dbWrite <- function(db, db_path, overwrite = FALSE) {

  db_ok <- dbCheck(db, full_BC = TRUE)  # database check

  # if (length(colnames(db)) != 7 || min(colnames(db) == c("Year", "Type", "TypeID", "Age", "Male", "Female", "Total")) != 1) {
  #   stop("Error: Database does not have the correct column format. Make sure all columns are included as follows: Year, Type, TypeID, Age, Male, Female, Total.")
  # }

  if(db_ok == TRUE) {
    if(length(db_path) == 1) {  # DB path is a full path
      if(file.exists(db_path) == TRUE & overwrite == FALSE) {
        stop("Error: File exists and overwrite is FALSE.")
      } else {
        print(paste0("Saving file...", db_path))
        data.table::fwrite(db, file = db_path, sep = ",") # Write csv database data
      }
    } else if(length(db_path) == 3) { # DB path is type, region, year for getDBPath
      db_path <- getDBPath(db_path[1], db_path[2], db_path[3])
      if (file.exists(db_path) == TRUE & overwrite == FALSE) {
        stop("Error: File exists and overwrite is FALSE.")
      } else {
        print(paste0("Saving file...", db_path))
        data.table::fwrite(db, file = db_path, sep = ",") # Write csv database data
      }
    } else { # Error unrecognized path
      stop("Error: Incorrect db_path format.")
    }
  } else if(db_ok == FALSE) {
    stop("Error: one or more database checks failed.") # Throw error
  }
}



