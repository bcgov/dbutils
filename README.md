
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbutils

<!-- badges: start -->

use\_lifecycle\_badge(“experimental”) <!-- badges: end -->

`dbutils` is a package of utility functions for processing and modifying
BC Stats’ population databases via three main categories:

  - database **access** functions (getDBPath, dbCheck, dbInfo, dbRead,
    dbWrite)  
  - **conversion** functions (conversionRead, dbConvert)  
  - **raking** functions (dbRake, raking helper functions)

## About

The BC Stats Population section has historically depended on APL; the
`dbutils` package is a conversion of the APL functions to R functions.

**Database** types include: population, deaths, births. Databases must
have 7 columns: Year, Type, TypeID, Age, Male, Female, and Total. Each
Year of data must have the same number of occurrences, and Years must be
continuous (no year can be missing).

There are several **naming conventions** used with such data, where
files are named ***DDDRRTYY***:

  - **DDD** is the demographic type, such as BIR = Birth, DEA = Death,
    POP = Population, etc.  
  - **RR** is the region code, such as:
      - RD - Regional District, which is the same data as Census
        Division (CD)  
      - DR - Development region (DR)  
      - HA - Local Heath Area (LHA)  
      - HS - Health Service Delivera Area (HSDA)  
      - HY - Health Authority (HA)  
      - CH - Community Health Service Area (CHSA)  
      - CF - Ministry of Children and Family Development (MCFD)  
      - CA - MCFD Service Delivery Area (MCFD\_SDA)  
      - CL - MCFD Local Service Area (MCFD\_LSA)  
      - SD - School District (SD)  
      - PS - College Region, or Post-Secondary (CR)  
      - SR - Special Regions
  - **T** is the data type, such as A = Actual, E = Estimates, O =
    Projections, P = Population  
  - **YY** is the year (or two-digit PEOPLE version number, indicating
    which PEOPLE run) you want data for

For example, population data is saved as POPRREYY (population
estimates), POPRRPYY (population projections), BIRRRYY (births) or
DEARRYY (deaths), where RR is the shorthand for the region code, and YY
is the last two digits of the year. (This was passed over from decades
of BC Stats PEOPLE development limited to 2-digit codes.)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bcgov/dbutils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dbutils)
## basic example code
```
