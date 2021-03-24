#### raking functions ----


#### read.inputs ----
#' Read xlsx or csv file
#'
#' reads in xlsx or csv input files, detecting those file types from file extension, from inputs
#' folder. This is a helper function used in \code{\link{dbRake}}.
#'
#' @param inputFile is a string of the name of an xlsx or csv file to be read in
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
read.inputs <- function(inputFile) {

  if(stringr::str_detect(inputFile, ".xlsx")) {
    myFile <- openxlsx::read.xlsx(here::here("inputs", inputFile))
  }

  if(stringr::str_detect(inputFile, ".csv")) {
    myFile <- readr::read_csv(here::here("inputs", inputFile))
  }

  if(stringr::str_detect(inputFile, ".xlsx", negate = TRUE) & stringr::str_detect(inputFile, ".csv", negate = TRUE)) {
    message("Please only read in an xlsx or csv file.")
  } else {
    myFile
  }

}


#### rounded ----
#' Round a number
#'
#' For some reason, R rounds to even (i.e., 12.5 rounds to 12), but we want to round to 0
#' (i.e., 12.5 rounds to 13). This is a helper function used in \code{\link{dbRake}}.
#'
#' @param x Number to be rounded
#' @return Rounded number
#' @examples
#' rounded(1.2345)  ## 1
#' rounded(12.5)    ## 13
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
#' @author Stephanie Yurchak, BC Stats
#' @export
rounded <- function(x) { trunc(x + 0.5) }


#### real.to.int ----
#' Converts real numbers to integers
#'
#' Converts real numbers (fractions) to integer numbers while preserving rounded sum.
#' This is a helper function used in \code{\link{dbRake}}.
#'
#' @param realNums a  vector of real (fraction) number
#' @return a vector of integer numbers that sums to the rounded sum of realNums
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
real.to.int <- function(realNums) {

  ## difference between rounded sum of realNums and rounded sum of just integer portion of realNums
  myDiff <- rounded(sum(realNums)) - rounded(sum(floor(realNums)))

  ## create vector of decimals (e.g., 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.10)
  x <- (1:length(realNums))/(100*length(realNums))

  ## randomize decimals, no duplicates (replace = FALSE)
  randNums <- sample(x = x, size = length(realNums), replace = FALSE)

  ## add randomized decimals to fractions
  if(any(class(realNums) == "data.frame")) {
    fracRand <- realNums %>% dplyr::bind_rows(- floor(realNums) + randNums) %>% colSums(.)
  } else {
    ## when realNums is a vector
    fracRand <- realNums - floor(realNums) + randNums
  }

  ## get DECREASING rank order
  sortOrder <- abs(rank(fracRand) - (1+length(realNums)))

  ## get floor of realNums vector
  intNums <- floor(realNums)

  ## add 1 to largest fracRand for number needed to increase to rounded sum of realNums
  for(i in which(sortOrder <= myDiff)) {
    intNums[i] <- floor(realNums[i]) + 1
  }

  ## return integer numbers vector
  intNums

}


#### calc.cols ----
#' Calculate necessary columns
#'
#' calculates necessary columns (Sum, Ctrl_TOTAL, Diff, adj_value) in preparation for prorating rows
#' or raking. First, it calculates the actual sum (of columns), then adds in VarRow control totals,
#' then calculates the difference, and finally calculates the adjustment value (difference divided by
#' number of groups). This is a helper function used in \code{\link{dbRake}}, and before calling \code{\link{prorate.row}}.
#'
#' This function is first called in dbRake Part 1 to update initial initial estimates of male/female
#' regional total values (before prorating rows, and again before raking), then again in Part 2 to
#' update initial 5 year age group and maximum age group estimates by Sex (before prorating rows,
#' and again before raking), and finally in Part 3 to update initial single year of age estimates
#' by Sex (before prorating rows, and again before raking).
#'
#' @param data a dataframe of inital population counts that need to be adjusted to control totals
#' (e.g., columns: a region variable ("VarRow"), 1 (for Males), 2 (for Females))
#' @param temp a dataframe of control totals to adjust data to (e.g., region control totals)
#' @param VarRow the name of the variable to join temp to data (e.g., VarRow for "LHA")
#' @param n_colGrps the number of groups to adjust over (e.g., number of Sexes, 2); this is
#' essentially the number of columns in data minus 1 (for the VarRow column)
#' @return the original dataframe with four new columns (Sum, Ctrl_TOTAL, Diff, adj_value)
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
calc.cols <- function(data, temp, VarRow, n_colGrps) {

  data %>%

    ## calc Sum of VarRow per VarCol (-1 to exclude VarRow column)
    dplyr::mutate(Sum = rowSums(data[ , -1])) %>%

    ## add in VarRow Control Totals, rename as Ctrl_TOTAL
    dplyr::left_join(temp, by = "VarRow") %>%
    dplyr::rename(Ctrl_TOTAL = TOTAL) %>%

    ## calc difference between Sum and Ctrl_TOTAL, and adjusted value
    dplyr::mutate(Diff = Ctrl_TOTAL - Sum,
                  adj_value = Diff / n_colGrps)

}


#### prorate.row ----
#' Adjust data row by row to sum to control totals
#'
#' Reconcile row by row, when using region control totals, while updating initial estimates of
#' male/female regional total values if difference is NOT zero, adjust actual data. This function
#' will be run iteratively through each row of a dataframe already prepared by running through
#' \code{\link{calc.cols}} while the row's difference is not zero. For example, for 1:n_Sex, prorate
#' so that region totals sum to region control totals. This is a helper function used in \code{\link{dbRake}}.
#'
#' @param CurrRow a dataframe of a row to prorate
#' @param n_colGrps the number of groups to adjust over (e.g., number of groups in region)
#' @param allowNegatives a logical of whether negative population values are allowed (usually FALSE)
#' @return original dataframe of row, that is now prorated by the adjustment values
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
prorate.row <- function(CurrRow, n_colGrps, allowNegatives) {

  myVars <- names(CurrRow)[2:(n_colGrps+1)]
  CurrRow <- CurrRow %>% dplyr::mutate(dplyr::across(tidyselect::all_of(myVars), `+`, CurrRow$adj_value))

  ## if negatives are NOT allowed, replace any negative adjusted data with zero
  if(allowNegatives == FALSE) {
    ## loop through data columns 2:(n_colGrps+1)
    for(j in 2:(n_colGrps+1)) {
      if (CurrRow[ ,j] < 0) { CurrRow[ ,j] <- 0 }
    }
  }

  ## calc new (adjusted) sum, new difference, and new adjustment value
  CurrRow$Sum <- sum(CurrRow[ ,2:(n_colGrps+1)])
  CurrRow$Diff <- CurrRow$Ctrl_TOTAL - CurrRow$Sum
  if(abs(CurrRow$Diff) < 0.001) { CurrRow$Diff <- 0 }
  CurrRow$adj_value <- CurrRow$Diff/n_colGrps

  CurrRow

}


#### prep.prorate.col ----
#' Calculate necessary rows
#'
#' calculates necessary rows (Sum, Ctrl_TOTAL, Diff, adj_value) in preparation for prorating rows
#' or raking. First, it calculates the actual sum (of rows), then adds in VarRow control totals,
#' then calculates the difference, and finally calculates the adjustment value (difference divided by
#' number of groups). This is a helper function used in \code{\link{dbRake}}, and before calling \code{\link{prorate.col}}.
#'
#' This function is first called in dbRake Part 1 to update initial initial estimates of male/female
#' regional total values (after prorating rows before prorating columns), then again in Part 2 to
#' update initial 5 year age group and maximum age group estimates by Sex (after prorating rows
#' before prorating columns) for the older population (vector called AgeGrpsOldest (e.g., "75-59"
#' "80-84" "85-89" "90-94" "95-99" "100") determined by AgeGrpMax) and then again for (remaining)
#' younger populations, and finally in Part 3 to update initial single year of age estimates
#' by Sex (after prorating rows before prorating columns).
#'
#' @param data a dataframe of inital population counts that need to be adjusted to control totals
#' (e.g., columns: a region variable ("VarRow"), 1 (for Males), 2 (for Females))
#' @param n_rowGrps the number of groups to adjust over (e.g., number of groups in region)
#' @param colGrps the number of groups to adjust over (e.g., number of Sexes, 2; number of Age
#' Groups); this is essentially the number of columns in data minus 1 (for the VarRow column) or
#' minus 2 (for the VarRow & Sex columns)
#' @param ctrl_total_row a row of control totals to adjust data to (e.g., region control totals)
#' @param AgeGrpMax age of the older population that will be prorated and raked separately from
#' other 5 year age groups. AgeGrpMax will include all ages, including itself, through the remainder
#' of the population. Default = NULL. If AgeGrpMax is not set, the function will use 75 and up
#' (not necessarily the oldest age; that is, the oldest age is usually 100, meaning 100 and up).
#' The BC Stats Demographics team determined that 75 was the best age for AgeGrpMax to ensure that
#' distortion in older populations is minimized. When prep.prorate.col is called in Part 1 & 3,
#' AgeGrpMax is NULL. AgeGrpMax is set to an age when prep.prorate.col is called in Part 2 when
#' prorating older population, but is NULL when prorating remaining younger population.
#' @param ageLast oldest age value. Default = NULL. When prep.prorate.col is called in Part 1 & 3,
#' ageLast is NULL. ageLast is set when prep.prorate.col is called in Part 2 when prorating older
#' population, but is NULL when prorating remaining younger population.
#' @return original dataframe, that is now prorated by the adjustment values
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
prep.prorate.col <- function(data, n_rowGrps, colGrps, ctrl_total_row, AgeGrpMax = NULL, ageLast = NULL){

  ## Step 0: get appropriate rows and columns
  out <- data[1:n_rowGrps, colGrps]

  ## Step 1: calc actual sum, add in VarRow control totals, calc difference
  out[nrow(out)+1, 1] <- "Sum"
  ## if running on age columns
  if(!is.null(AgeGrpMax) & !is.null(ageLast)) {
    ## ALSO if AgeGrpsMax is a single (non-grouped) age, use 'sum()'
    if(AgeGrpMax == ageLast) {
      out[nrow(out), -1] <- sum(out[, -1], na.rm = TRUE)
    } else {
      ## use colSums() to sum across multiple age columns
      out[nrow(out), -1] <- as.list(colSums(out[, -1], na.rm = TRUE))
    }
  } else {
    ## if running on columns other than age, use colSums() to sum across multiple columns (e.g., sex)
    out[nrow(out), -1] <- as.list(colSums(out[, -1], na.rm = TRUE))
  }

  out[nrow(out)+1, ] <- ctrl_total_row

  out[nrow(out)+1, 1] <- "Diff"
  out[nrow(out), -1] <- (out[which(out[, 1] == "Ctrl_TOTAL"), -1]
                         - out[which(out[, 1] == "Sum"), -1])

  ## Step 2: calc adjustment value (difference divided by number of groups)
  out[nrow(out)+1, 1] <- "adj_value"
  out[nrow(out), -1] <- (out[which(out[, 1] == "Diff"), -1] / n_rowGrps)

  out

}


#### prorate.col ----
#' Adjust data column by column to sum to row totals
#'
#' Reconcile column by column, when using region control totals, while updating initial estimates of
#' male/female regional total values if difference is NOT zero, adjust actual data. This function
#' will be run iteratively through each column of a dataframe already prepared by running through
#' \code{\link{prep.prorate.col}} while the column's difference is not zero. For example, for 1:n_Sex,
#' prorate so that region totals sum to region control totals. This is a helper function used in \code{\link{dbRake}}.
#'
#' @param CurrCol a dataframe of a column to prorate
#' @param n_rowGrps the number of groups to adjust over (e.g., number of groups in region)
#' @param allowNegatives a logical of whether negative population values are allowed (usually FALSE)
#' @return original dataframe of column, that is now prorated by the adjustment values
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
prorate.col <- function(CurrCol, n_rowGrps, allowNegatives) {

  myCol <- colnames(CurrCol)[2]
  adj_value <- CurrCol %>% dplyr::filter(VarRow == "adj_value") %>%
    dplyr::select(tidyselect::all_of(myCol)) %>% dplyr::pull()

  CurrCol[1:n_rowGrps, -1] <- CurrCol[1:n_rowGrps, -1] + adj_value
  # CurrCol[1:n_rowGrps, ] <- CurrCol[1:n_rowGrps, ] %>% dplyr::mutate(dplyr::across(tidyselect::all_of(myCol), `+`, adj_value))
  # CurrCol[1:n_rowGrps, -1] <- CurrCol[1:n_rowGrps, -1] + CurrCol[which(CurrCol[, 1] == "adj_value"), -1]

  ## if negatives are NOT allowed, replace any negative adjusted data with zero
  if(allowNegatives == FALSE) {
    ## loop through data rows 1:n_rowGrps
    for(j in 1:n_rowGrps) {
      if (CurrCol[j, -1] < 0) { CurrCol[j, -1] <- 0 }
    }
  }

  ## calc new (adjusted) sum, new difference, and new adjustment value
  CurrCol[which(CurrCol[, 1] == "Sum"), -1] <- sum(CurrCol[1:n_rowGrps, -1], na.rm = TRUE)
  CurrCol[which(CurrCol[, 1] == "Diff"), -1] <- (CurrCol[which(CurrCol[, 1] == "Ctrl_TOTAL"), -1]
                                                 - CurrCol[which(CurrCol[, 1] == "Sum"), -1])
  if(abs(CurrCol[which(CurrCol[, 1] == "Diff"), -1]) < 0.001) { CurrCol[which(CurrCol[, 1] == "Diff"), -1] <- 0 }
  CurrCol[which(CurrCol[, 1] == "adj_value"), -1] <- (CurrCol[which(CurrCol[, 1] == "Diff"), -1] / n_rowGrps)

  CurrCol

}


#### add.random.fraction.to.cols ----
#' Add random fraction for sorting
#'
#' Add a random number to a specified column, "my_col", then sort rows based on my_col, with the
#' random fraction used to break any ties. This is a helper function used in \code{\link{dbRake}}
#' withiin the raking algorithm functions (\code{\link{allowNegsnoMargin}}, \code{\link{noNegsnoMargin}},
#' and \code{\link{noNegsneedMargin}}), when there are more than two row groups (e.g., Regions, 5-year
#' Age Groups, Ages, more than two sexes (when Stats Can adds more than Male and Female)).
#'
#' @param df a dataframe (e.g., VarRow (e.g., for "LHA"), whichever columns being raked over, "Min")
#' @param my_col a column in df (i.e., "Min" which is the minimum value for each row). This is the
#' column that the rows need to be sorted by. That is, the df needs to be sorted with the VarRow with
#' the smallest minimum value to the one with the largest minimum value, in preparation for raking.
#' @return original dataframe without original my_col, but with a new sort_rows column with the
#' order needed to sort rows
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
add.random.fraction.to.cols <- function(df, my_col) {

  ## assumes df has VarRow as first col, multiple columns, then my_col which is col needing to sort rows by
  df <- dplyr::rename(df, this_col = {{my_col}})

  ## 1. create random column, based on number of rows in df
  df$random <- sample(x = (1:dim(df)[1]), size = dim(df)[1], replace = FALSE)

  ## 2. add random numbers as fractions to col needing to break tie (so my_col will be considered first then random to break ties)
  upPower10 <- 10*(10^ceiling(log10(dim(df)[1])))  ## round up to the nearest power of 10
  df$col_rand <- (df$this_col + df$random/upPower10)

  ## 3. sort rows in descending order col_rand, so adjustments always made to largest row
  ## subtract rank (where 1 = smallest) from (1 + dim(df)[1]) so 1 = largest
  df$sort_rows <- 1 + dim(df)[1] - rank(df$col_rand)

  ## 4. delete unnecessary columns
  df$random <- NULL
  df$col_rand <- NULL
  df$this_col <- NULL

  ## return df with new sort_rows column replacing original my_col
  df
}


#### raking algorithm function A: allowNegsnoMargin ----
#' Raking Algorithm Function A: when negative values are allowed
#'
#' When raking, the appropriate algorithm is chosen (negative values allowed or not, margin needed
#' or not), "whole people" adjustments to be made to selected cells in a row are calculated, then
#' "residual people" adjustments to be made to selected cells in a row are calculated, then column
#' control totals are reconciled. Raking is run iteratively row-by-row. Regardless of the algorithm
#' needed, five arguments are needed. A sixth argument, "needMargin" is only needed for algorithm C
#' (\code{\link{noNegsneedMargin}}). This is a helper function used in \code{\link{dbRake}} when
#' negative values ARE allowed.
#'
#' @param CurrRow a subset of data with 3 rows and columns: XXX, n_colGrps, Sum, Ctrl_TOTAL,
#' Diff, adj_value, where XXX is Sex in Part 1, Region in Part 2, and Age in Part 3. The rows
#' are the one XXX row currently being worked on, Adjustments, and AdjCurrRow.
#' @param CurrRow_value a counter to iterate through rows in data
#' @param data a dataframe of rows to be iteratively raked with columns: XXX,  n_colGrps, Sum,
#' Ctrl_TOTAL, Diff, adj_value, where XXX is the variable being raked (Sex in Part 1, Region in Part 2,
#' Age in Part 3). Rows are the rows of XXX as well as three summary rows: Sum, Ctrl_TOTAL, Diff.
#' @param n_colGrps the number of column groups to rake over (e.g., 89 LHAs, number of Age Groups, etc.)
#' @param n_rowGrps the number of rows groups to rake over (e.g., 2 Sexes, 89 LHAs, etc.)
#' @param RowAdj vector of zeros, of length n_colGrps (e.g., 89 zeros, etc.)
#' @return original dataframe, but with CurrRow's Diff now 0, and any adjustments made to CurrRow
#' reversed in the row below that has the largest minimum value
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
allowNegsnoMargin <- function(CurrRow, CurrRow_value, data, n_colGrps, n_rowGrps, RowAdj) {

  ### ********************** A. when negatives are allowed ********************** ###

  ### ** calc "whole people" adjustments to be made to all cells in the row (see p.12)

  ## Get column number(s) of valid groups (i.e., all groups)
  valid_grps <- (1:n_colGrps) + 1

  ## calc number of "whole people" to adjust by (multiply by sign (neg or pos) b/c take abs of Diff)
  adj_whole <- floor(abs(CurrRow$Diff[1]) / length(valid_grps)) * sign(CurrRow$Diff[1])

  ## no looping needed, as all groups are valid b/c negatives are allowed
  CurrRow[2, 2:(n_colGrps+1)] <- adj_whole

  ## determine # people actually allocated
  ## (when CurrRow < adj_whole, not all of adj_whole will be allocated)
  sum_adj <- sum(CurrRow[2, 2:(n_colGrps+1)])

  ## update Diff
  CurrRow[1, "Diff"] <- CurrRow[1, "Diff"] - sum_adj

  ## update AdjCurrRow row (i.e., AdjCurrRow = AdjCurrRow + Adjustments), which is subtracting, as adj_whole is neg
  CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[3, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

  ### ** calc "residual people" adjustments to be made to selected cells in row
  ### (valid cells to adjust if negatives values are allowed) (see p.16)

  ## (FYI: below process is the same as when negs are not allowed and values need to be taken away)
  ## add adjustments to date to original cell values (i.e., replace orig row with AdjCurrRow)
  CurrRow[1, 2:(n_colGrps + 1)] <- CurrRow[3, 2:(n_colGrps + 1)]

  ## update valid groups (i.e., updated CurrRow[1,] > 0)
  ## p.16: "Note that no adjusments are made to cells that have a population of 0." Negs allowed, so why not?
  valid_grps <- which(CurrRow[1, 2:(n_colGrps+1)] > 0) + 1

  ## add in blank NA Margin row
  CurrRow[4, 1] <- "Margin"
  CurrRow[4, 2:(n_colGrps+1)] <- NA

  ## create random numbers to sort descending (updated) AdjCurrRow row in case 2+ are the same
  CurrRow[5, 1] <- "Random"
  CurrRow[5, 2:(n_colGrps+1)] <- as.list(sample(x = (1:n_colGrps), size = n_colGrps, replace = FALSE))

  ## add random numbers as fractions to AdjCurrRow values (so AdjCurrRow will be considered first and random to break ties)
  upPower10 <- 10*(10^ceiling(log10(n_colGrps))) ## round up to the nearest power of 10
  CurrRow[6, 1] <- "AdjCurrRow + Random"
  CurrRow[6, 2:(n_colGrps+1)] <- CurrRow[3, 2:(n_colGrps+1)] + (CurrRow[5, 2:(n_colGrps+1)])/upPower10

  ## sort cells in descending order AdjCurrRow size, so adjustments always made to largest margins
  ## (use unlist() to get CurrRow row as a non-dataframe vector)
  ## subtract rank (where 1 = smallest) from (1+n_colGrps) so 1 = largest
  CurrRow[7, 1] <- "Sort"
  CurrRow[7, 2:(n_colGrps+1)] <- as.list(1 + n_colGrps - rank(unlist(CurrRow[6, 2:(n_colGrps+1)])))

  ## only adjust cells for which we have residual people (i.e., adjust where AdjCurrRow largest)
  for(i in which(CurrRow[7, ] <= abs(CurrRow$Diff[1]))) {
    CurrRow[1, i] <- CurrRow[1, i] + (1*sign(CurrRow$Diff[1]))
    CurrRow[2, i] <- CurrRow[2, i] + (1*sign(CurrRow$Diff[1]))  ## also update Adjustments row to adjust RowAdj for end
  }
  ## check:  sum(CurrRow[1, 2:(n_colGrps+1)]) == CurrRow$Ctrl_TOTAL[1]

  ## update row 1's columns: Sum & Diff, and RowAdj
  CurrRow[1, "Sum"] <- sum(CurrRow[1, 2:(n_colGrps+1)])
  CurrRow[1, "Diff"] <- CurrRow[1, "Sum"] - CurrRow[1, "Ctrl_TOTAL"]
  RowAdj <- RowAdj + unlist(CurrRow[2, 2:(n_colGrps+1)])

  ### ** reconcile column control totals (p.18-23)
  ## p.18&19: Negatives allowed, values added to current row, subtract values from lower rows
  ## p.20: Negatives allowed, values subtracted from current row, add values to lower rows

  ## replace original data row with updated CurrRow[1,]
  data[CurrRow_value, ] <- CurrRow[1, ]

  ## clean up
  rm(upPower10, i, CurrRow)

  ## update CurrRow_value
  CurrRow_value <-  CurrRow_value + 1

  if(n_rowGrps == 2) {
    ## if only two Sexes, reconciling is literally subtracting RowAdj (which should = updated Diff) from other Sex

    ## drop unneeded columns at end: Ctrl_TOTAL, Diff
    data$Ctrl_TOTAL <- NULL
    data$Diff <- NULL

    ## update Sum and Diff rows
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- colSums(data[1:n_rowGrps, 2:(n_colGrps+1)])
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)]
                                                     - data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])   ## RowAdj

    ## subtract Diff from other Sex (i.e., row above Sum row)
    data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)] <- (data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)]
                                                             - data[data$VarRow == "Diff", 2:(n_colGrps+1)])

    ## check that this adjustment makes Diff now all equal 0
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- colSums(data[1:n_rowGrps, 2:(n_colGrps+1)])
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)]
                                                     - data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])

  }

  if(n_rowGrps > 2) {
    ## TO-DO: add this in
    stop("The 'dbRake' function does not have functionality yet to handle more than 2 groups (e.g., Sexes) when negatives are allowed.")
  }

  #### ************************************ Check Point ************************************** ####
  ## At this point, columns (Regions) should sum to their control totals, AND
  ## rows (Sexes) should sum to their control totals
  # sum(data[data$Sex == "Sum", 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]
  # sum(data[1, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]
  # sum(data[2, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]
  # sum(data[data$Sex == "Diff", -1])  ## should be zero
  #### ************************************************************************************** ####

  data
}


#### raking algorithm function B: noNegsnoMargin ----
#' Raking Algorithm Function B: when negative values are NOT allowed and NO margin is needed
#'
#' When raking, the appropriate algorithm is chosen (negative values allowed or not, margin needed
#' or not), "whole people" adjustments to be made to selected cells in a row are calculated, then
#' "residual people" adjustments to be made to selected cells in a row are calculated, then column
#' control totals are reconciled. Raking is run iteratively row-by-row. Regardless of the algorithm
#' needed, five arguments are needed. A sixth argument, "needMargin" is only needed for algorithm C
#' (\code{\link{noNegsneedMargin}}). This is a helper function used in \code{\link{dbRake}} when
#' negative values are NOT allowed AND no margin is needed. Specifically, values need to be
#' subtracted from cells in current row, and added to lower rows. Because adjustments are added to
#' lower rows, no margin is needed.
#'
#' @param CurrRow a subset of data with 3 rows and columns: XXX, n_colGrps, Sum, Ctrl_TOTAL,
#' Diff, adj_value, where XXX is Sex in Part 1, Region in Part 2, and Age in Part 3. The rows
#' are the one XXX row currently being worked on, Adjustments, and AdjCurrRow.
#' @param CurrRow_value a counter to iterate through rows in data
#' @param data a dataframe of rows to be iteratively raked with columns: XXX,  n_colGrps, Sum,
#' Ctrl_TOTAL, Diff, adj_value, where XXX is the variable being raked (Sex in Part 1, Region in Part 2,
#' Age in Part 3). Rows are the rows of XXX as well as three summary rows: Sum, Ctrl_TOTAL, Diff.
#' @param n_colGrps the number of column groups to rake over (e.g., 89 LHAs, number of Age Groups, etc.)
#' @param n_rowGrps the number of rows groups to rake over (e.g., 2 Sexes, 89 LHAs, etc.)
#' @param RowAdj vector of zeros, of length n_colGrps (e.g., 89 zeros, etc.)
#' @return original dataframe, but with CurrRow's Diff now 0, and any adjustments made to CurrRow
#' reversed in the row below that has the largest minimum value
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
noNegsnoMargin <- function(CurrRow, CurrRow_value, data, n_colGrps, n_rowGrps, RowAdj) {

  ### ************************ B. when needMargin is FALSE *********************** ###
  ### ** Subtract values from cells in current row, add to lower rows, negs not allowed ** ###

  ### ** calc "whole people" adjustments to be made to all cells in the row (see p.13 & 14)

  ## Get column number(s) of valid groups (i.e., Adjusted Region > 0)
  valid_grps <- which(CurrRow[3, 2:(n_colGrps+1)] > 0) + 1

  ## For adjustments, repeat looping until Diff < number of valid_grps (i.e., until no "whole" people left)
  while(abs(CurrRow[1, "Diff"]) >= length(valid_grps) & length(valid_grps != 0) ) {

    ## (re)set Adjustments row to zero
    CurrRow[2, 2:(n_colGrps+1)] <- 0

    ## calc number of "whole people" who need to be subtracted from each valid group
    adj_whole <- (floor(abs(CurrRow$Diff[1]) / length(valid_grps))) * sign(CurrRow$Diff[1])

    ## fill adjustment row CurrRow[2, ], with minimum of AdjCurrRow or adj_whole, IFF a valid_grp
    for(a in valid_grps) {
      ## if AdjCurrRow (i.e., CurrRow[3, ]) < abs(adj_whole), only take AdjCurrRow amount, else take adj_whole
      if (CurrRow[3, a] < abs(adj_whole)) {
        CurrRow[2, a] <- CurrRow[3, a] * sign(CurrRow$Diff[1])
      } else {
        ## set adjustment
        CurrRow[2, a] <- adj_whole
      }
    }

    ## determine # people actually allocated
    ## (when CurrRow < abs(adj_whole), not all of adj_whole will be allocated)
    sum_adj <- sum(CurrRow[2, 2:(n_colGrps+1)])

    ## update Diff
    CurrRow[1, "Diff"] <- CurrRow[1, "Diff"] - sum_adj

    ## update AdjCurrRow row (i.e., AdjCurrRow = AdjCurrRow + Adjustments), which is subtracting, as adj_whole is neg
    CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[3, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

    ## update RowAdj
    RowAdj <- RowAdj + unlist(CurrRow[2, 2:(n_colGrps+1)])

    ## update valid groups (i.e., updated Margin > 0)
    valid_grps <- which(CurrRow[3, 2:(n_colGrps+1)] > 0) + 1

  }

  ### ** calc "residual people" adjustments to be made to selected cells in row
  ### (valid cells to adjust if values need to be taken away, negatives not allowed) (see p.16)

  ## add adjustments to date to original cell values (i.e., replace orig row with AdjCurrRow)
  CurrRow[1, 2:(n_colGrps + 1)] <- CurrRow[3, 2:(n_colGrps + 1)]

  ## add in blank NA Margin row
  CurrRow[4, 1] <- "Margin"
  CurrRow[4, 2:(n_colGrps+1)] <- NA

  ## create random numbers to sort descending (updated) AdjCurrRow row in case 2+ are the same
  CurrRow[5, 1] <- "Random"
  CurrRow[5, 2:(n_colGrps+1)] <- as.list(sample(x = (1:n_colGrps), size = n_colGrps, replace = FALSE))

  ## add random numbers as fractions to AdjCurrRow values (so AdjCurrRow will be considered first and random to break ties)
  upPower10 <- 10*(10^ceiling(log10(n_colGrps))) ## round up to the nearest power of 10
  CurrRow[6, 1] <- "AdjCurrRow + Random"
  CurrRow[6, 2:(n_colGrps+1)] <- CurrRow[3, 2:(n_colGrps+1)] + (CurrRow[5, 2:(n_colGrps+1)])/upPower10

  ## sort cells in descending order AdjCurrRow size, so adjustments always made to largest margins
  ## (use unlist() to get CurrRow row as a non-dataframe vector)
  ## subtract rank (where 1 = smallest) from (1+n_colGrps) so 1 = largest
  CurrRow[7, 1] <- "Sort"
  CurrRow[7, 2:(n_colGrps+1)] <- as.list(1 + n_colGrps - rank(unlist(CurrRow[6, 2:(n_colGrps+1)])))

  ## only adjust cells for which we have residual people (i.e., subtract where AdjCurrRow largest)
  for(i in which(CurrRow[7, ] <= abs(CurrRow$Diff[1]))) {
    CurrRow[1, i] <- CurrRow[1, i] - 1
    CurrRow[2, i] <- CurrRow[2, i] - 1  ## also update Adjustments row to add to RowAdj for end
  }
  ## check:  sum(CurrRow[1, 2:(n_colGrps+1)]) == CurrRow$Ctrl_TOTAL[1]

  ## update row 1's columns: Sum & Diff, and RowAdj
  CurrRow[1, "Sum"] <- sum(CurrRow[1, 2:(n_colGrps+1)])
  CurrRow[1, "Diff"] <- CurrRow[1, "Sum"] - CurrRow[1, "Ctrl_TOTAL"]
  ## do NOT Add RowAdj back to self as this has been accounted for by adding residual to whole adjustments in for loop above
  RowAdj <- unlist(CurrRow[2, 2:(n_colGrps+1)])

  ### ** reconcile column control totals (p.18-23)
  ## p.22&23: Negatives not allowed, values subtracted from current row, add values to lower rows

  ## replace original data row with updated CurrRow[1,]
  data[CurrRow_value, ] <- CurrRow[1, ]

  ## clean up
  rm(upPower10, i, CurrRow)

  ## update original data Sum & Diff rows
  data[which(data[ ,1] == "Sum"), 2:(n_colGrps+1)] <- as.list(colSums(data[1:n_rowGrps, 2:(n_colGrps+1)]))
  data[which(data[ ,1] == "Diff"), 2:(n_colGrps+1)] <- (data[which(data[ ,1] == "Sum"), 2:(n_colGrps+1)]
                                                        - data[which(data[ ,1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)])

  ## update CurrRow_value
  CurrRow_value <- CurrRow_value + 1

  if(n_rowGrps == 2) {
    ### p.18-23: Reconcile column control totals (when only 2 rows)
    ## if only two Sexes, reconciling is literally subtracting RowAdj (which should = updated Diff) from other Sex

    ## update Sum and Diff rows
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- as.list(colSums(data[1:n_rowGrps, 2:(n_colGrps+1)]))
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)] -
                                                       data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])  ## i.e., RowAdj

    ## subtract Diff from other Sex (i.e., row above Sum row)
    data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)] <- (data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)] -
                                                               data[data$VarRow == "Diff", 2:(n_colGrps+1)])

    ## check that this adjustment makes Diff now all equal 0
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- as.list(colSums(data[1:n_rowGrps, 2:(n_colGrps+1)]))
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)]
                                                     - data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])

  }

  if(n_rowGrps > 2) {
    ### p.18-23: Reconcile column control totals (when more than 2 rows)
    ## p.22&23: Negatives not allowed, values subtracted from current row, add values to lower rows
    ## i.e., sum(RowAdj) is negative
    ## p.18 "In our example, adjustments were made to all columns, so all columns will have to have
    ## values adjusted in lower rows to ensure columns once again sum to column control totals."

    ## 1. make temp with all non-adjusted data rows and subtract RowAdj (which is neg) from each row (p.22)
    temp <- data[CurrRow_value:n_rowGrps, 1:(n_colGrps+1)]

    adj_matrix <- rbind(RowAdj, RowAdj, deparse.level = 0)
    while (dim(adj_matrix)[1] < dim(temp)[1]) {
      adj_matrix <- rbind(adj_matrix, RowAdj, deparse.level = 0)
    }
    temp[, -1] <- temp[, -1] - adj_matrix
    rm(adj_matrix)

    ## 2. subtract temp from Ctrl_TOTAL, for each row (p.23, bullet 1)
    ## create adjustment matrix to subtract from temp data
    Ctrl_TOTAL <- data[which(data[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)]
    if (dim(temp)[1] > 1) {
      adj_matrix <- rbind(Ctrl_TOTAL, Ctrl_TOTAL, deparse.level = 0)
    } else {
      adj_matrix <- rbind(Ctrl_TOTAL, deparse.level = 0)
    }
    while (dim(adj_matrix)[1] < dim(temp)[1]) {
      adj_matrix <- rbind(adj_matrix, Ctrl_TOTAL, deparse.level = 0)
    }
    temp[, -1] <- adj_matrix - temp[, -1]
    rm(adj_matrix, Ctrl_TOTAL)

    ## 3. determine min value in each row (p.23, bullet 2)
    temp$Min <- apply(temp[, -1], 1, min, na.rm = TRUE)

    ## 4. get row sort order descending by Min column (using random fractional part to break ties)
    temp <- add.random.fraction.to.cols(df = temp, my_col = "Min")
    maxVarRow <- paste0(temp[(which(temp$sort_rows == min(temp$sort_rows))), 1])  ## i.e., VarRow (col 1) with largest Min value
    rm(temp)

    ## 5. change only the maxVarRow (i.e., subtract RowAdj from only maxVarRow); data[ ,1] is data$VarRow
    data[which(data[ ,1] == maxVarRow), 2:(n_colGrps + 1)] <- (data[which(data[ ,1] == maxVarRow), 2:(n_colGrps + 1)]
                                                               - RowAdj)

    ## 6. adjust that one row's Sum and Diff
    data$Sum[which(data[ ,1] == maxVarRow)] <- sum(data[which(data[ ,1] == maxVarRow), 2:(n_colGrps + 1)])
    data$Diff[which(data[ ,1] == maxVarRow)] <- (data$Ctrl_TOTAL[which(data[ ,1] == maxVarRow)]
                                                 - data$Sum[which(data[ ,1] == maxVarRow)])

    ## 7. update (again) original data Sum & Diff rows
    data[which(data[ ,1] == "Sum"), 2:(n_colGrps+1)] <- as.list(colSums(data[1:n_rowGrps, 2:(n_colGrps+1)]))
    data[which(data[ ,1] == "Diff"), 2:(n_colGrps+1)] <- (data[which(data[ ,1] == "Sum"), 2:(n_colGrps+1)]
                                                          - data[which(data[ ,1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)])

  }

  #### ************************************ Check Point ************************************** ####
  ## At this point, columns (Regions) should sum to their control totals, AND
  ## rows (Sexes) should sum to their control totals
  # sum(data[data$VarRow == "Sum", 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]
  # sum(data[1, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]
  # sum(data[2, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]
  # sum(data[data$VarRow == "Diff", -1])  ## should be zero
  #### ************************************************************************************** ####

  data
}


#### raking algorithm function C: noNegsneedMargin ----
#' Raking Algorithm Function C: when negative values are NOT allowed and margin IS needed
#'
#' When raking, the appropriate algorithm is chosen (negative values allowed or not, margin needed
#' or not), "whole people" adjustments to be made to selected cells in a row are calculated, then
#' "residual people" adjustments to be made to selected cells in a row are calculated, then column
#' control totals are reconciled. Raking is run iteratively row-by-row. Regardless of the algorithm
#' needed, five arguments are needed. A sixth argument, "needMargin" is only needed for algorithm C
#' (noNegsneedMargin). This is a helper function used in \code{\link{dbRake}} when negative values
#' are NOT allowed AND margin IS needed. Specifically, values need to be added to cells in current
#' row, and subtracted from lower rows. Because adjustments are subtracted from lower rows but
#' negative values are not allowed, a margin is needed.
#'
#' @param CurrRow a subset of data with 3 rows and columns: XXX, n_colGrps, Sum, Ctrl_TOTAL,
#' Diff, adj_value, where XXX is Sex in Part 1, Region in Part 2, and Age in Part 3. The rows
#' are the one XXX row currently being worked on, Adjustments, and AdjCurrRow.
#' @param CurrRow_value a counter to iterate through rows in data
#' @param data a dataframe of rows to be iteratively raked with columns: XXX,  n_colGrps, Sum,
#' Ctrl_TOTAL, Diff, adj_value, where XXX is the variable being raked (Sex in Part 1, Region in Part 2,
#' Age in Part 3). Rows are the rows of XXX as well as three summary rows: Sum, Ctrl_TOTAL, Diff.
#' @param n_colGrps the number of column groups to rake over (e.g., 89 LHAs, number of Age Groups, etc.)
#' @param n_rowGrps the number of rows groups to rake over (e.g., 2 Sexes, 89 LHAs, etc.)
#' @param RowAdj vector of zeros, of length n_colGrps (e.g., 89 zeros, etc.)
#' @param needMargin a logical (TRUE or FALSE) of whether a margin is needed. The margin is the
#' current row's Sum - CurrRow values - Adjustments - all rows above.
#' @return original dataframe, but with CurrRow's Diff now 0, and any adjustments made to CurrRow
#' reversed in the row below that has the largest minimum value
#' @family raking helpers
#' @seealso The overall raking function: \code{\link{dbRake}}()
noNegsneedMargin <- function(CurrRow, CurrRow_value, data, n_colGrps, n_rowGrps, RowAdj, needMargin) {

  ### ************************* C. when needMargin is TRUE *********************** ###
  ### ** Add values to cells in current row, subtract from lower rows, negs not allowed ** ###

  ## additional set-up for when needMargin is TRUE
  if(needMargin == TRUE) {

    ## if needed, add in Margin (Margin = Sum - CurrRow values - Adjustments - all rows above)
    CurrRow[4, 1] <- "Margin"
    CurrRow[4, 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)] -
                                      data[CurrRow_value, 2:(n_colGrps+1)] -
                                      CurrRow[2, 2:(n_colGrps+1)])

    ## when CurrRow_value > 1, also subtract all rows above in data
    if(CurrRow_value > 1) {
      for(aa in 1:(CurrRow_value-1)) {
        CurrRow[4, 2:(n_colGrps+1)] <- (CurrRow[4, 2:(n_colGrps+1)] - data[aa, 2:(n_colGrps+1)])
      }
    }

  }

  ### ** calc "whole people" adjustments to be made to all cells in the row (see p.14 & 15)

  ## Get column number(s) of valid groups (i.e., Margin > 0)
  valid_grps <- which(CurrRow[4, 2:(n_colGrps+1)] > 0) + 1

  ## For adjustments, repeat looping until Diff < number of valid_grps (i.e., until no "whole" people left)
  while(CurrRow[1, "Diff"] >= length(valid_grps) & length(valid_grps != 0) ) {

    ## (re)set Adjustments row to zero
    CurrRow[2, 2:(n_colGrps+1)] <- 0

    ## calc number of "whole people" who need to be added to each valid group and removed from other row(s)
    adj_whole <- floor(CurrRow$Diff[1] / length(valid_grps))

    ## fill adjustment row CurrRow[2, ], with minimum of Margin or adj_whole, IFF a valid_grp
    for(a in valid_grps) {
      ## if Margin (i.e., CurrRow[4, ]) < adj_whole, only take Margin amount, else take adj_whole
      if (CurrRow[4, a] < adj_whole) {
        CurrRow[2, a] <- CurrRow[4, a] * sign(CurrRow$Diff[1])
      } else {
        ## set adjustment
        CurrRow[2, a] <- adj_whole
      }
    }

    ## determine # people actually allocated
    ## (when CurrRow < adj_whole, not all of adj_whole will be allocated)
    sum_adj <- sum(CurrRow[2, 2:(n_colGrps+1)])

    ## update Diff
    CurrRow[1, "Diff"] <- CurrRow[1, "Diff"] - sum_adj

    ## update Margin row (i.e., Margin = Margin - Adjustments)
    CurrRow[4, 2:(n_colGrps+1)] <- CurrRow[4, 2:(n_colGrps+1)] - CurrRow[2, 2:(n_colGrps+1)]

    ## update AdjCurrRow row (i.e., AdjCurrRow = AdjCurrRow + Adjustments)
    CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[3, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

    ## update RowAdj
    RowAdj <- RowAdj + unlist(CurrRow[2, 2:(n_colGrps+1)])

    ## update valid groups (i.e., updated Margin > 0)
    valid_grps <- which(CurrRow[4, 2:(n_colGrps+1)] > 0) + 1

  }

  ### ** calc "residual people" adjustments to be made to selected cells in row
  ### (valid cells to adjust if values need to be added, negatives not allowed) (see p.17)

  ## add adjustments to date to original cell values (i.e., replace orig row with AdjCurrRow)
  CurrRow[1, 2:(n_colGrps + 1)] <- CurrRow[3, 2:(n_colGrps + 1)]

  ## create random numbers to sort descending (updated) margin row in case 2+ are the same
  CurrRow[5, 1] <- "Random"
  CurrRow[5, 2:(n_colGrps+1)] <- as.list(sample(x = (1:n_colGrps), size = n_colGrps, replace = FALSE))

  ## add random numbers as fractions to margin (so margin will be considered first and random to break ties)
  upPower10 <- 10*(10^ceiling(log10(n_colGrps))) ## round up to the nearest power of 10
  CurrRow[6, 1] <- "Margin + Random"
  CurrRow[6, 2:(n_colGrps+1)] <- (CurrRow[5, 2:(n_colGrps+1)])/upPower10 + CurrRow[4, 2:(n_colGrps+1)]

  ## sort cells in descending order Margin size, so adjustments always made to largest margins
  ## (use unlist() to get CurrRow row as a non-dataframe vector)
  ## subtract rank (where 1 = smallest) from (1+n_colGrps) so 1 = largest
  CurrRow[7, 1] <- "Sort"
  CurrRow[7, 2:(n_colGrps+1)] <- as.list(1 + n_colGrps - rank(unlist(CurrRow[6, 2:(n_colGrps+1)])))

  ## only adjust cells for which we have residual people (i.e., add where Margin largest)
  for(i in which(CurrRow[7, ] <= CurrRow$Diff[1])) {
    CurrRow[1, i] <- CurrRow[1, i] + 1
    CurrRow[2, i] <- CurrRow[2, i] + 1  ## also update Adjustments row to add to RowAdj for end
  }
  ## check:  sum(CurrRow[1, 2:(n_colGrps+1)]) == CurrRow$Ctrl_TOTAL[1]

  ## update row 1's columns: Sum & Diff, and RowAdj
  CurrRow[1, "Sum"] <- sum(CurrRow[1, 2:(n_colGrps+1)])
  CurrRow[1, "Diff"] <- CurrRow[1, "Sum"] - CurrRow[1, "Ctrl_TOTAL"]
  RowAdj <- RowAdj + unlist(CurrRow[2, 2:(n_colGrps+1)])

  ### ** reconcile column control totals (p.18-23)
  ## p.20-22: Negatives not allowed, values added to current row, subtract values from lower rows

  ## replace original data row with adjusted CurrRow[3,]
  data[CurrRow_value, ] <- CurrRow[1, ]

  ## clean up
  rm(upPower10, i, CurrRow)

  ## update CurrRow_value
  CurrRow_value <-  CurrRow_value + 1

  if(n_rowGrps == 2) {
    ## if only two Sexes, reconciling is literally subtracting RowAdj (which should = updated Diff) from other Sex

    ## drop unneeded columns at end: Ctrl_TOTAL, Diff
    data$Ctrl_TOTAL <- NULL
    data$Diff <- NULL

    ## update Sum and Diff rows
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- colSums(data[1:n_rowGrps, 2:(n_colGrps+1)])
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)]
                                                     - data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])   ## RowAdj

    ## subtract Diff from other VarRow (i.e., row above Sum row)
    data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)] <- (data[which(data$VarRow == "Sum")-1, 2:(n_colGrps+1)]
                                                             - data[data$VarRow == "Diff", 2:(n_colGrps+1)])

    ## update Sum and Diff again (all diffs should be 0 now)
    data[data$VarRow == "Sum", 2:(n_colGrps+1)] <- colSums(data[1:n_rowGrps, 2:(n_colGrps+1)])
    data[data$VarRow == "Diff", 2:(n_colGrps+1)] <- (data[data$VarRow == "Sum", 2:(n_colGrps+1)]
                                                     - data[data$VarRow == "Ctrl_TOTAL", 2:(n_colGrps+1)])
  }

  if(n_rowGrps > 2) {
    ### p.18-23: Reconcile column control totals (when more than 2 rows)
    ## p.20-22: Negatives not allowed, values added to current row, subtract values from lower rows
    ## i.e., sum(RowAdj) is positive

    ## 1. make temp with all non-adjusted data rows and subtract RowAdj (which is pos) from each row (p.21, bullet 1)
    temp <- data[CurrRow_value:n_rowGrps, 1:(n_colGrps+1)]

    adj_matrix <- rbind(RowAdj, RowAdj, deparse.level = 0)
    while (dim(adj_matrix)[1] < dim(temp)[1]) {
      adj_matrix <- rbind(adj_matrix, RowAdj, deparse.level = 0)
    }
    temp[, -1] <- temp[, -1] - adj_matrix
    rm(adj_matrix)

    ## 2. determine min value in each row (p.21, bullet 2)
    temp$Min <- apply(temp[, -1], 1, min, na.rm = TRUE)

    ## 3. get row sort order descending by Min column (using random fractional part to break ties)
    temp <- add.random.fraction.to.cols(df = temp, my_col = "Min")
    maxVarRow <- paste0(temp$VarRow[(which(temp$sort_rows == min(temp$sort_rows)))])  ## i.e., VarRow with largest Min value
    rm(temp)

    ## 4. change only the maxVarRow (i.e., subtract RowAdj from only maxVarRow); data[ ,1] is data$VarRow
    data[data$VarRow == maxVarRow, 2:(n_colGrps + 1)] <- (data[data$VarRow == maxVarRow, 2:(n_colGrps + 1)]
                                                          - RowAdj)

    ## 5. adjust that one row's Sum & Diff
    data$Sum[data$VarRow == maxVarRow] <- sum(data[data$VarRow == maxVarRow, 2:(n_colGrps + 1)])
    data$Diff[data$VarRow == maxVarRow] <- (data$Ctrl_TOTAL[data$VarRow == maxVarRow]
                                            - data$Sum[data$VarRow == maxVarRow])

  }

  #### ************************************ Check Point ************************************** ####
  ## At this point, columns (Regions) should sum to their control totals, AND
  ## rows (Sexes) should sum to their control totals
  # sum(data[data$Sex == "Sum", 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]
  # sum(data[1, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]
  # sum(data[2, 2:(n_Regions+1)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]
  # sum(data[data$Sex == "Diff", -1])  ## should be zero
  #### ************************************************************************************** ####

  data
}


#### dbRake ----
#' Rake population database
#'
#' @description
#' Reads a population database file (population, migration) and saves a population database file
#' with Region values raked for each of Age and Sex. Raking can be run with user-provided region
#' control totals, or without region control totals. Negative population values may be allowed or
#' not (default).
#'
#' This function assumes input files (e.g., InputData, CtrlPopTotals, etc.) are in an "inputs"
#' folder. The raked output will save to an "outputs" folder (which will be created if one does
#' not exist). If chosen, interim files are also saved to an "interim_files" folder within
#' "outputs" (this will be created if it does not exist and saveInterimFiles is TRUE). dbRake()
#' is a large function that takes a few minutes to run, and depends on multiple smaller functions.
#'
#' @details
#' dbRake is a large function with three main parts. \strong{Part 1} prorates and rakes Sex values
#' by Region. \strong{Part 2} prorates and rakes 5-year Age Group values by Region and Sex.
#' \strong{Part 3} prorates and rakes single-year Age values by Region and Sex. Throughout, checks
#' are performed and, if chosen, results are written to \strong{raking_log.csv} in the "outputs"
#' folder (regardless of whether raking succeeds or fails). As well, interim files may be saved to
#' an "interim_files" folder in "outputs" for future viewing. If raking succeeds, the final raked
#' data file is saved to "outputs".
#'
#' dbRake was originally an APL process (not in R). A PDF documenting that process, which holds
#' true for most of the underlying assumptions and procedures in dbRake, is available on BC Stats'
#' I drive (S152\\S52004) in \strong{Documentation > Raking > Methodology-Raking_Final.pdf}.
#'
#' @param InputData Name of .xlsx or .csv file that contains input data to be raked.
#' This file is assumed to have Region (e.g., LHA) by Sex (e.g., 1, 2, 3) as rows, and
#' Ages (e.g., 0, 1, 2, ..., TOTAL) as columns. Values are population counts.
#' @param CtrlPopTotals Name of .xlsx or .csv file that contains overall control totals
#' (e.g., "BC AS TOTALS.xlsx"). This file is assumed to have Sex (e.g., 1, 2, 3) as rows and
#' Ages (e.g., 0, 1, 2, ..., TOTAL) as columns. Values are population counts.
#' This file typically has dimensions of 3 (obs) by 103 variables.
#' @param CtrlRegionTotals Name of .xlsx or .csv file that contains overall control totals
#' (e.g., "LHA TOTALS.xlsx"). Default = NULL. This file is assumed to have Region (e.g., 89 LHAs)
#' as the first column and TOTAL (population counts) as the second column; this file is not broken out
#' by Sex or Age. This file typically has dimensions of n (obs) by 2 variables, where "n" is the
#' number of individual regions (e.g., 89 for LHA). If no name is provided (i.e., NULL), then region
#' control totals are not used. Instead, the InputData will be used to generate "control" totals.
#' @param CtrlAgeGrpsTotals Name of .xlsx or .csv file that contains initial 5 year age group totals.
#' Default = NULL. In virtually all cases, this variable will be NULL. In these cases, the InputData
#' will be used to generate "control" totals at 5-year groupings (e.g., 0-4, 5-9, 10-14, etc).
#' @param VarRegion Name of Region variable in all files (e.g., "LHA")
#' @param VarSex Name of Sex variable in all files (e.g., "Sex")
#' @param VarSexTotal Value that corresponds to Total (e.g., 3, when 1 and 2 are Male and Female)
#' @param AgeGrpMax Age of the older population that will be prorated and raked separately from
#' other 5 year age groups. AgeGrpMax will include all ages, including itself, through the remainder
#' of the population. Default = NULL. If AgeGrpMax is not set, the function will use 75 and up
#' (not necessarily the oldest age; that is, the oldest age is usually 100, meaning 100 and up).
#' The BC Stats Demographics team determined that 75 was the best age for AgeGrpMax to ensure that
#' distortion in older populations is minimized.
#' @param allowNegatives Logical value for whether or not negative population values are allowed.
#' Default = FALSE. Only migration data should be allowed to have negative values.
#' @param saveInterimFiles Logical value for whether or not interim files (.csvs) should be saved
#' throughout the process. Default = FALSE. If saved, they will be saved in "interim_files" within
#' "outputs" folder. This folder will be created if it does not exist and is needed.
#' @param writeRakingLog Logical value for whether or not a log file (raking_log.csv) should be
#' written. Default = FALSE. If written, it will be saved in "outputs" folder.
#' @param readFiles Logical value for whether or not input files (InputData, CtrlPopTotals,
#' CtrlRegionTotals, CtrlAgeGrpsTotals) need to be read in. Default = FALSE. If FALSE, files are
#' already in environment, likely by being called or created through another function (e.g.,
#' \code{\link{dbConvert}}).
#' @return RakedData.csv will be saved to "outputs" folder (which will be created if one does not
#' already exist). If set to TRUE, various interim files will be saved in an "interim_files" folder
#' within "outputs". If set to TRUE, a log file ("raking_log.csv") will also be saved to the
#' "outputs" folder.
#' @examples
#' \dontrun{  dbRake(InputData = "POPHAE19.xlsx", CtrlPopTotals = "BC AS TOTALS.xlsx",
#'                   CtrlRegionTotals = "LHA TOTALS.xlsx", CtrlAgeGrpsTotals = NULL,
#'                   VarRegion = "LHA", VarSex = "Sex", VarSexTotal = 3, AgeGrpMax = NULL,
#'                   allowNegatives = FALSE, saveInterimFiles = FALSE, writeRakingLog = FALSE,
#'                   readFiles = TRUE)  }
#' \dontrun{  ## if dbRake() is called in \code{\link{dbConvert}}(), which brings in inputs
#'            dbRake(InputData = ToDB, CtrlPopTotals = control_totals,
#'                   CtrlRegionTotals = region_totals, CtrlAgeGrpsTotals = NULL,
#'                   VarRegion = "LHA", VarSex = "Sex", VarSexTotal = 3, AgeGrpMax = NULL,
#'                   allowNegatives = FALSE, saveInterimFiles = FALSE, writeRakingLog = TRUE,
#'                   readFiles = FALSE)  }
#' @seealso Raking helpers include: \code{\link{rounded}}(), \code{\link{read.inputs}}(),
#' \code{\link{real.to.int}}(), \code{\link{calc.cols}}(), \code{\link{prorate.row}}(),
#' \code{\link{prep.prorate.col}}(), \code{\link{prorate.col}}(), and raking algorithm functions A, B, C:
#' \code{\link{allowNegsnoMargin}}(), \code{\link{noNegsnoMargin}}(), \code{\link{noNegsneedMargin}}()
#' @author Julie Hawkins, BC Stats
#' @export
dbRake <- function(InputData, CtrlPopTotals, CtrlRegionTotals = NULL, CtrlAgeGrpsTotals = NULL,
                   VarRegion, VarSex, VarSexTotal, AgeGrpMax = NULL, allowNegatives = FALSE,
                   saveInterimFiles = FALSE, writeRakingLog = FALSE, readFiles = FALSE) {

  #### 0. Prep ----

  ## A. check for required folder(s); create if needed and doesn't exist
  if(!file.exists(here::here("outputs"))) {
    dir.create(here::here("outputs"))
    message(paste0("An 'outputs' folder has been created at '", here::here(), "/'."))
  }

  if(saveInterimFiles == TRUE) {
    if(!file.exists(here::here("outputs", "interim_files"))) {
      dir.create(here::here("outputs", "interim_files"))
      message(paste0("An 'interim_files' subfolder has been created at '", here::here("outputs"), "/'."))
    } else {
      message("Interim files were saved to '", here::here("outputs", "interim_files"), "/'.")
    }
  }

  ## B. create log file, if required
  if(writeRakingLog == TRUE) {
    raking_log <- data.frame(message = as.character(), stringsAsFactors = FALSE)
  }

  ## C. read data, if needed
  if(readFiles == TRUE) {

    if(!file.exists(here::here("inputs"))) {
      stop(paste0("The 'inputs' folder cannot be found. It should be at: '", here::here(), "/inputs'."))
    }

    ## C1. read in population control totals
    CtrlPopTotals <- read.inputs(inputFile = CtrlPopTotals)

    ## C2. read in region control totals, if they exist; set UseControlRegionTotals to TRUE or FALSE accordingly
    if(!is.null(CtrlRegionTotals)) {
      CtrlRegionTotals <- read.inputs(inputFile = CtrlRegionTotals)
      UseControlRegionTotals <- TRUE
    } else {
      UseControlRegionTotals <- FALSE
    }

    ## C3. read in age control totals, if they exist; set have5yrAgeGrps to TRUE or FALSE accordingly
    if(!is.null(CtrlAgeGrpsTotals)) {
      CtrlAgeGrpsTotals <- read.inputs(inputFile = CtrlAgeGrpsTotals)
      have5yrAgeGrps <- TRUE
    } else {
      have5yrAgeGrps <- FALSE
    }

    ## C4. read in input data; this is what needs to be raked
    InputData <- read.inputs(inputFile = InputData)

  }

  if(readFiles == FALSE) {

    ## C2. set UseControlRegionTotals to TRUE or FALSE accordingly
    if(!is.null(CtrlRegionTotals)) {
      UseControlRegionTotals <- TRUE
    } else {
      UseControlRegionTotals <- FALSE
    }

    ## C3. set have5yrAgeGrps to TRUE or FALSE accordingly
    if(!is.null(CtrlAgeGrpsTotals)) {
      have5yrAgeGrps <- TRUE
    } else {
      have5yrAgeGrps <- FALSE
    }

  }

  ## C5. update raking_log, if required
  if(writeRakingLog == TRUE) {
    if(UseControlRegionTotals == TRUE) {
      ## add message to raking_log
      raking_log[nrow(raking_log)+1, 1] <- "Prep: 'UseControlRegionTotals' is set to TRUE because you named a Control Region Totals file."
    } else {
      raking_log[nrow(raking_log)+1, 1] <- "Prep: 'UseControlRegionTotals' is set to FALSE because you did not name a Control Region Totals file."
    }
  }

  if(writeRakingLog == TRUE) {
    if(have5yrAgeGrps == TRUE) {
      ## add message to raking_log
      raking_log[nrow(raking_log)+1, 1] <- "Prep: 'have5yrAgeGrps' is set to TRUE because you named a Control Age Totals file."
    } else {
      raking_log[nrow(raking_log)+1, 1] <- "Prep: 'have5yrAgeGrps' is set to FALSE because you did not name a Control Age Totals file."
    }
  }

  ## D. check for negative values if allowNegatives is FALSE
  if(allowNegatives == FALSE) {
    if(any(InputData < 0)) {
      stop("You set 'allowNegatives' as FALSE, but there is at least one negative value in the data. Fix this.")
    } else {
      if(writeRakingLog == TRUE) {
        ## add message to raking_log
        raking_log[nrow(raking_log)+1, 1] <- "Prep: 'allowNegatives' is set to FALSE."
      }
    }
  }

  ## E. create OutputData from InputData, that will be updated with changes
  OutputData <- InputData

  if(writeRakingLog == TRUE) {
    ## E. add message to raking_log re set values
    raking_log[nrow(raking_log)+1, 1] <- paste0("Prep: you set '", VarRegion, "' as the region, and '",
                                                VarSexTotal, "' as the value for Sex Total; also, '",
                                                AgeGrpMax, "' is the maximum age, meaning this age and ",
                                                "older will be prorated, but not raked, to minimize ",
                                                "distortion in older populations.")
  }

  #### Part 1 ----
  ## Part 1. Updating initial estimates of male/female regional total values

  ## Part 1.0: prep ----
  ## 1A. redistribute input data as wide data, without Sex TOTAL info
  ## assumes InputData has a Region column, a Sex column, many age columns, and a TOTAL column
  ## want data with Region column; pivot "long" Sex as "wide" columns with just the TOTAL data, no age data

  data <- InputData %>%
    dplyr::rename(Sex = {{VarSex}}, Region = {{VarRegion}}) %>%
    dplyr::filter(Sex != VarSexTotal) %>%
    dplyr::select(VarRow = Region, Sex, TOTAL) %>%
    tidyr::pivot_wider(names_from = "Sex", values_from = "TOTAL")

  ## 1B. calc number of Regions and number of (non-Total) Sexes; to determine # of groups to adjust over
  n_Regions <- dim(data)[1]
  n_Sex <- dim(CtrlPopTotals)[1] - 1

  ## Part 1.1: prorate rows ----
  if(UseControlRegionTotals == TRUE) {
    ## !!!! If there are CtrlRegionTotals !!!!

    ## 1C. rename VarRegion as VarRow so left_join in calc.cols() works
    temp <- dplyr::rename(CtrlRegionTotals, VarRow = {{VarRegion}})

    ## 1D. calc necessary columns
    ## Step 1: calc actual sum, add in VarRow (i.e., Region) control totals, calc difference
    ## Step 2: calc adjustment value (difference divided by number of groups)
    n_colGrps <- n_Sex
    n_rowGrps <- n_Regions
    data <- calc.cols(data = data, temp, VarRow = VarRegion, n_colGrps)

    ## 1E. reconcile row by row (i.e., for 1:n_Sex, prorate so region totals sum to region control totals)
    for (i in 1:n_rowGrps) {

      ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
      ## Step 4: repeat Steps 1 through 3 while difference is not zero
      CurrRow <- data[i, ]

      ## WHILE difference is NOT zero, adjust actual data
      while(abs(CurrRow$Diff) > 0.0000000001) {
        CurrRow <- prorate.row(CurrRow, n_colGrps, allowNegatives)
      }

      ## ensure all numbers are integers (i.e., no fractional people allowed)
      CurrRow[, 2:(n_colGrps + 1)] <- real.to.int(realNums = CurrRow[, 2:(n_colGrps + 1)])

      ## replace actual data with adjusted data in CurrRow
      data[i, ] <- CurrRow

    }

    ## 1F. remove no-longer-needed objects
    rm(i, CurrRow, temp)

    ### ************************************ Check Point ************************************** ###
    ## At this point, Region rows sum to their control total, BUT
    ## Sex columns do NOT (necessarily) sum to their control totals
    # sum(data$Sum) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]  ## should be TRUE
    # sum(data$`1`) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]   ## likely not zero
    # sum(data$`2`) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]   ## likely not zero
    ### *************************************************************************************** ###
    if(sum(data$Sum) != CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]) {
      stop("Error 1.1 (prorate rows): Region rows should sum to their control total, but they do not.
           Specifically, sum(data$Sum) differs from the overall CtrlPopTotals' TOTAL.")
    } else {
      if(writeRakingLog == TRUE) {
        ## add message to raking_log
        if(UseControlRegionTotals == TRUE) {
          raking_log[nrow(raking_log)+1, 1] <- "Part 1.1 (prorate rows): Region rows now sum to their control total."
        } else {
          raking_log[nrow(raking_log)+1, 1] <- "Part 1.1 (prorate rows): Region Control Totals were not provided, so will be created next."
        }
      }
    }

  }

  ## Part 1.2: prorate columns ----
  ### ** Rake down columns to match column totals, aka, prorate **
  ## 1G. prep for prorating columns
  ## get sex Ctrl_TOTALs
  temp <- CtrlPopTotals %>%
    dplyr::select(Sex, TOTAL) %>%
    dplyr::filter(Sex != VarSexTotal) %>%
    tidyr::pivot_wider(names_from = "Sex", values_from = "TOTAL") %>%
    dplyr::mutate(VarRow = "Ctrl_TOTAL") %>%
    dplyr::select(VarRow, tidyselect::everything())

  n_colGrps <- n_Sex
  n_rowGrps <- n_Regions
  data <- data %>% dplyr::mutate(VarRow = as.character(VarRow))

  ## 1H. calculate necessary rows: Sum (colSums), Ctrl_TOTAL (temp), Diff (subtraction), adj_value (division)
  ## Step 1: calc actual sum, add in VarRow control totals, calc difference
  ## Step 2: calc adjustment value (difference divided by number of groups)
  dataCols <- prep.prorate.col(data, n_rowGrps, colGrps = 1:(n_colGrps+1),
                               ctrl_total_row = temp, AgeGrpMax = NULL, ageLast = NULL)

  ## 1I. prorate column by column (i.e., over n_Regions)
  for (i in 2:(n_colGrps+1)) {

    ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
    ## Step 4: repeat Steps 1 through 3 while difference is not zero
    CurrCol <- as.data.frame(dataCols[ ,c(1, i)])

    ## WHILE difference is NOT zero, adjust actual data
    while(abs(CurrCol[which(CurrCol[, 1] == "Diff"), -1]) > 0.0000000001) {
      CurrCol <- prorate.col(CurrCol, n_rowGrps, allowNegatives)
    }

    ## ensure all numbers are integers (i.e., no fractional people allowed)
    CurrCol[1:n_rowGrps, -1] <- real.to.int(realNums = CurrCol[1:n_rowGrps, -1])

    ## replace actual data with adjusted data in CurrCol
    dataCols[ ,i] <- CurrCol[,-1]

  }

  ### ************************************ Check Point ************************************** ###
  ## At this point, Region rows sum to their control total, AND
  ## Sex columns should also sum to their control totals
  # sum(dataCols[1:89, -1]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]  ## should be TRUE
  # sum(dataCols$`1`[1:89]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]   ## should be zero
  # sum(dataCols$`2`[1:89]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]   ## should be zero
  ## when region is CHSA
  # sum(dataCols[1:218, -1]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]  ## should be TRUE
  # sum(dataCols$`1`[1:218]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]   ## should be zero
  # sum(dataCols$`2`[1:218]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]   ## should be zero
  ## when fewer ages
  # sum(dataCols[1:29, -1]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]  ## should be TRUE
  # sum(dataCols$`1`[1:29]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]   ## should be zero
  # sum(dataCols$`2`[1:29]) - CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]   ## should be zero
  ### *************************************************************************************** ###
  check1.13 <- sum(dataCols[1:n_Regions, -1]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]
  check1.11 <- sum(dataCols[1:n_Regions, 2]) - CtrlPopTotals$TOTAL[1]
  check1.12 <- sum(dataCols[1:n_Regions, 3]) - CtrlPopTotals$TOTAL[2]
  check1.1 <- check1.13 == TRUE & identical(check1.11, check1.12, 0)
  if(check1.1 != TRUE) {
    stop("Error 1.2 (prorate cols): Sex columns should sum to their control total, but they do not.
           Specifically, sum(dataCols[1:n_Regions, -1]) differs from the overall CtrlPopTotals' TOTAL,
           OR, one or more of the individual sexes do not sum to their Ctrl_Pop_Total.")
  } else {
    if(writeRakingLog == TRUE) {
      ## add message to raking_log
      raking_log[nrow(raking_log)+1, 1] <- "Part 1.2 (prorate columns): Sex columns now sum to their control total."
    }
  }

  ## remove no-longer-needed objects
  rm(i, CurrCol, temp, check1.13, check1.11, check1.12, check1.1)

  ## Part 1.3: rake prorated data ----

  if(UseControlRegionTotals == FALSE) {
    ## !!!! If there are NO CtrlRegionTotals !!!!

    ## need to create "Sum" column that would have been made if we ran steps 1J through 1Q
    dataCols <- dataCols %>% dplyr::mutate(Sum = rowSums(dataCols[ , -1])) #as.numeric(`1`) + as.numeric(`2`))

    ## need to create "Ctrl_Regions_Total" file (to be used later in checks) now that we have created them
    CtrlRegionTotals <- dataCols[1:n_Regions, ] %>% dplyr::select({{VarRegion}} := VarRow, TOTAL = Sum)

  }

  if(UseControlRegionTotals == TRUE) {
    ## !!!! If there are CtrlRegionTotals !!!!

    ## 1J. calc necessary columns: Sum, Ctrl_TOTAL, Diff, adj_value
    ## Step 1: calc actual sum, add in VarRow control totals, calc difference
    ## Step 2: calc adjustment value (difference divided by number of groups)

    ## Region Ctrl_TOTALs
    temp <- data %>% dplyr::select(VarRow, TOTAL = Ctrl_TOTAL) %>% dplyr::mutate(VarRow = as.character(VarRow))
    dataCols <- calc.cols(data = dataCols, temp, VarRow, n_colGrps)
    dataCols$adj_value <- NULL
    rm(temp)

    ### ** Reorder rows, if necessary **
    ## 1K. sort (data) rows in ascending order (min -> max) of row totals (i.e., Sum column)
    dataCols[1:n_rowGrps, ] <- dplyr::arrange(dataCols[1:n_rowGrps, ], Sum)

    ### ** Rake the data: **PrivateRakePreprocessedData, aka, rake **
    ### rake over all rows except for last row (it should be done by default)
    ## At this point, n_colGrps = n_Regions and n_rowGrps = n_Sex

    ## 1L. set CurrRow_value, and pull CurrRow from data to work on, iterate over rows
    CurrRow_value <- 1  ## first row in data first time through; CurrRow[1,]

    while(CurrRow_value < n_rowGrps) {

      CurrRow <- dataCols[CurrRow_value, ]

      ## set up new row for Adjustments; initially, all adjustments = 0
      CurrRow[2, 1] <- "Adjustments"
      CurrRow[2, 2:(n_colGrps+1)] <- 0

      ## set up new row for ADJUSTED current row; = CurrRow + Adjustments
      CurrRow[3, 1] <- "AdjCurrRow"
      CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[1, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

      ## create RowAdj to save all adjustments made that need to be made at end to the last row/reflected in data
      RowAdj <- rep(0, times = n_colGrps)

      ## determine whether need Margin or not
      if(allowNegatives == FALSE) {
        ## if negatives NOT allowed, determine whether we need Margin or not
        if(CurrRow$Sum[1] < CurrRow$Ctrl_TOTAL[1]) {
          needMargin <- TRUE                      ## algorithm C: if values need to be added & negatives not allowed
        } else {
          needMargin <- FALSE                     ## algorithm B: if values need to be subtracted & negatives not allowed
        }
      } else {
        ## else if negatives ARE allowed, Margin is not needed
        needMargin <- FALSE                       ## algorithm A: if negatives ARE allowed
      }

      ## 1M. choose and run appropriate algorithm:
      ## 1N. calc "whole people" adjustments to be made to all cells in the row
      ## 1O. calc "residual people" adjustments to be made to selected cells in row
      ## 1P. reconcile column control totals

      if(allowNegatives == TRUE) {

        ## p.12: A. Adjust cells in the current row if negative values are allowed
        dataCols <- allowNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

      } else {

        if(needMargin == FALSE) {
          ## p.13: B. Adjust cells in the current row if values need to be taken away, negatives not allowed
          ## "If the sum of ages is greater than the control total, then values need to be taken away from
          ## age groups for the current row and added to age groups for rows below the current row."
          dataCols <- noNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

        } else {
          ## i.e., needMargin == TRUE, allowNegatives == TRUE
          ## p.14: C. Adjust cells in the current row if values need to be added, negatives not allowed
          ## "If the sum of ages is less than the control total, then values needed to be added to age
          ## groups for the current row, and taken away from age groups for rows below the current row."
          dataCols <- noNegsneedMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj, needMargin)

        }
      }

      ## update CurrRow_value
      CurrRow_value <- CurrRow_value + 1

    }


    ## 1Q. adjust last row and update Sum & Diff rows
    dataCols[CurrRow_value, 2:(n_colGrps+1)] <- (dataCols[CurrRow_value, 2:(n_colGrps+1)]
                                                 - dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+1)])
    dataCols$Sum[CurrRow_value] <- rowSums(dataCols[CurrRow_value, 2:(n_colGrps+1)])
    dataCols$Diff[CurrRow_value] <- dataCols$Ctrl_TOTAL[CurrRow_value] - dataCols$Sum[CurrRow_value]
    dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)] <- as.list(colSums(dataCols[1:n_rowGrps, 2:(n_colGrps+1)]))
    dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+1)] <- (dataCols[which(dataCols[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)]
                                                                  - dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)])

    rm(CurrRow, CurrRow_value, needMargin, RowAdj)
  }

  ## 1R. update OutputData with changes and save as interim file

  ## get just data (and Sum) rows
  temp <- dataCols[1:(n_rowGrps), 1:(n_colGrps+2)]

  ## flip data back to long format
  temp <- temp %>%
    tidyr::pivot_longer(c(-VarRow), names_to = "VarSex", values_to = "TOTAL") %>%
    dplyr::rename(VarRegion = VarRow) %>%
    dplyr::mutate(VarSex = dplyr::case_when(VarSex == "Sum" ~ "3", TRUE ~ as.character(VarSex))) %>%
    dplyr::mutate(VarRegion = as.numeric(VarRegion), VarSex = as.numeric(VarSex))

  ### ** Put the data back in order, if necessary **
  ## re-sort rows by InputData order
  rows_order <- InputData %>%
    dplyr::select(VarRegion = (which(names(InputData) == VarRegion)),
           VarSex = (which(names(InputData) == VarSex))) %>%
    dplyr::mutate(row_order = dplyr::row_number())
  temp <- dplyr::left_join(temp, rows_order, by = c("VarRegion", "VarSex"))
  temp <- dplyr::arrange(temp, row_order)
  temp$row_order <- NULL
  temp <- dplyr::rename(temp, {{VarRegion}} := VarRegion, {{VarSex}} := VarSex)


  ## drop original TOTAL from OutputData (it was a copy from InputData)
  OutputData$TOTAL <- NULL

  ## join to OutputData the now raked estimates of male/female regional total values
  OutputData <- OutputData %>%
    dplyr::left_join(temp, by = c({{VarRegion}}, {{VarSex}}))

  if(saveInterimFiles == TRUE) {
    ## save as interim file
    readr::write_csv(OutputData, here::here("outputs", "interim_files",
                                     "OutputData_1_updated_initial_estimates_Sex_Region_Totals.csv"))
  }

  ## clean up
  rm(temp, data, dataCols, n_colGrps, n_rowGrps, rows_order)

  if(writeRakingLog == TRUE) {
    ## add message to raking_log
    raking_log[nrow(raking_log)+1, 1] <- "Part 1: Total Sex values have been updated for each Region. See 'outputs\\interim_files' if interested."
  }

  #### Part 2 ----
  ## Part 2. Updating initial 5 year age group and maximum age group estimates, by Sex

  ## Part 2.0: prep ----

  ## if they don't exist, create 5-year age groups (i.e., "CtrlAgeGrpsTotals" and "OutputData5")
  if(have5yrAgeGrps == FALSE) {

    ## find all ages ending in 0 and 5, merge in single vector, in numeric order; get name of last age
    age0s <- names(CtrlPopTotals[tidyselect::ends_with(match = "0", vars = names(CtrlPopTotals))])
    age5s <- names(CtrlPopTotals[tidyselect::ends_with(match = "5", vars = names(CtrlPopTotals))])
    ageStarts <- c(age0s, age5s)
    ageStarts <- sort(as.numeric(ageStarts))
    ageLast <- as.character(max(as.numeric(age0s)))
    rm(age0s, age5s)

    ## AgeGrps5Yr names (ignore last age0s which should be last age group in data (e.g., 100, 90))
    AgeGrps5Yr <- rep(NA, length(ageStarts)-1)
    for (i in 1:length(AgeGrps5Yr)) {
      AgeGrps5Yr[i] <- paste0(ageStarts[i], "-", (ageStarts[i]+4))
    }; rm(i)
    # AgeGrps5Yr: "0-4" "5-9" "10-14" ... "85-89" "90-94" "95-99"

    ## create 5 year & maximum age groups **control totals**
    CtrlAgeGrpsTotals <- CtrlPopTotals
    for (i in 1:length(AgeGrps5Yr)) {
      CtrlAgeGrpsTotals <- CtrlAgeGrpsTotals %>%
        dplyr::mutate(temp = rowSums(CtrlAgeGrpsTotals[
          which(names(CtrlAgeGrpsTotals) == ageStarts[i]):(which(names(CtrlAgeGrpsTotals) == ageStarts[i])+4)
          ])) %>%
        dplyr::rename(!!AgeGrps5Yr[i] := temp)
    }; rm(i)
    CtrlAgeGrpsTotals <- CtrlAgeGrpsTotals %>%
      dplyr::select(Sex, tidyselect::all_of(AgeGrps5Yr), tidyselect::all_of(ageLast), TOTAL)
    attributes(CtrlAgeGrpsTotals$`0-4`) <- NULL  ## for some reason "0-4" may be named

    ## create 5 year & maximum age groups **sample data**
    OutputData5 <- OutputData %>% dplyr::rename(Region = {{VarRegion}}, Sex = {{VarSex}})
    for (i in 1:length(AgeGrps5Yr)) {
      OutputData5 <- OutputData5 %>%
        dplyr::mutate(temp = rowSums(OutputData5[
          which(names(OutputData5) == ageStarts[i]):(which(names(OutputData5) == ageStarts[i])+4)
          ])) %>%
        dplyr::rename(!!AgeGrps5Yr[i] := temp)
    }; rm(i)
    OutputData5 <- OutputData5 %>% dplyr::select(Region, Sex, tidyselect::all_of(AgeGrps5Yr), tidyselect::all_of(ageLast), TOTAL)

  } else {
    ageLast <- names(CtrlAgeGrpsTotals)[ncol(CtrlAgeGrpsTotals)-1]
  }

  ## 2A. identify Sexes (e.g., 1, 2, 3), set CurrSex as first element,
  ##     id cols of 5 yr age groups and max age group (ageLast)
  Sexes <- InputData %>% dplyr::pull({{VarSex}}) %>% unique()
  CurrSex <- Sexes[1]
  AgeGrps5Yr <- CtrlAgeGrpsTotals %>%
    dplyr::select(which(stringr::str_detect(string = names(CtrlAgeGrpsTotals), pattern = "-"))) %>% names()

  ## either create AgeGrpMax or check that it is the beginning of a 5YrGrp
  if(is.null(AgeGrpMax)) {
    ## if AgeGrpMax does not yet exist, set as 75+
    AgeGrpMax <- 75
    ## if age 75 does not exist (e.g., birth data), get max age group (second-to-last column name)
    if(!(75 %in% names(InputData))) {                   # if(!(75 %in% names(CtrlAgeGrpsTotals))) {
      ## otherwise use what was set in raking arguments
      AgeGrpMax <- names(InputData)[ncol(InputData)-1]  # AgeGrpMax <- names(CtrlAgeGrpsTotals)[ncol(CtrlAgeGrpsTotals)-1]
    }
  } else {
    ## check that AgeGrpMax is beginning of a 5YrGrp; if not, have user re-set the value
    AgeGrpsStarts <- unique(stringr::str_sub(AgeGrps5Yr, start = 1, end = (stringr::str_locate(AgeGrps5Yr, pattern = "-")-1)))
    if(!(AgeGrpMax %in% AgeGrpsStarts) & AgeGrpMax != ageLast){
      stop("Error 2.0 (prep 5yr age groups): The value you set for AgeGrpMax is not the beginning of a 5-year age group.
            Please choose another value and restart the code.")
    }
    rm(AgeGrpsStarts)
  }

  ## set/sum oldest age group(s)
  if(AgeGrpMax != ageLast) {
    ## if AgeGrpMax is NOT the last column, create AgeGrpsOldest as sum from AgeGrpMax thru ageLast
    temp <- which(unique(stringr::str_sub(AgeGrps5Yr, start = 1, end = (stringr::str_locate(AgeGrps5Yr, pattern = "-")-1))) == AgeGrpMax)
    AgeGrpsOldest <- c(AgeGrps5Yr[temp:length(AgeGrps5Yr)], ageLast)
    # AgeGrpsOldest: "75-59" "80-84" "85-89" "90-94" "95-99" "100"
    rm(temp)
  } else {
    ## if AgeGrpMax is the last column, set AgeGrpsOldest the same as ageLast
    AgeGrpsOldest <- ageLast
  }
  n_AgeGrps <- dim(CtrlAgeGrpsTotals)[2] - 2  ## -2 to not count Sex and TOTAL columns


  ## Part 2.1-2.4: raking done in while() loop for non-total sexes ----
  ## 2B. repeat for all Sexes, except Total
  while(CurrSex < VarSexTotal) {

    ## Part 2.1: prorate AgeGrpsOldest columns ----
    if(have5yrAgeGrps == FALSE) {

      ### when OutputData does not originally have 5 year age groups already (i.e., have5yrAgeGrps == FALSE)

      ## 2C. filter data by CurrSex
      data <- OutputData5 %>%
        dplyr::filter(Sex == CurrSex)

      ## 2D. add total level data as last row
      temp <- CtrlAgeGrpsTotals %>%
        dplyr::rename(Sex = {{VarSex}}) %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::mutate(Region = "Ctrl_TOTAL") %>%
        dplyr::select(Region, Sex, tidyselect::everything())

      data <- rbind(data, temp); rm(temp)

    } else {

      ### when OutputData has 5 year age groups already (i.e., have5yrAgeGrps == TRUE)

      ## 2C. filter data by CurrSex
      data <- OutputData %>%
        dplyr::rename(Sex = {{VarSex}}, Region = {{VarRegion}}) %>%
        dplyr::filter(Sex == CurrSex)

      ## 2D. add total level data as last row
      temp <- CtrlPopTotals %>%
        dplyr::rename(Sex = {{VarSex}}) %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::mutate(Region = "Ctrl_TOTAL") %>%
        dplyr::select(Region, Sex, tidyselect::everything())

      data <- rbind(data, temp); rm(temp)

    }

    ## rename Region as VarRow
    data <- dplyr::rename(data, VarRow = Region)

    ## set n_rowGrps
    n_rowGrps <- n_Regions

    ## 2E. prorate AgeGrpsOldest (i.e., prorate maximum age group(s))

    ## prep for prorating AgeGrpsOldest columns
    temp <- data[data$VarRow == "Ctrl_TOTAL", c("VarRow", AgeGrpsOldest)]
    # temp: 1 obs, 7 vars, with Ctrl_TOTALs for age groups: "VarRow" 75-79" "80-84"  "85-89"  "90-94"  "95-99"  "100"
    MaxAge <- prep.prorate.col(data, n_rowGrps, colGrps = c("VarRow", AgeGrpsOldest),
                               ctrl_total_row = temp, AgeGrpMax, ageLast)
    # MaxAge: 93 obs (89 LHAs, Sum row, Ctrl_TOTAL row, Diff row, adj_value row) of 7 vars

    ## call prorate function; ensure all numbers are integers (i.e., no fractional people allowed)
    for (i in 2:ncol(MaxAge)) {

      ## add/subtract adjustment value to/from actual data to get first interim value
      ## repeat while difference is not zero
      CurrCol <- as.data.frame(MaxAge[ ,c(1, i)])

      ## WHILE difference is NOT zero, adjust actual data
      while(abs(CurrCol[which(CurrCol[, 1] == "Diff"), -1]) > 0.0000000001) {
        CurrCol <- prorate.col(CurrCol, n_rowGrps, allowNegatives)
      }

      ## ensure all numbers are integers (i.e., no fractional people allowed)
      CurrCol[1:n_rowGrps, -1] <- real.to.int(realNums = CurrCol[1:n_rowGrps, -1])

      ## replace actual data with adjusted data in CurrCol
      MaxAge[ ,i] <- CurrCol[,-1]

    }; rm(i, CurrCol, temp)

    check2.1 <- sum(MaxAge[1:n_Regions, -1]) == sum(CtrlAgeGrpsTotals[CtrlAgeGrpsTotals$Sex == CurrSex, AgeGrpsOldest])
    if(check2.1 != TRUE) {
      stop("Error 2.1 (prorate AgeGrpsOldest columns): MaxAge columns for AgeGrpsOldest should sum to the current sex's control total, but they do not.")
    } else {
      if(writeRakingLog == TRUE) {
        ## add message to raking_log
        raking_log[nrow(raking_log)+1, 1] <- paste0("Part 2.1, Sex ", CurrSex,
                                                    " (prorate columns): MaxAge columns (",
                                                    paste(AgeGrpsOldest, collapse = ", "),
                                                    ") now sum to their control total for this Sex.")
      }
      rm(check2.1)
    }

    ## sum AgeGrpsOldest
    if(AgeGrpMax != ageLast) {
      ## use rowSums() if AgeGrpsMax is more than one group (to get each row's sum)
      MaxAge$TotalOldest <- rowSums(MaxAge[, -1])
    } else {
      MaxAge$TotalOldest <- MaxAge[,which(names(MaxAge) == AgeGrpMax)]
    }

    ## Part 2.2: prorate non-AgeGrpsOldest rows ----
    ### ** Now, work on non-AgeGrpsOldest **
    ## 2F. subtract the prorated oldest age group(s) from Region totals to rake majority of 5 yr age groups
    ## set up: rows = regions w/ last row = Ctrl_TOTAL; cols = VarRow, Sex, 5 yr age groups, TOTAL
    ## Do NOT include oldest age group(s) in prorating rows of 5 year age groups! i.e., drop AgeGrpsOldest
    data <- data %>%
      dplyr::select(-tidyselect::all_of(AgeGrpsOldest)) %>%
      dplyr::left_join(MaxAge, by = "VarRow") %>%
      dplyr::mutate(TOTAL = TOTAL - TotalOldest) %>%
      dplyr::select(-tidyselect::all_of(AgeGrpsOldest), -TotalOldest)


    ### ** Rake across rows to match row totals: PrivateReconcileTotalsByAdding, aka, prorate; on non-AgeGrpsOldest **
    ## 2G. calc necessary columns (raking by addition prep)
    ## Step 1: calc actual sum, add in VarRow (i.e., Region) control totals, calc difference
    ## Step 2: calc adjustment value (difference divided by number of groups)

    ## set n_colGrps; n_rowGrps is still = n_Regions
    n_colGrps <- n_AgeGrps - length(AgeGrpsOldest)  ## exclude AgeGrpsOldest

    ## get Region control totals for CurrSex and number of 5 year age groups
    temp <- data %>% dplyr::select(VarRow, TOTAL)

    ## drop Sex & TOTAL from data
    data <- data %>% dplyr::select(-Sex, -TOTAL)
    data <- calc.cols(data, temp, VarRow, n_colGrps)


    ## 2H. prorate row by row (i.e., for 1:n_Regions, prorate so region age group totals sum to region control totals)
    for (i in 1:n_rowGrps) {

      ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
      ## Step 4: repeat Steps 1 through 3 while difference is not zero
      CurrRow <- data[i, ]

      ## WHILE difference is NOT zero, adjust actual data
      while(abs(CurrRow$Diff) > 0.0000000001) {
        CurrRow <- prorate.row(CurrRow, n_colGrps, allowNegatives)
      }

      ## ensure all numbers are integers (i.e., no fractional people allowed)
      CurrRow[, 2:(n_colGrps + 1)] <- real.to.int(realNums = CurrRow[, 2:(n_colGrps + 1)])

      ## replace actual data with adjusted data in CurrRow
      data[i, ] <- CurrRow

    }

    ## remove no-longer-needed objects
    rm(i, CurrRow, temp)

    ### ************************************ Check Point ************************************** ###
    ## At this point, Region rows (across 5 year age groups) sum to their control total, BUT
    ## 5 year age group columns do NOT (necessarily) sum to their control totals
    ## example: sum(data$`0-4`[-nrow(data)]) - CtrlAgeGrpsTotals$`0-4`[CtrlAgeGrpsTotals$Sex == CurrSex]  ## likely not zero
    ### *************************************************************************************** ###


    ## Part 2.3: prorate non-AgeGrpsOldest columns ----
    ### ** Rake down columns to match column totals: PrivateReconcileTotalsByAdding, aka, prorate; on non-AgeGrpsOldest **
    ## get age Ctrl_TOTALs for CurrSex
    temp <- data[nrow(data), 1:(n_colGrps+1)]

    ## 2I. calculate necessary rows: Sum (colSums), Ctrl_TOTAL (temp), Diff (subtraction), adj_value (division)
    ## Step 1: calc actual sum, add in VarRow control totals, calc difference
    ## Step 2: calc adjustment value (difference divided by number of groups)
    dataCols <- prep.prorate.col(data, n_rowGrps, colGrps = 1:(n_colGrps+1),
                                 ctrl_total_row = temp, AgeGrpMax = NULL, ageLast = NULL)

    ## 2J. prorate column by column
    for (i in 2:(n_colGrps+1)) {

      ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
      ## Step 4: repeat Steps 1 through 3 while difference is not zero
      CurrCol <- dataCols[ ,c(1, i)]

      ## WHILE difference is NOT zero, adjust actual data
      while(abs(CurrCol[which(CurrCol[, 1] == "Diff"), -1]) > 0.0000000001) {
        CurrCol <- prorate.col(CurrCol, n_rowGrps, allowNegatives)
      }

      ## ensure all numbers are integers (i.e., no fractional people allowed)
      CurrCol[1:n_rowGrps, -1] <- real.to.int(realNums = CurrCol[1:n_rowGrps, -1])

      ## replace actual data with adjusted data in CurrCol
      dataCols[ ,i] <- CurrCol[,-1]

    }

    ### ************************************ Check Point ************************************** ###
    ## At this point, Region rows (across 5 year age groups) sum to their control total, and
    ## 5 year age group columns (EXCEPT for AgeGrpsOldest) do sum to their control totals; example:
    ## sum(dataCols$`0-4`[1:89]) - CtrlAgeGrpsTotals$`0-4`[CtrlAgeGrpsTotals$Sex == CurrSex]     ## should be zero
    ## sum(dataCols$`70-74`[1:89]) - CtrlAgeGrpsTotals$`70-74`[CtrlAgeGrpsTotals$Sex == CurrSex] ## should be zero
    ### *************************************************************************************** ###
    # AgeGrpsNotOldest <- setdiff(AgeGrps5Yr, AgeGrpsOldest)
    # check2.3 <- sum(dataCols[1:n_Regions, -1]) == sum(CtrlAgeGrpsTotals[CtrlAgeGrpsTotals$Sex == CurrSex, AgeGrpsNotOldest])
    # if(check2.3 != TRUE) {
    #   stop("Error 2.3 (prorate non-AgeGrpsOldest columns): 5 year age group columns (EXCEPT for AgeGrpsOldest) should sum to the current sex's control total, but they do not.")
    # } else {
    #   if(writeRakingLog == TRUE) {
    #     ## add message to raking_log
    #     raking_log[nrow(raking_log)+1, 1] <- paste0("Part 2.3, Sex ", CurrSex,
    #                                                 " (prorate columns): Remaining 5-year age group columns (",
    #                                                 AgeGrpsNotOldest[1], " through ",
    #                                                 AgeGrpsNotOldest[length(AgeGrpsNotOldest)],
    #                                                 ") now sum to their control total for this Sex.")
    #   }
    # }

    ## remove no-longer-needed objects
    rm(i, CurrCol, temp)  ## check2.3, AgeGrpsNotOldest


    ## Part 2.4: rake prorated data ----
    ## 2K. calc necessary columns: Sum, Ctrl_TOTAL, Diff, adj_value
    ## Step 1: calc actual sum, add in VarRow control totals, calc difference
    ## Step 2: calc adjustment value (difference divided by number of groups)

    ## Region Ctrl_TOTALs for CurrSex (AgeGrpOldest have been subtracted already)
    temp <- data %>% dplyr::select(VarRow, TOTAL = Ctrl_TOTAL)
    dataCols <- calc.cols(data = dataCols, temp, VarRow, n_colGrps)
    dataCols$adj_value <- NULL
    rm(temp)


    ### ** Reorder rows, if necessary **
    ## sort (data) rows in ascending order (min -> max) of row totals (i.e., Sum column)
    dataCols[1:n_rowGrps, ] <- dplyr::arrange(dataCols[1:n_rowGrps, ], Sum)


    ### ** Rake the data: **PrivateRakePreprocessedData, aka, rake **
    ### rake over all rows except for last row (it should be done by default)
    ## At this point, n_colGrps = n_AgeGrps-1 and n_rowGrps = n_Regions

    ## 2L. set CurrRow_value, and pull CurrRow from data to work on, iterate over rows
    CurrRow_value <- 1  ## first row in data first time through; CurrRow[1,]

    while(CurrRow_value < n_rowGrps) {

      CurrRow <- dataCols[CurrRow_value, ]

      ## set up new row for Adjustments; initially, all adjustments = 0
      CurrRow[2, 1] <- "Adjustments"
      CurrRow[2, 2:(n_colGrps+1)] <- 0

      ## set up new row for ADJUSTED current row; = CurrRow + Adjustments
      CurrRow[3, 1] <- "AdjCurrRow"
      CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[1, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

      ## create RowAdj to save all adjustments made that need to be made at end to the last row/reflected in data
      RowAdj <- rep(0, times = n_colGrps)

      ## determine whether need Margin or not
      if(allowNegatives == FALSE) {
        ## if negatives NOT allowed, determine whether we need Margin or not
        if(CurrRow$Sum[1] < CurrRow$Ctrl_TOTAL[1]) {
          needMargin <- TRUE                      ## algorithm C: if values need to be added & negatives not allowed
        } else {
          needMargin <- FALSE                     ## algorithm B: if values need to be subtracted & negatives not allowed
        }
      } else {
        ## else if negatives ARE allowed, Margin is not needed
        needMargin <- FALSE                       ## algorithm A: if negatives ARE allowed
      }

      ## 2M. choose and run appropriate algorithm for Part 2:
      ## 2N. calc "whole people" adjustments to be made to all cells in the row
      ## 2O. calc "residual people" adjustments to be made to selected cells in row
      ## 2P. reconcile column control totals (p.18-23)
      if(allowNegatives == TRUE) {

        ## p.12: A. Adjust cells in the current row if negative values are allowed
        dataCols <- allowNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

      } else {

        if(needMargin == FALSE) {
          ## p.13: B. Adjust cells in the current row if values need to be taken away, negatives not allowed
          ## "If the sum of ages is greater than the control total, then values need to be taken away from
          ## age groups for the current row and added to age groups for rows below the current row."
          dataCols <- noNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

        } else {
          ## i.e., needMargin == TRUE, allowNegatives == TRUE
          ## p.14: C. Adjust cells in the current row if values need to be added, negatives not allowed
          ## "If the sum of ages is less than the control total, then values needed to be added to age
          ## groups for the current row, and taken away from age groups for rows below the current row."
          dataCols <- noNegsneedMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj, needMargin)

        }
      }

      ## update CurrRow_value
      CurrRow_value <- CurrRow_value + 1

    }

    ## 2Q. adjust last row and update Sum & Diff rows
    ## updated Sum row
    dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+2)] <- as.list(colSums(dataCols[1:n_rowGrps, 2:(n_colGrps+2)]))
    ## update Diff row
    dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+2)] <- (dataCols[which(dataCols[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+2)]
                                                                  - dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+2)])
    ## update last row (add Diff to last row)
    dataCols[CurrRow_value, 2:(n_colGrps+2)] <- (dataCols[CurrRow_value, 2:(n_colGrps+2)]
                                                 + dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+2)])
    ## update Ctrl_TOTAL's Diff cell
    dataCols$Diff[CurrRow_value] <- dataCols$Ctrl_TOTAL[CurrRow_value] - dataCols$Sum[CurrRow_value]
    ## update Sum row to reflect Diffs added in
    dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+2)] <- as.list(colSums(dataCols[1:n_rowGrps, 2:(n_colGrps+2)]))
    ## update Diff row to reflect new Diffs
    dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+2)] <- (dataCols[which(dataCols[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+2)]
                                                                  - dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+2)])

    ### ** Put the data back in order, if necessary **
    ## 2R. re-sort rows by InputData order
    rows_order <- InputData %>%
      dplyr::filter(Sex == CurrSex) %>%
      dplyr::select(which(names(InputData) == VarRegion)) %>%
      dplyr::rename(VarRow = which(names(InputData) == VarRegion)) %>%
      dplyr::mutate(row_order = dplyr::row_number(),
                    VarRow = as.character(VarRow))
    dataCols <- dplyr::left_join(dataCols, rows_order, by = "VarRow")
    dataCols[1:n_rowGrps, ] <- dplyr::arrange(dataCols[1:n_rowGrps, ], row_order)
    dataCols$row_order <- NULL

    ## 2S. Insert the MaxAge values back into the data, and add them back on to the Region totals
    ## add back MaxAge data
    dataCols <- dplyr::left_join(dataCols, MaxAge, by = "VarRow")

    ## update Sum with AgeGrpsOldest sum (i.e., TotalOldest)
    dataCols <- dataCols %>%
      dplyr::mutate(Sum = Sum + TotalOldest) %>%
      dplyr::select(VarRow, tidyselect::all_of(AgeGrps5Yr), tidyselect::all_of(ageLast), Sum)

    ## 2T. update OutputData5 with changes for this CurrSex and save as interim file
    n_colGrps <- n_AgeGrps


    ## get just data rows (and data and Sum columns)
    temp <- dataCols[1:n_rowGrps,]

    ## rename VarRow & Sum columns as Region & TOTAL to match with OutputData5, add Sex column back in
    temp <- temp %>%
      dplyr::mutate(Sex = CurrSex) %>%
      dplyr::rename(Region = VarRow, TOTAL = Sum) %>%
      dplyr::select(Region, Sex, tidyselect::everything())

    ## replace in OutputData5 the now raked estimates of 5 year and maximum age group values for CurrSex
    OutputData5 <- OutputData5 %>% dplyr::mutate(Region = as.character(Region))  ## in case region is numeric
    OutputData5[OutputData5$Sex == CurrSex, ] <- temp

    if(saveInterimFiles == TRUE) {
      ## save as interim file
      readr::write_csv(OutputData5, here::here("outputs", "interim_files", paste0("OutputData5_2S_Sex", CurrSex,
                                                                                  "_updated_initial_estimates_Sex_Region_Age_Groups.csv")))
    }

    ### ************************************ Check Point ************************************** ###
    ## At this point, Region rows sum to their control total, AND
    ## 5-year age group columns sum to their control totals, for the current sex
    # sum(dataCols$Sum[1:n_rowGrps]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == CurrSex]  ## should be TRUE
    ### *************************************************************************************** ###
    if(sum(dataCols$Sum[1:n_rowGrps]) != CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == CurrSex]) {
      stop("Error 2.4 (raking AgeGrps data): 5-year age group columns rows for the current sex should sum to their control total, but they do not.")
    } else {
      if(writeRakingLog == TRUE) {
        ## add message to raking_log
        raking_log[nrow(raking_log)+1, 1] <- paste0("Part 2, Sex ", CurrSex, ": 5-year age group values ",
                                                    "have been updated for each Region for this Sex.",
                                                    " See 'outputs\\interim_files' if interested.")
      }
    }

    ### clean up
    rm(temp, data, CurrRow, CurrRow_value, RowAdj, dataCols, rows_order, needMargin, n_colGrps, n_rowGrps, MaxAge)

    ## 2U. move to next Sex
    CurrSex <- CurrSex + 1

  }


  ## Part 2.5: update total Sex values, by 5-year age groups ----
  ## 2V. sum all Sexes to get updated total Sex values
  TotalSex <- OutputData5 %>% dplyr::filter(Sex == 1)  ## initiate with first Sex value
  counter <- 2                                  ## set counter to second Sex value
  while(counter < VarSexTotal) {
    ## in case there are more than 2 non-total sexes
    temp <- OutputData5 %>% dplyr::filter(Sex == counter)
    TotalSex[, -1] <- TotalSex[, -1] + temp[, -1]  ## sum all (even Sex) but Region
    rm(temp)
    counter <- counter + 1
    TotalSex$Sex <- counter     ## needed for when there are more than 2 non-total Sexes (e.g., 1+2+3 != 4)
  }

  ## 2W. replace in OutputData5 the updated estimates of 5 year and maximum age group regional total values for VarSexTotal
  for(i in seq_along(AgeGrps5Yr)) {
    OutputData5[OutputData5$Sex == counter, names(OutputData5) == AgeGrps5Yr[i]] <- TotalSex[, names(TotalSex) == AgeGrps5Yr[i]]
  }
  OutputData5[OutputData5$Sex == counter, names(OutputData5) == ageLast] <- TotalSex[, names(TotalSex) == ageLast]

  if(saveInterimFiles == TRUE) {
    ## save as interim file
    readr::write_csv(OutputData5, here::here("outputs", "interim_files",
                                             paste0("OutputData5_2W_Sex", CurrSex,
                                                    "_updated_initial_estimates_Sex_Region_Age_Groups.csv")))
  }


  ### ************************************ Check Point ************************************** ###
  ## At this point, Region rows (across 5 year age groups) sum to their control total, and
  ## 5 year age group columns sum to their control totals
  ## sum(OutputData5[OutputData5$Sex == 1, 3:23]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1] ## should be TRUE
  ## sum(OutputData5[OutputData5$Sex == 2, 3:23]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2] ## should be TRUE
  ## sum(OutputData5[OutputData5$Sex == 3, 3:23]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3] ## should be TRUE
  ## all(OutputData5[OutputData5$Sex == 3, "TOTAL"] == CtrlRegionTotals$TOTAL)  ## should be TRUE
  ## sum(CtrlPopTotals[CtrlPopTotals$Sex == 1, 2:6]) - sum(OutputData5[OutputData5$Sex == 1, 3])  ## should be zero
  ## sum(CtrlPopTotals[CtrlPopTotals$Sex == 1, 102]) - sum(OutputData5[OutputData5$Sex == 1, 23])  ## should be zero
  ### *************************************************************************************** ###

  ## 2X. clean up
  rm(CurrSex, i, TotalSex, counter)

  if(writeRakingLog == TRUE) {
    ## 2Y. add message to raking_log
    raking_log[nrow(raking_log)+1, 1] <- "Part 2: Now, 5 year and maximum age group values have been updated for each Sex and Region. See 'outputs\\interim_files' if interested."
  }


  #### Part 3 ----
  ## Part 3. Updating initial single year of age estimates, by Sex ----

  ## Part 3.0: prep ----
  ## 3A. set number of single ages (minus Sex and TOTAL columns) and CurrSex
  ## Already set Sexes, n_AgeGrps, AgeGrpMax, AgeGrpsOldest, ageLast, AgeGrps5Yr, etc. in Part 2
  n_Ages <- dim(CtrlPopTotals)[2] - 2
  CurrSex <- Sexes[1]

  ## Part 3.1-3.3: raking done in while() loop for non-total sexes, for() each AgeGrps5Yr ----
  ## 3B. repeat for all Sexes, except Total
  while(CurrSex < VarSexTotal) {

    age <- 1

    for(age in seq_along(AgeGrps5Yr)) {

      ## Part 3.1: prorate rows ----
      ## 3C. set relevant single ages: CurrAgeGrp and AgeSingles
      CurrAgeGrp <- AgeGrps5Yr[age]
      AgeBegin <- stringr::str_split(string = CurrAgeGrp, pattern = "-")[[1]][1]
      AgeEnd <- stringr::str_split(string = CurrAgeGrp, pattern = "-")[[1]][2]
      AgeSingles <- AgeBegin:AgeEnd
      rm(AgeBegin, AgeEnd)

      ## 3D. filter input data by CurrSex and AgeSingles
      ## "OutputData" IS input data at this point as only the TOTAL column has been updated
      data <- OutputData %>%
        dplyr::rename(Sex = {{VarSex}}, VarRow = {{VarRegion}}) %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::select(VarRow, Sex, as.character(AgeSingles[1]), as.character(AgeSingles[2]),
                      as.character(AgeSingles[3]), as.character(AgeSingles[4]), as.character(AgeSingles[5]))


      ### ** Rake across rows to match row totals: PrivateReconcileTotalsByAdding, aka, prorate **
      ## 3E. calc necessary columns (raking by addition prep)
      ## set up: rows = regions w/ last row = Ctrl_TOTAL; cols = single ages, TOTAL (from Part2)
      ## Step 1: calc actual sum, add in VarRow (i.e., Region) control totals, calc difference
      ## Step 2: calc adjustment value (difference divided by number of groups)

      ## temporarily save CurrAgeGrp column from OutputData5 (i.e., Ctrl_TOTAL for current Sex and CurrAgeGrp)
      ## OutputData5 has been updated (in Part 2)
      temp <- OutputData5 %>%
        dplyr::rename(Sex = {{VarSex}}) %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::select(VarRow = Region, TOTAL = tidyselect::all_of(CurrAgeGrp))  ## this is 1:n_Regions X 2 cols (VarRow and updated 5-yr age group)

      ## drop Sex column for now (it's just all = CurrSex) so calc.cols() works properly
      data <- data %>% dplyr::select(-{{VarSex}}) %>% dplyr::mutate(VarRow = as.character(VarRow))

      ## set n_colGrps (batch of 5 single age cols) and n_rowGrps (region rows)
      n_colGrps <- length(AgeSingles)
      n_rowGrps <- n_Regions

      ## calc columns (Sum, Ctrl_TOTAL, Diff, adj_value)
      data <- calc.cols(data, temp, VarRow, n_colGrps)

      ## 3F. prorate row by row (i.e., for 1:n_Regions, prorate so region totals sum to region control totals)
      for (i in 1:n_rowGrps) {

        ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
        ## Step 4: repeat Steps 1 through 3 while difference is not zero
        CurrRow <- data[i, ]

        ## WHILE difference is NOT zero, adjust actual data
        while(abs(CurrRow$Diff) > 0.0000000001) {
          CurrRow <- prorate.row(CurrRow, n_colGrps, allowNegatives)
        }

        ## ensure all numbers are integers (i.e., no fractional people allowed)
        CurrRow[, 2:(n_colGrps + 1)] <- real.to.int(realNums = CurrRow[, 2:(n_colGrps + 1)])

        ## replace actual data with adjusted data in CurrRow
        data[i, ] <- CurrRow

      }
      data$adj_value <- NULL

      ## clean up
      rm(temp, i, CurrRow)

      ### ************************************ Check Point ************************************** ###
      ## At this point, columns across each row (Region) sum to their control total,
      ## BUT rows down each column (single ages) do NOT (necessarily) sum to their control totals (AgeGrp)
      ## sum(data[1:n_rowGrps, 2:(n_colGrps+1)]) - sum(OutputData5[1:n_Regions, CurrAgeGrp])  ## should be zero
      ## sum(data[,names(data) %in% AgeSingles]) - sum(CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, names(CtrlPopTotals) %in% AgeSingles])  ## should be zero
      ## example: sum(data[,names(data) %in% AgeSingles[1]]) - CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, names(CtrlPopTotals) == AgeSingles[1]]   ## likely not zero
      ### *************************************************************************************** ###


      ## Part 3.2: prorate columns ----
      ### ** Rake down columns to match column totals: PrivateReconcileTotalsByAdding, aka, prorate **

      ## 3G. prep to prorate columns
      ## single age Ctrl_TOTALs for CurrSex
      temp <- CtrlPopTotals %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::mutate(VarRow = "Ctrl_TOTAL") %>%
        dplyr::select(VarRow, which(names(CtrlPopTotals) %in% AgeSingles))

      n_colGrps <- length(AgeSingles)
      n_rowGrps <- n_Regions

      ## 3H. calculate necessary rows: Sum (colSums), Ctrl_TOTAL (temp), Diff (subtraction), adj_value (division)
      ## Step 1: calc actual sum, add in VarRow control totals, calc difference
      ## Step 2: calc adjustment value (difference divided by number of groups)
      dataCols <- prep.prorate.col(data, n_rowGrps, colGrps = 1:(n_colGrps+1),
                                   ctrl_total_row = temp, AgeGrpMax = NULL, ageLast = NULL)

      ## 3I. prorate column by column
      for (i in 2:(n_colGrps+1)) {

        ## Step 3: add/subtract adjustment value to/from actual data to get first interim value
        ## Step 4: repeat Steps 1 through 3 while difference is not zero
        CurrCol <- dataCols[ ,c(1, i)]

        ## WHILE difference is NOT zero, adjust actual data
        while(abs(CurrCol[which(CurrCol[, 1] == "Diff"), -1]) > 0.0000000001) {
          CurrCol <- prorate.col(CurrCol, n_rowGrps, allowNegatives)
        }

        ## ensure all numbers are integers (i.e., no fractional people allowed)
        CurrCol[1:n_rowGrps, -1] <- real.to.int(realNums = CurrCol[1:n_rowGrps, -1])

        ## replace actual data with adjusted data in CurrCol
        dataCols[ ,i] <- CurrCol[,-1]

      }

      ## remove no-longer-needed objects
      rm(i, CurrCol, temp)

      ### ************************************ Check Point ************************************** ###
      ## At this point, columns across each row (Region) sum close to their control total,
      ## and rows down each column (single ages) sum to their control totals (age)
      ## sum(dataCols[1,-1]) - OutputData5[CurrSex, CurrAgeGrp]          ## possibly no longer zero, but close
      ## sum(dataCols[1:n_Regions, names(dataCols) == AgeSingles[1]]) - CtrlPopTotals[CurrSex, names(CtrlPopTotals) == AgeSingles[1]]  ## should be zero
      ## sum(dataCols[1:n_Regions, names(dataCols) == AgeSingles[2]]) - CtrlPopTotals[CurrSex, names(CtrlPopTotals) == AgeSingles[2]]  ## should be zero
      ## sum(dataCols[1:n_Regions, names(dataCols) == AgeSingles[3]]) - CtrlPopTotals[CurrSex, names(CtrlPopTotals) == AgeSingles[3]]  ## should be zero
      ## sum(dataCols[1:n_Regions, names(dataCols) == AgeSingles[4]]) - CtrlPopTotals[CurrSex, names(CtrlPopTotals) == AgeSingles[4]]  ## should be zero
      ## sum(dataCols[1:n_Regions, names(dataCols) == AgeSingles[5]]) - CtrlPopTotals[CurrSex, names(CtrlPopTotals) == AgeSingles[5]]  ## should be zero
      ### *************************************************************************************** ###


      ## Part 3.3: rake prorated data ----
      ## 3J. calc necessary columns: Sum, Ctrl_TOTAL, Diff, adj_value
      ## Step 1: calc actual sum, add in VarRow control totals, calc difference
      ## Step 2: calc adjustment value (difference divided by number of groups)

      ## Region Ctrl_TOTALs for CurrSex and CurrAgeGrp
      temp <- OutputData5 %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::select(VarRow = Region, TOTAL = CurrAgeGrp)
      dataCols <- calc.cols(data = dataCols, temp, VarRow, n_colGrps)
      dataCols$adj_value <- NULL
      rm(temp)


      ### ** Reorder rows, if necessary **
      ## 3K. sort (data) rows in ascending order (min -> max) of row totals (i.e., Sum column)
      dataCols[1:n_rowGrps, ] <- dplyr::arrange(dataCols[1:n_rowGrps, ], Sum)


      ### ** Rake the data: **PrivateRakePreprocessedData, aka, rake **
      ### rake over all rows except for last row (it should be done by default)
      ## At this point, n_colGrps = the five single ages and n_rowGrps = n_Regions

      ## 3L. set CurrRow_value, and pull CurrRow from data to work on, iterate over rows
      CurrRow_value <- 1  ## first row in data first time through; CurrRow[1,]

      while(CurrRow_value < n_rowGrps) {

        CurrRow <- dataCols[CurrRow_value, ]

        ## set up new row for Adjustments; initially, all adjustments = 0
        CurrRow[2, 1] <- "Adjustments"
        CurrRow[2, 2:(n_colGrps+1)] <- 0

        ## set up new row for ADJUSTED current row; = CurrRow + Adjustments
        CurrRow[3, 1] <- "AdjCurrRow"
        CurrRow[3, 2:(n_colGrps+1)] <- CurrRow[1, 2:(n_colGrps+1)] + CurrRow[2, 2:(n_colGrps+1)]

        ## create RowAdj to save all adjustments made that need to be made at end to the last row/reflected in data
        RowAdj <- rep(0, times = n_colGrps)

        ## determine whether need Margin or not
        if(allowNegatives == FALSE) {
          ## if negatives NOT allowed, determine whether we need Margin or not
          if(CurrRow$Sum[1] < CurrRow$Ctrl_TOTAL[1]) {
            needMargin <- TRUE                      ## algorithm C: if values need to be added & negatives not allowed
          } else {
            needMargin <- FALSE                     ## algorithm B: if values need to be subtracted & negatives not allowed
          }
        } else {
          ## else if negatives ARE allowed, Margin is not needed
          needMargin <- FALSE                       ## algorithm A: if negatives ARE allowed
        }

        ## 3M. choose and run appropriate algorithm:
        ## 3N. calc "whole people" adjustments to be made to all cells in the row
        ## 3O. calc "residual people" adjustments to be made to selected cells in row
        ## 3P. reconcile column control totals (p.18-23)
        if(allowNegatives == TRUE) {

          ## p.12: A. Adjust cells in the current row if negative values are allowed
          dataCols <- allowNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

        } else {

          if(needMargin == FALSE) {
            ## p.13: B. Adjust cells in the current row if values need to be taken away, negatives not allowed
            ## "If the sum of ages is greater than the control total, then values need to be taken away from
            ## age groups for the current row and added to age groups for rows below the current row."
            dataCols <- noNegsnoMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj)

          } else {
            ## i.e., needMargin == TRUE, allowNegatives == TRUE
            ## p.14: C. Adjust cells in the current row if values need to be added, negatives not allowed
            ## "If the sum of ages is less than the control total, then values needed to be added to age
            ## groups for the current row, and taken away from age groups for rows below the current row."
            dataCols <- noNegsneedMargin(CurrRow, CurrRow_value, data = dataCols, n_colGrps, n_rowGrps, RowAdj, needMargin)

          }
        }

        ## update CurrRow_value
        CurrRow_value <- CurrRow_value + 1

      }


      ## 3Q. adjust last row and update Sum & Diff rows
      dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)] <- as.list(colSums(dataCols[1:n_rowGrps, 2:(n_colGrps+1)]))
      dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+1)] <- (dataCols[which(dataCols[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)]
                                                                    - dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)])
      dataCols[CurrRow_value, 2:(n_colGrps+1)] <- (dataCols[CurrRow_value, 2:(n_colGrps+1)]
                                                   + dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+1)])
      dataCols$Sum[CurrRow_value] <- rowSums(dataCols[CurrRow_value, 2:(n_colGrps+1)])
      dataCols$Diff[CurrRow_value] <- dataCols$Ctrl_TOTAL[CurrRow_value] - dataCols$Sum[CurrRow_value]
      dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)] <- as.list(colSums(dataCols[1:n_rowGrps, 2:(n_colGrps+1)]))
      dataCols[which(dataCols[, 1] == "Diff"), 2:(n_colGrps+1)] <- (dataCols[which(dataCols[, 1] == "Ctrl_TOTAL"), 2:(n_colGrps+1)]
                                                                    - dataCols[which(dataCols[, 1] == "Sum"), 2:(n_colGrps+1)])


      ### ** Put the data back in order, if necessary **
      ## 3R. re-sort rows by InputData order
      rows_order <- InputData %>%
        dplyr::filter(Sex == CurrSex) %>%
        dplyr::select(which(names(InputData) == VarRegion)) %>%
        dplyr::rename(VarRow = which(names(InputData) == VarRegion)) %>%
        dplyr::mutate(row_order = dplyr::row_number(),
                      VarRow = as.character(VarRow))
      dataCols <- dplyr::left_join(dataCols, rows_order, by = "VarRow")
      dataCols[1:n_rowGrps, ] <- dplyr::arrange(dataCols[1:n_rowGrps, ], row_order)
      dataCols$row_order <- NULL


      ### ************************************ Check Point ************************************** ###
      ## At this point, Region rows (across 5 year age groups) sum to their control total, and
      ## 5 year age group columns sum to their control totals, AND now single age values for
      ## the CurrAgeGrp and CurrSex should sum to their control totals (for each region).
      ### *************************************************************************************** ###
      testAll <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles)]) == sum(OutputData5[OutputData5$Sex == CurrSex, AgeGrps5Yr[age]])
      testAge1 <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles[1])]) == CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, which(names(CtrlPopTotals) == AgeSingles[1])]
      testAge2 <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles[2])]) == CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, which(names(CtrlPopTotals) == AgeSingles[2])]
      testAge3 <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles[3])]) == CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, which(names(CtrlPopTotals) == AgeSingles[3])]
      testAge4 <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles[4])]) == CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, which(names(CtrlPopTotals) == AgeSingles[4])]
      testAge5 <- sum(dataCols[1:n_rowGrps, which(names(dataCols) %in% AgeSingles[5])]) == CtrlPopTotals[CtrlPopTotals$Sex == CurrSex, which(names(CtrlPopTotals) == AgeSingles[5])]
      if(all(testAll, testAge1, testAge2, testAge3, testAge4, testAge5) == TRUE) {
        if(writeRakingLog == TRUE) {
          ## add message to raking_log
          raking_log[nrow(raking_log)+1, 1] <- paste0("Part 3, Sex ", CurrSex, ": Single age values ",
                                                      "(for each region) in age group ", CurrAgeGrp,
                                                      " are raked ok.")
        }
        rm(testAll, testAge1, testAge2, testAge3, testAge4, testAge5)
      } else {
        stop(paste0("Error 3.3 (raking single age data): Age group ", CurrAgeGrp, " is NOT raked ok. Something does not balance to one or more Control Total(s)."))
      }


      ## 3S. update OutputData with changes for this CurrSex & CurrAgeGrp

      ## get just data rows and columns
      temp <- dataCols[1:n_rowGrps, 2:(n_colGrps+1)]

      ## replace in OutputData the now raked single age estimates for CurrSex & CurrAgeGrp
      OutputData[OutputData$Sex == CurrSex, as.character(AgeSingles)] <- temp

      ## clean up
      rm(temp, data, CurrRow, CurrRow_value, RowAdj, dataCols, rows_order,
         needMargin, n_colGrps, n_rowGrps, CurrAgeGrp, AgeSingles)

      ## 3T. move to next age in AgeGrps5Yr
      age <- age + 1


    }  ## end for loop through AgeGrps5Yr

    ## replace ageLast values with those run in Part 2 (i.e., saved in OutputData5)
    OutputData[OutputData$Sex == CurrSex, as.character(ageLast)] <- OutputData5[OutputData5$Sex == CurrSex, as.character(ageLast)]

    if(saveInterimFiles == TRUE) {
      ## save as interim file
      readr::write_csv(OutputData, here::here("outputs", "interim_files",
                                              paste0("OutputData_3S_Sex", CurrSex,
                                                     "_raked_estimates_Sex_Region_Ages.csv")))
    }

    ## 3U. move to next Sex
    CurrSex <- CurrSex + 1

  }  ## end while loop through non-total Sexes


  ## Part 3.4: update total Sex values, by single year ages ----
  ## 3V. sum all Sexes to get updated total Sex values
  TotalSex <- OutputData %>% dplyr::filter(Sex == 1)  ## initiate with first Sex value
  counter <- 2                                 ## set counter to second Sex value
  while(counter < VarSexTotal) {
    ## in case there are more than 2 non-total sexes
    temp <- OutputData %>% dplyr::filter(Sex == counter)
    TotalSex[, -1] <- TotalSex[, -1] + temp[, -1]  ## sum all (even Sex) but Region
    rm(temp)
    counter <- counter + 1
    TotalSex$Sex <- counter     ## needed for when there are more than 2 non-total Sexes (e.g., 1+2+3 != 4)
  }

  ## 3W. replace in OutputData the updated estimates of single age group regional total values for VarSexTotal
  OutputData[OutputData$Sex == counter, ] <- TotalSex

  if(saveInterimFiles == TRUE) {
    ## save as interim file
    readr::write_csv(OutputData, here::here("outputs", "interim_files",
                                            paste0("OutputData_3S_Sex", CurrSex,
                                                   "_raked_estimates_Sex_Region_Ages.csv")))
  }

  ## 3X. final check point
  ### ************************************ Check Point ************************************** ###
  ## At this point, columns across each row (Region) sum to their control total,
  ## AND rows down each column (single ages) sum to their control totals (AgeGrp),
  ## AND no cells are negative (if allowNegatives = FALSE).
  ## sum(OutputData[1:n_Regions, 3:(n_Ages+2)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]  ## TRUE
  ## OutputData$TOTAL[OutputData$Sex == VarSexTotal] == CtrlRegionTotals$TOTAL  ## all TRUE
  ## any(OutputData < 0)   ## need FALSE if allowNegatives = FALSE
  ## all(OutputData >= 0)  ## need TRUE if allowNegatives = FALSE
  ### *************************************************************************************** ###
  testCols1 <- sum(OutputData[OutputData$Sex == 1, 3:(n_Ages+2)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 1]
  testCols2 <- sum(OutputData[OutputData$Sex == 2, 3:(n_Ages+2)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 2]
  testCols3 <- sum(OutputData[OutputData$Sex == 3, 3:(n_Ages+2)]) == CtrlPopTotals$TOTAL[CtrlPopTotals$Sex == 3]
  testRows <- all((OutputData$TOTAL[OutputData$Sex == VarSexTotal] == CtrlRegionTotals$TOTAL) == TRUE)
  if(allowNegatives == FALSE & any(OutputData < 0) == FALSE) {
    testCells <- TRUE
  } else {
    if(allowNegatives == FALSE & any(OutputData < 0) == TRUE) {
      testCells <- FALSE
    } else {
      testCells <- TRUE
    }
  }

  if(all(c(testCols1, testCols2, testCols3, testRows, testCells) == TRUE)) {

    ## save as final output file
    readr::write_csv(OutputData, here::here("outputs", "RakedData.csv"))

    if(writeRakingLog == TRUE) {
      # add message to raking_log
      raking_log[nrow(raking_log)+1, 1] <- "SUCCESS. Data has been successfully raked. See 'outputs' for 'RakedData.csv'."
    }
    message("Data has been successfully raked. See 'outputs' for 'RakedData.csv'.")
    rm(testCols1, testCols2, testCols3, testRows, testCells)
    rm(CurrSex, TotalSex, counter, age)

  } else {

    if(writeRakingLog == TRUE) {
      raking_log[nrow(raking_log)+1, 1] <- "FAIL. Something has gone wrong. Check tests."
      if(testCols1 == FALSE) { raking_log[nrow(raking_log)+1, 1] <- ("The sum of raked columns in Sex 1 does not match Population Control Total.") }
      if(testCols2 == FALSE) { raking_log[nrow(raking_log)+1, 1] <- ("The sum of raked columns in Sex 2 does not match Population Control Total.") }
      if(testCols3 == FALSE) { raking_log[nrow(raking_log)+1, 1] <- ("The sum of raked columns in Sex 3 does not match Population Control Total.") }
      if(testRows == FALSE) { raking_log[nrow(raking_log)+1, 1] <- ("One or more rows do not sum to its/their Region Control Total(s).") }
      if(testCells == FALSE) { raking_log[nrow(raking_log)+1, 1] <- ("Negatives are NOT allowed, but one or more raked values are negative.") }
    }
    message("FAIL. Raking has stopped. One or more final checks did not pass: ")
    if(testCols1 == FALSE) { message("The sum of raked columns in Sex 1 does not match Population Control Total.") }
    if(testCols2 == FALSE) { message("The sum of raked columns in Sex 2 does not match Population Control Total.") }
    if(testCols3 == FALSE) { message("The sum of raked columns in Sex 3 does not match Population Control Total.") }
    if(testRows == FALSE) { message("One or more rows do not sum to its/their Region Control Total(s).") }
    if(testCells == FALSE) { message("Negatives are NOT allowed, but one or more raked values are negative.") }
    utils::View(OutputData)
    readr::write_csv(OutputData, here::here("outputs", "RakedData_failed.csv"))

  }
  if(writeRakingLog == TRUE) {
    readr::write_csv(raking_log, here::here("outputs", "raking_log.csv"))
  }

  #### DONE ----
  if(writeRakingLog == TRUE) {
    return(list(RakedData = OutputData,
                RakingLog = raking_log))
  } else {
    return(list(RakedData = OutputData))
  }

}
