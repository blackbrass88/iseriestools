#' @title Convert To Julian Date
#'
#' @description
#' This function takes a Date object and converts it to a text string in Julian date format.
#' The Julian formatis a four digit year and the three digit number of the day of the year.
#' For example January 1st's number is 001 and December 31st is either 365 or 366 if it is a leap year.
#' @param my.date Date object
#' @return text string in the format of YYYYDDD
#' @keywords julian date
#' @export to_julian
#' @examples
#' to_julian(as.Date("2017-03-31"))

to_julian <- function(my.date){

    ##Find the year.
    yr <- format(my.date, "%Y")

    ## The first day of this year is our base date.
    jan1.yr <- as.Date(paste(yr, "-01-01", sep=""), format="%Y-%m-%d")

    ##Get number of days since jan1.yr
    dy <- my.date - jan1.yr

    ##Adjust days to 3 digits
    dy.text <- paste0(paste0(rep(0, 3 - nchar(dy + 1)), collapse = ""), dy + 1)

    ##Build Julian formated date
    j.date <- paste0(yr, dy.text)

    return(j.date)

}
