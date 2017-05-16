#' @title Convert From Julian Date
#'
#' @description
#' This function takes a Julian date and converts it to a date object.
#' The Julian format expected is a four digit year and the three digit number of the day of the year.
#' For example January 1st's number is 001 and December 31st is either 365 or 366 if it is a leap year.
#' @param j.date Text string in the format of YYYYDDD
#' @return object of class Date
#' @keywords julian, date
#' @examples
#' from.julian("2017090")

from.julian <- function(j.date){

    if(purrr::is_empty(j.date)) {
        return(NA)
    }

    ## Find the year.
    yr <- trunc(j.date/1000)

    yr[yr==0] <- NA

    ## Find number of minutes.
    julian.day <- j.date %% 1000

    ## The first day of this year is our base date.
    jan1.yr <- as.Date(paste(yr, "-01-01", sep=""), format="%Y-%m-%d")

    ## Pass to as.Date function the number of days since the "origin"
    ## date.  Our origin date is Jan. 1, YYYY, which is stored as a Date
    ## object in jan1.yr.  Note that Jan. 1, YYYY is Julian day 001.
    my.date <- as.Date(julian.day-1, origin=jan1.yr)

    return(my.date)
}
