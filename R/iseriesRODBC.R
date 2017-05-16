#' @title Connect to iSeries Server
#'
#' @description
#'     Creates a connection device to using an ODBC iSeries driver and odbcDriverConnect from the
#'     RODBC package.
#'
#' @param SystemAddress IP or host name of server. Defaults to SERVICE for my convienence.
#' @param Library Specifiy and optional library to restrict the connection to.
#' @param User username to access server
#' @param Password password to access server
#' @return object of class RODBC
#' @export
#' @seealso \code{\link[RODBC]{odbcConnect}}

iseriesRODBC <- function(SystemAddress="SERVICE", Library=NA, User, Password){
    if(is.na(Library)){
        Channel <- RODBC::odbcDriverConnect(connection = paste0("Driver={iSeries Access ODBC Driver}",
                                                         ";System=",SystemAddress,
                                                         ";ReadOnly=True",
                                                         ";uid=",User,
                                                         ";pwd=",Password),
                                     readOnlyOptimize = TRUE)
    } else {
        Channel <- RODBC::odbcDriverConnect(connection = paste0("Driver={iSeries Access ODBC Driver}",
                                                         ";System=",SystemAddress,
                                                         ";DBQ=",Library,
                                                         ";ReadOnly=True",
                                                         ";uid=",User,
                                                         ";pwd=",Password),
                                     readOnlyOptimize = TRUE)
    }
    rm(Password)
    return(Channel)
}
