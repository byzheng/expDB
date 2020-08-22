# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:31 PM Sunday, 19 August 2012
# * Copyright: AS IS
# *

# expDB API for traits

#' Insert or update trait into expDB

#' @param con a connection object as produced by dbConnect
#' @param data A data frame includes all columns
#' @export
dbAddTraits <- function(con, data)
{
    names(data) <- tolower(names(data))
    dbInsertUpdateByRow(con, 'expdb_trait', data)
    return(invisible())
}

#' Get trait list

#' @param con a connection object as produced by dbConnect
#' @export
dbGetTraits <- function(con)
{
    return(DBI::dbReadTable(con, 'expdb_trait'))
}