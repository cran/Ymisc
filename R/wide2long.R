#' @title Reshapes the data
#' @description
#' wide2long() function is data-reshaping function for wide format data.
#' This function mainly targets the brain structure data that contains the data from the left and right hemispheres
#'
#' @param data The wide format data.
#' @param ID The column of identifiers.
#' @param separator A character vector that separates characters in the variable names.
#' @param hemisphere Whether a hemisphere indicator in the variable names is a prefix or suffix. At this point, only the "prefix" option is available.
#' @param start The column that specifies the starting point of a set of variables to be reshaped
#' @param end The column that specifies the endpoint of a set of variables to be reshaped
#' @return The long format data
#'
#' @export
#'
#' @import stats
#'
#' @examples
#'
#' data(sample_data)
#'
#' long<-wide2long(
#' data=sample_data,
#' ID="ID",
#' separator="_",
#' start="lh_Thalamus",
#' end="rh_AccumbensArea",
#' hemisphere="prefix"
#' )


wide2long<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  if(hemisphere!="prefix"){
    stop("Hemisphere indicator should be the prefix.")
  } else{


    start <- match(start,names(data))
    end <- match(end,names(data))

    d <- stats::reshape(data=data,
                    direction="long",
                    idvar=ID,
                    varying=start:end,
                    sep=separator,
                    timevar="region")

    rownames(d) <- NULL

  data <- data.frame(d[order(d[,ID]),])


  }

  return(data)

}

