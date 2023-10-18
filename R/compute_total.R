#' @title the bilateral (Left + Right) measures.
#'
#' @description
#' compute_total() creates new columns that are the bilateral (Left + Right) measures.
#'
#' @param data The wide format data
#' @param ID The column of identifiers.
#' @param left_hemisphere The prefix or suffix that indicates the left hemisphere in the variable names
#' @param right_hemisphere The prefix or suffix string that indicate the right hemisphere in the variable names
#' @param hemisphere The character vector that indicates whether a hemisphere indicator in the variable names is a prefix or suffix.
#' @param separator A character vector that separates characters in the variable names.
#' @param start The column that specifies the starting point of a set of variables to calculate the bilateral (L+R) measures.
#' @param end The column that specifies the endpoint of a set of variables to calculate the bilateral (L+R) measures.
#' @return The data with the bilateral (L+R) measures.
#'
#' @export
#'
#' @examples
#'
#' data(sample_data)
#'
#' compute_total(sample_data,
#' left_hemisphere="lh",
#' right_hemisphere="rh",
#' separator="_",
#' ID="ID",
#' hemisphere="prefix",
#' start="lh_Thalamus",
#' end="rh_AccumbensArea")

compute_total <- function(data=sample_data,
                       left_hemisphere="lh",
                       right_hemisphere="rh",
                       separator="_",
                       ID="ID",
                       hemisphere="prefix",
                       start,
                       end) {




  start <- match(start,names(data))
  end <- match(end,names(data))

  NID <- match(ID,names(data))
  data2 <- data[,c(start:end,NID)]

  namelist <- colnames(data[,c(start:end)])

  slist <- strsplit(namelist,split = separator)

  llist <- list()

  if(hemisphere=="prefix"){

    for(i in 1:length(slist)){
      llist[i] <- slist[[i]][2]
    }

  } else if(hemisphere=="suffix"){

    for(i in 1:length(slist)){
      llist[i] <- slist[[i]][1]
    }
  }
  llist <- unique(llist)

  for( i in 1:length(llist)){

    data2[[paste0("total",separator,llist[[i]])]] <-
      data2[[paste0(left_hemisphere,separator,llist[[i]])]] + data2[[paste0(right_hemisphere,separator,llist[[i]])]]

  }

  data4 <- data2[,grep("total", names(data2))]
  data <- data.frame(cbind(data,data4))

  return(data)
}


