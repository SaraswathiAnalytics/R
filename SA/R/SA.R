#' Fill NA values
#'
#' The function \code{Fill_NA()} is used to fill \code{NA} values with the same data type values.
#' It works on vectors & Data Frames with respect to character, factor, numeric,integer data types.
#' If any other data types is used this function can't be apply.
#'
#' @param data A vector or data frame
#' @param replace character. If there exists \code{'MEAN'} in given argument then replace \code{NA} values with \code{\emph{mean}}.
#' if \code{'MEDIAN'} then replace with \code{\emph{median}}, otherwise replace with 0.
#' @return Returns \code{NA} filled vector or data frame
#' @author Naveen Kumar M.Sc., \emph{Email}: \email{info@saraswathianalytics.com} OR \emph{WhatsApp}: \href{https://wa.me/918688896472}{Click Here}
#' @examples
#' v1 <- c(1,2,3,NA, 5.2)
#' v2 <- c(NA,'a','a','b','d')
#' v3 <- c(3+2i,4,6,NA,1)
#' df <- data.frame(v1,v2,v3);df
#' str(df)
#' Fill_NA(df) # example of \code{data.frame}
#' Fill_NA(df,  replace = 'MEDIAN')  example with data.frame
#' Fill_NA(v1, replace = 'MEAN')  example with vector
#'
#' @seealso \link{Missing_C} for missing count, \link{Missing_P} getting missing percentage
#' @export

Fill_NA <- function(data, replace = 0)
{
  if(is.vector(data) == TRUE)
  {
    if(is.character(data) == TRUE | is.factor(data) == TRUE)
    {
      data <- data[!is.na(data)]
      data <- unique(data)[which.max(tabulate(match(data,unique(data))))]
    }else if(is.numeric(data) == TRUE | is.integer(data) == TRUE)
    {
      if(replace == 0)
      {
        data[is.na(data)] = 0
      }else if(replace == 'MEAN')
      {
        data[is.na(data)] = mean(data, na.rm = TRUE)
      }else if(replace == 'MEDIAN')
      {
        data[is.na(data)] = median(data, na.rm = TRUE)
      }
    }
  }else if(is.data.frame(data) == TRUE)
  {
    for(i in 1:ncol(data))
    {
      if(is.character(data[,i]) == TRUE | is.factor(data[,i]) == TRUE)
      {
        Mode <-  function(x)
        {
          x <- x[!is.na(x)]
          m <- unique(x)[which.max(tabulate(match(x,unique(x))))]
          return(m)
        }
        data[is.na(data[,i]),i] = Mode(data[,i])
      }

      if(is.numeric(data[,i]) == TRUE | is.integer(data[,i]) == TRUE)
      {
        if(replace == 0)
        {
          data[is.na(data[,i]),i] = 0
        }else if(replace == 'MEAN')
        {
          data[is.na(data[,i]),i] = mean(data[,i], na.rm = TRUE)
        }else if(replace == 'MEDIAN')
        {
          data[is.na(data[,i]),i] = median(data[,i], na.rm = TRUE)
        }
      }
    }

  }
  return(data)
}

#' Mode
#'
#' The function \code{Mode} returns mode of the given vector (or) data frame along with same data type values.
#' It's working on vectors & Data Frames.
#'
#' @param data A vector or data frame
#' @return The mode of the input
#' @author Naveen Kumar M.Sc., \emph{Email}: \email{info@saraswathianalytics.com} OR \emph{WhatsApp}: \href{https://wa.me/918688896472}{Click Here}
#' @examples
#' v1 <- c("B","A","A","B","A")
#' v2 <- c(NA,1,2,3,2)
#' v3 <- c(3+2i,8,6,NA,1)
#' df <- data.frame(v1,v2,v3);df
#' Mode(v1) # passing vector
#' Mode(df) # passing dataframe
#' @seealso \link{Fill_NA} for filling \code{NA} values, \link{Missing_P} for missing percentage and \link{Missing_C} for missing count
#' @export

Mode <-  function(data)
{
  if(is.vector(data)==TRUE)
  {
    data <- data[!is.na(data)]
    m <- unique(data)[which.max(tabulate(match(data,unique(data))))]
    return(m)
  }else if(is.data.frame(data)==TRUE)
  {
    MODE <- data.frame(row.names = 'Mode')
    for(i in 1:ncol(data))
    {
      Var_Name=names(data)[i]
      val <- data[Var_Name][!is.na(data[Var_Name])]
      MODE[[Var_Name]] <- unique(val)[which.max(tabulate(match(val,unique(val))))]
    }
    return(MODE)
  }
}




#' Missing Percentage
#'
#' The function \code{Missing_P} returns missing percentage of the given vector (or) data frame.
#' It's working on vectors & Data Frames.
#'
#' @param x A vector or data frame
#' @return The missing percentage of input
#' @author Naveen Kumar M.Sc., \emph{Email}: \email{info@saraswathianalytics.com} OR \emph{WhatsApp}: \href{https://wa.me/918688896472}{Click Here}
#' @examples
#' v1 <- c('A','B','C',NA,'D',NA)
#' v2 <- c(NA,1,2,3,2,6)
#' v3 <- c(3+2i,8,6,NA,1,NA)
#' v4 <- c(3,8,6,1,1,7)
#' df <- data.frame(v1,v2,v3,v4);df
#' Missing_P(v1) # passing vector
#' Missing_P(df) # passing dataframe
#' @seealso \link{Fill_NA} for filling \code{NA} values, \link{Mode} for mode, \link{Missing_C} for missing count
#' @export


Missing_P <- function(x)
{
  if(is.vector(x) == TRUE)
  {
    cat(round((sum(is.na(x))/length(x))*100),"%",sep = "")
  }else if(is.data.frame(x) == TRUE)
  {
    Missing <- data.frame(row.names = 'Missing%')
    for(i in 1:ncol(x))
    {
      Var_Name=names(x)[i]
      Missing[[Var_Name]] <- paste(round((sum(is.na(x[Var_Name]))/length(x[,i]))*100),"%",sep = "")
    }
    return(Missing)
  }
}



#' Missing count
#'
#' The function \code{Missing_C} returns missing count of the given vector (or) data frame.
#' It's working on vectors & Data Frames.
#'
#' @param x A vector or data frame
#' @return The missing count of input
#' @author Naveen Kumar M.Sc., \emph{Email}: \email{info@saraswathianalytics.com} OR \emph{WhatsApp}: \href{https://wa.me/918688896472}{Click Here}
#' @examples
#' v1 <- c('A','B','C',NA,'D',NA)
#' v2 <- c(NA,1,2,3,2,6)
#' v3 <- c(3+2i,8,6,NA,1,NA)
#' v4 <- c(3,8,6,1,1,7)
#' df <- data.frame(v1,v2,v3,v4);df
#' Missing_C(v1) # passing vector
#' Missing_C(df) # passing dataframe
#' @seealso \link{Fill_NA} for filling \code{NA} values, \link{Missing_P} for missing percentage
#' @export

Missing_C <- function(x)
{
  if(is.vector(x) == TRUE)
  {
    return(sum(is.na(x)))
  }else if(is.data.frame(x) == TRUE)
  {
    Missing <- data.frame(row.names = 'Missing Count')
    for(i in 1:ncol(x))
    {
      Var_Name=names(x)[i]
      Missing[[Var_Name]] <- sum(is.na(x[Var_Name]))
    }
    return(Missing)
  }
}


#' making X_Mas tree
#'
#' The function \code{X_Mas} returns Christmas tree.
#'
#' @param locator A \code{character}, if \code{locator = 'locator'} Reads the position of the graphics cursor when the (first) mouse button is pressed or
#' location may also be specified by setting x to a single keyword from the list \code{"bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright",
#'  "right" and "center"}. This places the legend on the inside of the plot frame at the given location.
#'
#' @return Return Christmas tree
#' @author Naveen Kumar M.Sc., \emph{Email}: \email{info@saraswathianalytics.com} OR \emph{WhatsApp}: \href{https://wa.me/918688896472}{Click Here}
#' @seealso \link{legend} for locating legend, \link{locator} for position of the graphics cursor.
#'
#' @export

X_Mas <- function(locator = "topright")
{
  wish = readline(prompt = 'Enter wish style :')
  art = readline(prompt = 'Enter Art by...! :')
  plot(1:10,1:10,xlim = c(-5,5),main= wish,ylim = c(0,10),xlab = " ",ylab = " ",type = 'n',xaxt="n",yaxt="n")
  rect(-1,0,1,2,col = 'tan3',border = 'tan4',lwd = 3)
  polygon(c(-5,0,5),c(2,4,2),col = 'palegreen3',border = "palegreen4",lwd=3)
  polygon(c(-4,0,4),c(3.5,5.5,3.5),col = 'palegreen4',border = "palegreen3",lwd=3)
  polygon(c(-3,0,3),c(5,6.5,5),col = 'palegreen3',border = "palegreen4",lwd=3)
  polygon(c(-2,0,2),c(6.25,7.5,6.25),col = 'palegreen4',border = "palegreen3",lwd=3)

  # add some ornaments
  points(x=runif(4,-5,5),y=rep(2,4),col=sample(c('blue','red'),size = 4,replace = T),cex=3,pch=19)
  points(x=runif(4,-4,4),y=rep(3.5,4),col=sample(c('blue','red'),size = 4,replace = T),cex=3,pch=19)
  points(x=runif(4,-3,3),y=rep(5,4),col=sample(c('blue','red'),size = 4,replace = T),cex=3,pch=19)
  points(x=runif(4,-2,2),y=rep(6.25,4),col=sample(c('blue','red'),size = 4,replace = T),cex=3,pch=19)
  points(0,7.5,pch=8,cex=5,col='gold',lwd=3)

  # add some presents
  xPress = runif(10,-4.5,4.5)
  xWidth = runif(10,0.1,.5)
  xHeight = runif(10,0,1)
  for(i in 1:10)
  {
    rect(xPress[i]-xWidth[i],0,xPress[i]+xWidth[i],xHeight[i],col = sample(c('blue','red'),size = 1))
    rect(xPress[i]-0.2*xWidth[i],0,xPress[i]+0.2*xWidth[i],xHeight[i],col = sample(c('gold','grey87'),size = 1))

  }
  if(locator == 'locator')
  {
    legend(locator(1), art, title = 'Art by...', col = 'skyblue')
  }else{
    legend(locator, art, title = 'Art by...', col = 'skyblue')
  }
}

