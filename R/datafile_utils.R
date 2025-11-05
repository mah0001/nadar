#' @title read data file
#'
#' @description read data(csv) file and return the dataframe
#'
#' @param variables variable description
#' @param file input data(csv) file path
#' @param type Type of data file (csv | tsv)
#'
#' @return A dataframe
#'
#' @examples  .read_data({},'D:\\MetadataEditorSource\\MetadataEditor_Dev\\MetadataEditor\\Sample Data\\in\\HND_2012_L2L.csv')
#'
#' @export .read_data

.read_data <- function(variables,
                       file="",
                       type="csv",
                       digits="",
                       ...)
{
  csvHeader <- c()
  if(type == "tsv"){
    # Read tsv header to pick the column names
    csvHeader <- as.character(read_tsv(file=file, n_max = 1, col_types = cols(.default = "c"), col_names = FALSE))
  } else {
    # Read csv header to pick the column names
    csvHeader <- as.character(read_csv(file=file, n_max = 1, col_types = cols(.default = "c"), col_names = FALSE))
  }

  # Column classes required while reading CSV or Table.
  # If column classes not provided, then R guess the type of column and lead to issues like leading zero missing
  columnClasses <-lapply(csvHeader, function(x) {
    if(x %in% variables$internalName){
      variable.info <- variables[variables$internalName == x,]
      varType <- ifelse(is.null(variable.info$type), "character", as.character(variable.info$type))
      # For numeric variables exact data type should be provided, Otherwise precision may lost
      dataType <- ifelse(is.null(variable.info$varType), "character", as.character(variable.info$varType))
      c(switch( varType, "character" = "c", "numeric" = switch(dataType, "double" = "d", "integer" = "i", "number" = "n", "d"), "date" = "D",  "?"))
    } else{
      # defualt will be ?, default data type
      c("?")
    }
  })

  # set options digits to handle larger decmal values, otherwise decimal will be trimmed off
  if(digits != ""){
    option.digits <- getOption("digits")
    on.exit( options("digits"=option.digits))
    options("digits"=as.numeric(digits))
  }

  # Read CSV with the options given below
  # header=TRUE               - header should be included
  # colClasses=columnClasses  - data type of each column
  # sep=","                   - delemeter comma
  # na="*"                    - na replaced with *
  DF_DATA <- NA
  if(type == "tsv"){
    DF_DATA <- read_tsv(file, col_names = TRUE, col_types =columnClasses, na = c("", "*"), quoted_na = TRUE)
  } else {
    DF_DATA <- read_csv(file, col_names = TRUE, col_types =columnClasses, na = c("", "*"), quoted_na = TRUE)
  }

  # Get column names of DF, convert tibble to dataframe, assign the column names
  # Resolve the issue of spanish nada_study_list, column names were converted to unicode chars while converting to df
  colnames <- colnames(DF_DATA)
  DF_DATA <- data.frame(DF_DATA)
  colnames(DF_DATA) <- colnames

  return(DF_DATA)
}


#' @title variable statistics
#'
#' @description calculate variable statistics
#'
#' @param df dataframe
#' @param varName variable name
#'
#' @return A list contain variable statistics
#'
#' @examples  .summary_stats(df,'uqnr')
#'
#' @export .summary_stats

.summary_stats <- function(df,
                           varName,
                           ...)
{

  # set options digits (22 - max value) to make sure decimal precision not lost in aritmetic operations(MIN, MAX)
  # option.digits <- getOption("digits")
  # on.exit( function() options("digits"=option.digits) )
  # options("digits"=22)

  minValue <- NA
  maxValue <- NA
  meanValue <- NA
  stdDevValue <- NA

  if (sapply(df[varName], is.numeric)) {
    if(all(is.na(df[varName]))){
      minValue <- "*"
      maxValue <- "*"
      meanValue <- "*"
      stdDevValue <- "*"
    } else {
      #calculate satistics
      minValue <- min(df[varName], na.rm=T)
      maxValue <- max(df[varName], na.rm=T)
      meanValue <- sapply(df[varName], mean, na.rm = TRUE)
      stdDevValue <- sapply(df[varName], sd, na.rm = TRUE)
    }
  }

  invalids <- sum(is.na(df[varName]))
  valids <-  sum(!is.na(df[varName]))

  statsList <- list(min=minValue,max=maxValue,mean=meanValue,stdev=stdDevValue,vald=valids,invd=invalids)

  return(statsList)

}


#' @title variable summary
#'
#' @description get variable summary statistics
#'
#' @param df dataframe
#' @param varName variable name
#'
#' @return A list contain variable summary statistics
#'
#' @examples  .varStats(df,'uqnr')
#'
#' @export .varStats

.varStats <- function(df,
                      varName,
                      ...)
{
  #calculate satistics
  # summary statistics logic moved to new fn: to reuse the logic in import data
  statistics <- .summary_stats(df, varName)

  statsList <- list(
    valrng=list(range=list(min=statistics$min,max=statistics$max,mean=statistics$mean,stdev=statistics$stdev,name=varName)),
    sumStat=list(list(type="vald",text=statistics$vald),list(type="invd",text=statistics$invd))
  )

  return(statsList)
}

#' @title calculate category frequency
#'
#' @description calcuilate category frequency od the selected variable
#'
#' @param df dataframe
#' @param catgryDF categories
#' @param varName variable name
#'
#' @return list of categories
#'
#' @examples  .calculate_catFrequency(x)
#'
#' @export .calculate_catFrequency
.calculate_catFrequency <- function(df,
                                    catgryDF,
                                    varName,
                                    ... )
{
  # calculate frequency of the given categories
  freqTable <- count(df[[varName]])
  colnames(freqTable) <- c("Value","freq")

  catgry <- lapply(catgryDF$catValu, function(val){
    if(is.na(val)){
      catg <- catgryDF[is.na(catgryDF$catValu),]
      freq <-  freqTable[is.na(freqTable$Value), 'freq']
    }
    else{
      catg <- na.omit(catgryDF[catgryDF$catValu==val,])
      freq <- na.omit(freqTable[freqTable$Value==val, 'freq'])
    }
    if(length(freq) == 0 || is.na(freq)){
      freq <- 0
    }
    list(catValu=val,labl=catg$labl,labelled=TRUE,catStat=list(type="freq",text=freq))
  })

  return(catgry)

}


#' @title format number
#'
#' @description convert numeric vector to string format with correct decimal precisions
#'
#' @param x variable
#' @param dcml number of decimal
#'
#' @return formatted variable
#'
#' @examples  .format_num(x)
#'
#' @export .format_num
.format_num <- function( x,
                         dcml=0,
                         ...)
{

  # function to convert numeric vector to string format with correct decimal precisions
  # to handle the scenario where dcml greater than the length of the variable
  # for ex: value =  1, dcml = 2, then value will be converted to 1.00'

  dcml <- as.numeric(dcml)

  # format function returns 'NA' and not na and the length of vector become 2 instead of 1
  # excluded na while applying format.
  is_na <- is.na(x)
  # round to the number of decimal places if dcml > 0
  if(dcml > 0) {
    x[!is_na] <- format(round(x[!is_na],dcml),nsmall=dcml)
  } else {
    # remove trailing zeros, came from csv read with options digits=22
    # used format option digits=15 to make sure the precision is not lost for large numbers
    # using too large a value of digits may lead to representation errors in the calculation of the number of significant digits and the decimal representation: these are likely for digits >= 16
    x[!is_na] <- format(x[!is_na], digits=15, drop0trailing=TRUE)
  }

  # trim white spaces introduced by format fn, to get the exact width
  x <- trimws(x, which = c("both", "left", "right"))

  return (x);
}


#' @title variable width
#'
#' @description Calculate the variable width
#'
#' @param variable variable
#' @param type variable type
#' @param catValues variable categories
#'
#' @return variable width
#'
#' @examples .varibale_width(var1, 'numeric', c() )
#'
#' @export .varibale_width
.varibale_width <- function(variable,
                            type,
                            catValues,
                            ...)
{
  ## --- Setup ---
  # if all values of that variable is NA then set length 1
  # when wrting to CSV we write all empty string or NA ('' | NA) to '*', when reading we convert * to NA
  if(all(is.na(variable))) {
    variable.length <- 1
  } else {
    # TO DO: Check the internal logic of fn: format.info
    variable.length <- format.info(variable)
  }

  # If variable is a factor include category values in the width calculation
  if(!(is.na(catValues) || length(catValues) == 0)) {
    # calculate max of category value length
    # fomat.info is not used here, unicode issue may not come expecting category values as numeric
    catvalue.length <- max(nchar(as.character(catValues)), na.rm = TRUE)
    variable.length <- max(catvalue.length, variable.length)
  }

  return (variable.length)
}


#' @title read matching variables
#'
#' @description Read CSV file and return only the matching variables
#'
#' @param variables variables
#' @param file data file
#' @param default default type to be used when reading the CSV file
#'
#' @return dataframe
#'
#' @examples .read_matchingVariables(var1, 'c://f1.csv' )
#'
#' @export .read_matchingVariables
.read_matchingVariables <- function(variables,
                                    file="",
                                    default="?",
                                    ...)
{
  # function read csv file, get matching columns as dataframe
  # if any new variable then set NA
  # return the matching fatadataframe

  # read CSV file and find matching variables
  csvHeader <- as.character(read_csv(file=file, n_max = 1, col_types = cols(.default = "c"), col_names = FALSE))
  matchingVariables <- intersect(variables, csvHeader)

  # Read CSV and create DF for matching variables
  DF_DATA <- suppressWarnings(suppressMessages(read_csv(file, col_types = cols(.default = default), col_names = TRUE, na = c("", "*"), quoted_na = TRUE)))
  matchingDF <- DF_DATA [,matchingVariables]

  # dataframe subset returns list if only one variable is selected, convert to dataframe if so
  if (!is.data.frame(matchingDF)) {
    matchingDF <- data.frame(matchingDF, stringsAsFactors=FALSE)
    colnames(matchingDF) <- matchingVariables
  }

  # find the new variables, not exists in CSV and set NA
  newVariables <- setdiff(variables, matchingVariables)
  matchingDF[newVariables] <- NA

  return(matchingDF)

}

#' @title find stata data type
#'
#' @description find stata data type of a numeric variable
#'
#' @param data variable
#'
#' @return stata data type
#'
#' @examples .stata_datatype(data)
#'
#' @export .stata_datatype
.stata_datatype <- function(data,
                            ...)
{
  tryCatch({
    if(all(is.na(data))){
      return("double")
    } else {
      data <- as.numeric(data)
      ## --- Setup ---
      # Get the number of decimal places
      decimalplaces <- lapply(data,function(x) {
        if (!is.na(x) && (x %% 1) != 0) {
          nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
          0
        }
      })

      ## Numeric data types and value ranges
      #  Reference - http://wlm.userweb.mwn.de/wstatvar.htm
      #  byte	integer values between -127 and 100
      #  int	integer values between -32,767 and 32,740
      #  long	integer values between -2,147,483,647 and 2,147,483,620
      #  float	real numbers (i.e. numbers with decimal places) with about 8 digits of accuracy
      #  double	real numbers (i.e. numbers with decimal places) with about 16 digits of accuracy

      type <- double

      maxdecimalplaces <- max(as.numeric(decimalplaces))

      if (maxdecimalplaces > 8) {
        # double - real value with about 16 digits of accuracy
        type <- "double"
      } else if (maxdecimalplaces > 0) {
        # float - real value with about 8 digits of accuracy
        type <- "float"
      } else {
        # integer values
        minValue <- min(as.numeric(data), na.rm=T)
        maxValue <- max(as.numeric(data), na.rm=T)

        if( minValue > -127 && maxValue < 100 ){
          # byte -    integer values between -127 and 100
          type <- "byte"
        } else if( minValue > -32767 && maxValue < 32740 ){
          # int -     integer values between -32,767 and 32,740
          type <-"int"
        } else{
          # long -	integer values between -2,147,483,647 and 2,147,483,620
          type <- "long"
        }
      }

      return(type)
    }
  }, error = function(err) {
    return("double")
  })
}

#' @title  Write fixed width format
#'
#' @description  Write fixed width format (Note: the write.fwf method is copied from 'gdata' R package since we are not able to load the package because of the dependencies like perl)
#'
#' @param x dataframe
#' @param file output file path
#'
#' @return write format
#'
#' @examples .write_fwf(df, 'c://test.txt')
#'
#' @export .write_fwf
.write_fwf <- function(x,
                       file="",
                       locale="",
                       encoding="UTF-8",
                       append=FALSE,
                       quote=FALSE,
                       sep=" ",
                       na="",
                       rownames=FALSE,
                       colnames=TRUE,
                       rowCol=NULL,
                       justify="left",
                       formatInfo=FALSE,
                       quoteInfo=TRUE,
                       width=NULL,
                       eol="\n",
                       qmethod=c("escape", "double"),
                       scientific=TRUE,
                       ...)
{
  ## --- Setup ---

  # if (!(is.na(locale) || locale == '')){
  #   Sys.setlocale("LC_CTYPE", locale=locale)
  # }

  dapply <- function(x, FUN, ..., simplify=TRUE)
  {
    if(is.data.frame(x))
      return(sapply(x, FUN, ..., simplify=simplify))
    else if(is.matrix(x))
      return(apply(x, 2, FUN, ...))
    else
      stop("x must be a data.frame or a matrix")
  }

  if(!(is.data.frame(x) || is.matrix(x)))
    stop("'x' must be a data.frame or matrix")
  if(length(na) > 1)
    stop("only single value can be defined for 'na'")

  if(!scientific)
  {
    option.scipen <- getOption("scipen")
    on.exit( function() options("scipen"=option.scipen) )
    options("scipen"=100)
  }


  if(rownames) {
    x <- as.data.frame(x)
    x <- cbind(rownames(x), x)
    rowColVal <- ifelse(!is.null(rowCol), rowCol, "row")
    colnames(x)[1] <- rowColVal
  }
  colnamesMy <- colnames(x)
  if(length(colnamesMy)==0)
    colnamesMy <- paste( "V", 1:ncol(x), sep="")

  nRow <- nrow(x)
  nCol <- length(colnamesMy)

  widthNULL <- is.null(width)
  if(!widthNULL && length(width) != nCol) {
    warning("recycling 'width'")
    widthOld <- width
    width <- integer(length=nCol)
    width[] <- widthOld
  }

  ## --- Format info ---

  retFormat <- data.frame(colname=colnamesMy,
                          nlevels=0,
                          position=0,
                          width=0,
                          digits=0,
                          exp=0,
                          stringsAsFactors=FALSE)

  ## Which columns are numeric like
  isNum <- dapply(x, is.numeric)
  ## is.numeric picks also Date and POSIXt
  isNum <- isNum & !(dapply(x, inherits, what="Date") |
                       dapply(x, inherits, what="POSIXt"))

  ## Which columns are factors --> convert them to character
  isFac <- dapply(x, is.factor)
  if(any(isFac))
    ## This conditional is necessary because if x is a matrix, even if
    ## all(isFAC==FALSE), this assignment will coerce it to mode
    ## character.  This isn't a problem for dataframes.
    x[, isFac] <- sapply(x[, isFac, drop=FALSE], as.character)

  ## Collect information about how format() will format columns.
  ## We need to get this info now, since format will turn all columns to character
  tmp <- dapply(x, format.info, ..., simplify=FALSE)
  if(is.matrix(x)) tmp <- as.data.frame(tmp)
  tmp1 <- sapply(tmp, length)
  tmp <- t(as.data.frame(tmp))
  retFormat$width <- tmp[, 1]
  ## Collect other details for numeric columns
  if(any(isNum)) {
    ## Numeric columns with digits
    test <- tmp1 > 1
    if(any(test)) {
      retFormat[test, c("digits", "exp")] <- tmp[test, c(2, 3)]
      ## Numeric columns with scientific notation
      test2 <- tmp[test, 3] > 0
      if(any(test2)) ## adding +1; see ?format.info
        retFormat[test, ][test2, "exp"] <- retFormat[test, ][test2, "exp"] + 1
    }
  }

  ## --- Format ---

  ## store original object in 'y'
  y <- x

  ## Formatting (to character)
  for(i in 1:nCol) {
    if(widthNULL) {
      tmp <- NULL
    } else {
      tmp <- width[i]
    }
    ## Due to na.encode bug in format() in 2.7.1; na.encode=TRUE should
    ## return NA values and not "NA", but even then we rely on the
    ## following test to "fiddle" with the value in 'na' argument since -
    ## NA should not increase the width of column with width 1, while wider
    ## value for 'na' should increase the width
    test <- is.na(y[, i])
    ## Make a copy to make sure we get character after first format() - Date class caused problems
    x2 <- character(length=nRow)
    ## Add formatted values
    x2[!test] <- format(y[!test, i], justify=justify, width=tmp, ...) #THIS LINE IS PROBLEM
    #x2[!test] <- y[!test, i]

    ## Add 'na' value
    x2[test] <- na
    ## Replace the original
    x[, i] <- x2
    ## Collect width (again)
    tmp2 <- format.info(x2, ...)[1]
    ## Reformat if 'na' value change the width of the column
    if(tmp2 != retFormat[i, "width"]) {
      retFormat[i, "width"] <- tmp2
      ## ifelse() makes sure that numeric columns are justified to right
      #x[, i] <- format(x[, i], justify=ifelse(isNum[i], "right", justify), width=tmp, ...) #THIS LINE IS ALSO PROBLEM

    }
    ## Reformat 'na' value if it is narrower than the width of the column
    if(nchar(na) < retFormat[i, "width"]) {
      x[test, i] <- format(na, justify=ifelse(isNum[i], "right", justify),
                           width=retFormat[i, "width"], ...)
    }
  }

  ## Number of levels for "non-numeric"" columns
  if(any(!isNum)) {
    retFormat[!isNum, "nlevels"] <- dapply(x[, !isNum, drop=FALSE],
                                           function(z) length(unique(z)))
  }

  ## Check that width was not to small
  if(!widthNULL) {
    test <- retFormat$width > width
    if(any(test)) {
      tmpCol <- paste(colnamesMy[test], collapse=", ")
      tmpWidth <- paste(width[test], collapse=", ")
      tmpNeed <- paste(retFormat$width[test], collapse=", ")
      stop(paste("'width' (", tmpWidth, ") was too small for columns: ",
                 tmpCol, "\n 'width' should be at least (", tmpNeed, ")",
                 sep=""))
    }
  }

  ## --- Write ---

  if(colnames) {
    if(rownames && is.null(rowCol)) {
      colnamesMy <- colnamesMy[-1]
    }

    write.table(t(as.matrix(colnamesMy)),
                file=file,
                append=append,
                quote=quote,
                sep=sep,
                eol=eol,
                na=na,
                row.names=FALSE,
                col.names=FALSE,
                qmethod=qmethod,
                fileEncoding = encoding)
  }

  write.table(x=as.matrix(x),
              file=file,
              append=(colnames || append),
              quote=quote,
              sep=sep,
              eol=eol,
              na=na,
              row.names=FALSE,
              col.names=FALSE,
              qmethod=qmethod,
              fileEncoding = encoding)

  ## --- Return format and fixed width information ---

  if(formatInfo) {
    ## be carefull with these ifelse constructs
    retFormat$position[1] <- ifelse(quote, ifelse(quoteInfo, 1, 2), 1)
    if(ifelse(quote, quoteInfo, FALSE)) retFormat$width <- retFormat$width + 2
    N <- nrow(retFormat)
    if(N > 1) {
      for(i in 2:N) {
        retFormat$position[i] <- retFormat$position[i - 1] +
          retFormat$width[i - 1] + nchar(x=sep, type="chars") +
          ifelse(quote, ifelse(quoteInfo, 0, 1), 0)
      }
    }
    if(rownames && is.null(rowCol)) {
      retFormat <- retFormat[-1,]
      rownames(retFormat) <- 1:(N-1)
    }
    return(retFormat)
  }
}

