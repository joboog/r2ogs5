# Functions to read and process outputted *.tec files

#' Check if url is available
#' @description Base R solution to check url to avoid dependency RCurl
#' @param url_in An url or file path
#'
#' @return *logical* if url can be accessed
#'
#' @examples
valid_url <- function(url_in) {
  if (stringr::str_starts(url_in, "https://|http://")) {
  con <- url(url_in)
  check <- suppressWarnings(try(
    open.connection(con, open="rt",timeout=2),silent=T)[1])
  suppressWarnings(try(
    close.connection(con),silent=T))

  return(ifelse(is.null(check),TRUE,FALSE))
  } else {
    return(FALSE)
  }
}

#' ogs5_read_tecplot
#' @description Wrapper to read ***.tec** file. Calls
#' [ogs5_read_tecplot_domain()], [ogs5_read_tecplot_polyline()],
#' [ogs5_read_tecplot_point()].
#' @param filepath *character* Path to ***.tec** file.
#' @param geo_object *character* Associated geometry type: c("domain", "POINT", "POLYLINE",
#'   "SURFACE").
#' @return Dataframe.
#' @export
ogs5_read_tecplot <- function(filepath = character(),
                              geo_object = character()){

  if (geo_object=="domain"){
    df=ogs5_read_tecplot_domain(filepath)
  }
  if (geo_object=="POINT"){
    df=ogs5_read_tecplot_point(filepath)
  }
  if (geo_object=="POLYLINE"){
    stop("Function to read output on POLYLINE not implemented yet.",
         call. = FALSE)
  }
  if (geo_object=="SURFACE"){
    stop("Function to read output on POLYLINE not implemented yet.",
         call. = FALSE)
  }

  return(df)
}


#' ogs5_read_tecplot_domain
#' @description Read data from ***.tec** file of entire model domain.
#' @param filepath *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_domain<-function(filepath){

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE DOMAINfrom ogs5 simulations and returns a dataframe
  # filepath: path + name of the *.tec file
  # or url of raw file on github/gitlab repository

    # check filepath
  if (file.exists(filepath) | (valid_url(filepath))){
  } else if (!(file.exists(filepath)) & (!(valid_url(filepath)))){
    stop("'filepath' does not exist.", call = FALSE)
  }

  #=== get header =====================================================

  # read first two lines
  header <- scan(
    file = filepath,
    n = 2,
    what = "character",
    blank.lines.skip = TRUE,
    sep = "\n",
    quiet = TRUE
  )
  header <- header[stringr::str_which(header, "VARIABLES")]
  # remove pattern
  header<- gsub("\"", "", header)
  header <- stringr::str_remove(header, "VARIABLES ")
  header <- stringr::str_remove(header, "=")

  # split by " " or ","
  if (stringr::str_detect(header, ",")) {
    sep <- ","
  } else {
    sep <- " "
  }
  header <- stringr::str_split(header, sep) %>%
    unlist %>%
    stringr::str_subset("[:alpha:]+") # make sure only words are kept
  header <- stringr::str_trim(header) # remove whitespace

  #=== get data =======================================================

  data <- scan(
    file = filepath,
    n = -1L,
    what = "character",
    blank.lines.skip = TRUE,
    sep = "\n",
    quiet = TRUE
  )

  #=== get time

  t_ind <- stringr::str_which(data, "ZONE T=")
  v_ind <- stringr::str_which(data, "VARIABLES")
  ts <- data[t_ind]
  ts <- ts %>%
    stringr::str_extract("\\d+\\.\\d+e\\+\\d+|\\d+\\.\\d+e\\-\\d+") %>%
    as.numeric

  #=== get point data

  df <- data %>%
    stringr::str_split(" ")

  # split data where non numbers are encountered
  data_ind <- df %>%
    sapply(function(x) x[[1]]) %>%
    stringr::str_which("\\d+\\.\\d+e\\+\\d+|\\d+\\.\\d+e\\-\\d+")

  df <- df[data_ind] %>%
    sapply(stringr::str_subset,
           pattern = "\\d+\\.\\d+e\\+\\d+|\\d+\\.\\d+e\\-\\d+") %>%
    t %>%
    as.data.frame %>%
    apply(2, as.numeric)



  splt <- which(diff(data_ind) > 1) # last numbers of each time step

  strt <- data_ind[1]
  # expand time vector
  tv <- NULL
  for (i in 1:(length(ts) - 1)) {
    tv <- c(tv, rep(ts[i], (data_ind[splt[i]] - strt) + 1))
    strt <- data_ind[splt[i] + 1]
  }
  tv <- c(tv, rep(ts[i + 1],  (utils::tail(data_ind, 1) - strt) + 1))

  #=== combine all
  colnames(df)<-header

  df <- dplyr::bind_cols(TIME = tv, df)

  return(df)
}


#' ogs5_read_tecplot_polyline
#' @description Read data from ***.tec** file of a polyline.
#' @param filepath *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_polyline <- function(filepath) {

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE POLYLINE from ogs5 simulations and returns a dataframe
  # filepath: path + name of the *.tec file

  # check filepath
  if (!(file.exists(filepath))) {
    stop("'filepath' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filepath, "polyline|ply|LINE") == FALSE) {
    return(NULL)
  }

  #=== get header =====================================================

  # read second line
  header <- readLines(con = filepath, n = 2L)[2]
  # remove pattern
  header <- gsub("VARIABLES =", "", header)
  header <- gsub("\"", "", header)

  # split by ","
  header <- header %>%
            stringr::str_squish() %>%
            stringr::str_split(" ") %>%
            unlist()

  #=== get data =======================================================

  #=== get time
  ts <- readLines(con=filepath, n = -1L, ok = TRUE) %>%
        stringr::str_squish()

  ts <- ts %>%
        stringr::str_extract("ZONE T=\"TIME=\\d+\\.\\d+e\\+\\d+") %>%
        stats::na.omit() %>%
        gsub(pattern="ZONE T=\"TIME=", replacement="") %>%
        as.numeric()

  #=== get point data
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filepath,
                              " ", escape_double = FALSE, col_names = FALSE,
                              trim_ws = TRUE, skip = 3)))

  # extract all lines with elements in first column that do not match pattern
  df <- df %>%
    dplyr::filter(
      !is.na(stringr::str_extract(.data$X1,"\\d+\\.\\d+e\\+\\d+")) |
        !is.na(stringr::str_extract(.data$X1,"\\d+\\.\\d+e\\-\\d+"))
    )
  # remove empty columns
  df <- df[, sapply(df, function(i) !all(is.na(i)))] %>%
    apply(2,as.numeric) %>%
    tibble::as_tibble()

  #=== combine all
  colnames(df) <- header

  # create df with point coordinates and time
  # then add as TIME column
  df <- df %>%
    dplyr::bind_cols(TIME = rep(ts, rep(nrow(df)/length(ts), length(ts)))) %>%
    dplyr::select(.data$TIME, dplyr::everything())

  return(df)
}


#' ogs5_read_tecplot_point
#' @description Read data from ***.tec** file of a point.
#' @param filepath *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_point <- function(filepath){

  # content:
  # reads tecplot file from point source with any kind of components
  # filepath: path+name of *.tec file
  # or url of raw file on github/gitlab repository

  if (file.exists(filepath) | (valid_url(filepath))){
  } else if (!(file.exists(filepath)) & (!(valid_url(filepath)))){
    stop("'filepath' does not exist.", call = FALSE)
  }

  # get header first
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filepath, " ", escape_double = FALSE, col_names = FALSE,
                              trim_ws = TRUE, skip = 1)))

  header <- dplyr::tbl_df(df) %>%
    dplyr::slice(1) %>%
    c(., recursive=TRUE) %>%
    unname

  header <- stats::na.omit(header)
  header <- header[-(1:2)]
  rm(df)

  # read data
  suppressWarnings(
    suppressMessages(
    df <- readr::read_delim(filepath, " ", escape_double = FALSE, col_names = FALSE,
                     trim_ws = TRUE, skip = 3)))
  df <- as.data.frame(df)
  df <- df[, !sapply(df, is.character)]

  # exclude the empty column
  has_data <- function(x) { !all(is.na(x)) }
  df <- df %>% dplyr::select_if(has_data)

  # set column names
  colnames(df) <- header

  return(df)
}


#' ogs5_read_many_tecplots
#' @description Reads many *.tec files from one filepath. Wrapper for
#' [ogs5_read_tecplot()].
#' @param filepath *character* Path to ***.tec** files.
#' @param geo_object *character* Associated geometry type: c("domain", "POINT", "POLYLINE",
#'   "SURFACE").
#' @return Dataframe.
#' @export
#' @examples
#' \dontrun{
#' tec_df <- ogs5_read_many_tecplots(filepath = "examples/tmp/ex1",
#'                                   geo_object = "domain")
#' }
ogs5_read_many_tecplots <- function(filepath = character(),
                                    geo_object = character()){

  # check filepath
  if (!(dir.exists(filepath))){
    stop("'filepath' does not exist.", call = FALSE)
  }

  if (!(geo_object %in% c("domain", "POINT",
                          "POLYLINE", "SURFACE"))){
    stop(paste0("'geo_object' restricted to 'domain', 'POINT',",
            " 'POLYLINE' and 'SURFACE'."), call. = FALSE)
  }

  df <- plyr::ldply(
      list.files(
        path=filepath,
        pattern=".tec",
        full.names=TRUE
    )[stringr::str_which(
        string=list.files(
          path=filepath,
          pattern=".tec",
          full.names=TRUE
        ),
        pattern=paste0(tolower(geo_object), "|", geo_object)
      )
      ],
    function(filepath) {

      df <- ogs5_read_tecplot(filepath = filepath,
                              geo_object = geo_object)
      df$filepath=filepath
      return(df)
    }
  )
  return(df)
}
