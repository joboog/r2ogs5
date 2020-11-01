
ogs5_read_tecplot <- function(filename = character(),
                              geo_object = character()){

  # content:
  # function to read *.tec files by calling functoin to read *.tec depending
  # on the corresponding geometry
  # filename: path+name of file
  # geo_object: associated geometric domain
  #
  # returns:

  if (geo_object=="domain"){
    df=ogs5_read_tecplot_domain(filename)
  }
  if (geo_object=="POINT"){
    df=ogs5_read_tecplot_point(filename)
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

# Read *.tec file for whole domain ----------------------------------------


#' ogs5_read_tecplot_domain
#'
#' @param filepath path to filename
#'
#' @return
#'
#' @examples
ogs5_read_tecplot_domain<-function(filepath){

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE DOMAINfrom ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file
  # or url of raw file on github/gitlab repository

    # check filepath
  if (file.exists(filepath) | (RCurl::url.exists(filepath))){
  } else if (!(file.exists(filepath)) & (!(RCurl::url.exists(filepath)))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filepath, "domain")==FALSE){
    return(NULL)
  }

  #=== get header =====================================================

  # read first line
  header <- readLines(con=filepath,n=1L)
  # remove pattern
  header <- gsub("VARIABLES  =","",header)
  header<- gsub("\"", "", header)

  # split by ","
  header<-stringr::str_split(header, ",")
  header<-as.vector(header[[1]])
  # remove whitespace
  header<-stringr::str_trim(header)



  #=== get data =======================================================

  #=== get time
  ts<-readLines(con=filepath, n = -1L, ok = TRUE)
  ts<-ts %>%
    stringr::str_extract("ZONE T=\"\\d+\\.\\d+e\\+\\d+") %>%
    na.omit() %>%
    gsub(pattern="ZONE T=\"", replacement="") %>%
    as.numeric()

  #=== get point data
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filepath,
                              " ", escape_double = FALSE, col_names = FALSE,
                              trim_ws = TRUE, skip = 3)))

  # extract all lines with elements in first column that do not match pattern
  df<-df %>%
    dplyr::filter(
      !is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\+\\d+"))|!is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\-\\d+"))
    )
  # remove empty columns
  df<- df[, sapply(df, function(i) !all(is.na(i)))] %>%
    apply(2,as.numeric) %>%
    tibble::as_tibble()

  #=== combine all
  colnames(df)<-header

  # create df with point coordinates and time
  # then add as TIME column
  df<- df %>%
    dplyr::bind_cols(
      expand.grid(X=unique(df$X), Y=unique(df$Y), Z=unique(df$Z), TIME=ts) %>%
        dplyr::select(TIME)
    ) %>%
    dplyr::select(
      X,Y,Z,TIME, dplyr::everything()
    )

  return(df)
}


# Read *.tec file for POLYLINE ----------------------------------------------

#' ogs5_read_tecplot_polyline
#'
#' @param filepath path to the filename
#'
#' @return
#'
#' @examples
ogs5_read_tecplot_polyline <- function(filepath) {

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE POLYLINE from ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file

  # check filepath
  if (!(file.exists(filepath))) {
    stop("'filename' does not exist.", call = FALSE)
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
        na.omit() %>%
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
      !is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\+\\d+")) |
        !is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\-\\d+"))
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
    dplyr::select(TIME, dplyr::everything())

  return(df)
}

#' ogs5_read_tecplot_point
#'
#' @param filepath path to the filename
#'
#' @return
#'
#' @examples
ogs5_read_tecplot_point <- function(filepath){

  # content:
  # reads tecplot file from point source with any kind of components
  # filename: path+name of *.tec file
  # or url of raw file on github/gitlab repository

  if (file.exists(filepath) | (RCurl::url.exists(filepath))){
  } else if (!(file.exists(filepath)) & (!(RCurl::url.exists(filepath)))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (!stringr::str_detect(filepath, "POINT|point")){
    return(NULL)
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

  header <- na.omit(header)
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
#'
#' Function to read in any [*.tec]-files inside a directory. Calls on the
#' functions \code{ogs5_read_tecplot_point}, \code{ogs5_read_tecplot_polyline},
#' \code{ogs5_read_tecplot_domain} depending on the specified *geo_object* type.
#'
#' @param filepath (*character*) path to [*.tec]-files.
#' @param geo_object (*character*) Name for the type of geometric ogs object
#' the tec file is associated with. Can be any of "POINT", "POLYLINE", "
#' SURFACE", "domain".
#'
#' @return *Data.frame* with timeseries of the simulation reasults.
#' @export
#'
#' @examples
ogs5_read_many_tecplots <- function(filepath = character(),
                                    geo_object = character()){

  # content:
  # reads many *.tec files from one filepath
  # you hae to specify the file path and the type of tec file
  # types are: POINT, POLYLINE, SURFACE, DOMAIN
  # returns: dataframe with contents of *.tec files

  # filepath ... path of tec files
  # geo_object... type of geometric ogs object the tec file is associated with
  #               POINT, POLYLINE, SURFACE, domain

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
    function(filename) {

      df <- ogs5_read_tecplot(filename = filename,
                              geo_object = geo_object)
      df$filename=filename
      return(df)
    }
  )
  return(df)
}

