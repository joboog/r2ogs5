# Functions to read and process outputted *.tec files

#' ogs5_read_tecplot
#' @description Wrapper to read ***.tec** file. Calls
#' [ogs5_read_tecplot_domain()], [ogs5_read_tecplot_polyline()],
#' [ogs5_read_tecplot_point()].
#' @param filename *character* Path to ***.tec** file.
#' @param geo_object *character* Associated geometry type: c("domain", "POINT", "POLYLINE",
#'   "SURFACE").
#' @return Dataframe.
#' @export
ogs5_read_tecplot <- function(filename = character(),
                              geo_object = character()){

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


#' ogs5_read_tecplot_domain
#' @description Read data from ***.tec** file of entire model domain.
#' @param filename *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_domain<-function(filename){

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE DOMAINfrom ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file
  # or url of raw file on github/gitlab repository

    # check filepath
  if (file.exists(filename) | (RCurl::url.exists(filename))){
  } else if (!(file.exists(filename)) & (!(RCurl::url.exists(filename)))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filename, "domain")==FALSE){
    return(NULL)
  }

  #=== get header =====================================================

  # read first line
  header <- readLines(con=filename,n=1L)
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
  ts<-readLines(con=filename, n = -1L, ok = TRUE)
  ts<-ts %>%
    stringr::str_extract("ZONE T=\"\\d+\\.\\d+e\\+\\d+") %>%
    na.omit() %>%
    gsub(pattern="ZONE T=\"", replacement="") %>%
    as.numeric()

  #=== get point data
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filename,
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


#' ogs5_read_tecplot_polyline
#' @description Read data from ***.tec** file of a polyline.
#' @param filename *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_polyline <- function(filename) {

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE POLYLINE from ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file

  # check filepath
  if (!(file.exists(filename))) {
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filename, "polyline|ply|LINE") == FALSE) {
    return(NULL)
  }

  #=== get header =====================================================

  # read second line
  header <- readLines(con = filename, n = 2L)[2]
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
  ts <- readLines(con=filename, n = -1L, ok = TRUE) %>%
        stringr::str_squish()

  ts <- ts %>%
        stringr::str_extract("ZONE T=\"TIME=\\d+\\.\\d+e\\+\\d+") %>%
        na.omit() %>%
        gsub(pattern="ZONE T=\"TIME=", replacement="") %>%
        as.numeric()

  #=== get point data
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filename,
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
#' @description Read data from ***.tec** file of a point.
#' @param filename *character* Path to ***.tec** file.
#' @return Dataframe.
ogs5_read_tecplot_point <- function(filename = character()){

  # content:
  # reads tecplot file from point source with any kind of components
  # filename: path+name of *.tec file
  # or url of raw file on github/gitlab repository

  if (file.exists(filename) | (RCurl::url.exists(filename))){
  } else if (!(file.exists(filename)) & (!(RCurl::url.exists(filename)))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filename, "POINT")==FALSE){
    return(NULL)
  }

  # get header first
  suppressWarnings(
    suppressMessages(
      df <- readr::read_delim(filename, " ", escape_double = FALSE, col_names = FALSE,
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
    df <- readr::read_delim(filename, " ", escape_double = FALSE, col_names = FALSE,
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
#' tec_df <- ogs5_read_many_tecplots(filepath = "examples/tmp/ex1",
#'                                   geo_object = "domain")
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

  df=plyr::ldply(
    list.files(
      path=filepath,
      pattern=".tec",
      full.names=TRUE
    )[which(
      stringr::str_detect(
        string=list.files(
          path=filepath,
          pattern=".tec",
          full.names=TRUE
        ),
        pattern=geo_object
      )==TRUE
    )],
    function(filename) {

      df <- ogs5_read_tecplot(filename = filename,
                              geo_object = geo_object)
      df$filename=filename
      return(df)
    }
  )
}
