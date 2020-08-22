# Functions to read and process outputted *.tec files



# generic to ogs5_read_tecplot-- ------------------------------------------
ogs5_read_tecplot <- function(filename = character(),
                              geo_object = character()){

  # content:
  # function to read *.tec files by calling functoin to read *.tec depending
  # on the corresponding geometry
  # filename: path+name of file
  # geo_object: associated geometric domain
  #
  # returns: dataframe of *.tec

  if (geo_object=="domain"){
    df=ogs5_read_tecplot_domain(filename)
  }
  if (geo_object=="POINT"){
    df=myReadTecPlot_POINT(filename)
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

ogs5_read_tecplot_domain<-function(filename){

  # content:
  # this function reads an individual tecplot file (*.tec)
  # of $GEOMETRY_TYPE DOMAINfrom ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file

    # check filepath
  if (!(file.exists(filename))){
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

ogs5_read_tecplot_point <- function(filename = character()){

  # content:
  # reads tecplot file from point source with any kind of components
  # filename: path+name of *.tec file

  if (!(file.exists(filename))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filename, "POINT")==FALSE){
    return(NULL)
  }

  # get header first
  df <- readr::read_delim(filename, " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)

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

  # set column names
  colnames(df) <- header

  return(df)
}

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

