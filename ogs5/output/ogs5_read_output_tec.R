# Functions to read and process outputted *.tec files


# Read *.tec file for whole domain ----------------------------------------

ogs5_read_tecplot_domain<-function(filename = character()){
  
  # content:
  # this function reads an individual tecplot file (*.tec) 
  # of $GEOMETRY_TYPE DOMAINfrom ogs5 simulations and returns a dataframe
  # filename: path + name of the *.tec file
  
  require(magrittr)
  
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
  ts<-readLines(con=filename,n=-1L)
  ts<-ts %>% 
    stringr::str_extract("ZONE T=\"\\d+\\.\\d+e\\+\\d+") %>% 
    na.omit() %>% 
    gsub(pattern="ZONE T=\"", replacement="") %>% 
    as.numeric()
  
  #=== get point data
  df <- readr::read_delim(filename,
                          " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 3)
  # extract all lines with elements in first column that do not match pattern 
  df<-df %>% 
    dplyr::filter(
      !is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\+\\d+"))|!is.na(stringr::str_extract(X1,"\\d+\\.\\d+e\\-\\d+"))
    ) 
  # remove empty columns
  df<- df[, sapply(df, function(i) !all(is.na(i)))] %>% 
    apply(2,as.numeric) %>% 
    dplyr::tbl_df()
  
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
  
  require(magrittr)
  
  if (!(file.exists(filename))){
    stop("'filename' does not exist.", call = FALSE)
  }
  if (stringr::str_detect(filename, "POINT")==FALSE){
    return(NULL)
  }
  
  # get header first
  require(readr)
  df <- read_delim(filename, " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
  
  header <- dplyr::tbl_df(df) %>%
    dplyr::slice(1) %>% 
    c(., recursive=TRUE) %>%
    unname
  
  header <- na.omit(header)
  header <- header[-(1:2)]
  rm(df)
  
  # read data
  df <- read_delim(filename, " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 3)
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
      df$filename=filename
      return(df)
    }
  )
}

