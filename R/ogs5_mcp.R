# validates list for mcp and mcp_component class

create_ogs5_mcp <- function(){
   
   list() %>% structure(class = "ogs5_mcp")
   
}

valid_ogs5_mcp <- function(x){
   
   if (!class(x)=="ogs5_mcp"){
      stop(paste(x, " is not of class 'ogs5_mcp' "), call. = FALSE)
   }
   
}

valid_ogs5_mcp_component <- function(x){
   
   if (!class(x)=="ogs5_mcp_component"){
      stop(paste(x, " is not of class 'ogs5_mcp_component' "), call. = FALSE)
   }
   
}
