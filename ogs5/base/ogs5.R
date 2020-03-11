
# a constructor for the ogs5 base-class

create_ogs5 <- function(
   #x = list(input=list(), output=list(mod)),
   sim_name = character(NULL),
   sim_id = integer(NULL),
   sim_path = character(NULL)
) {
   
   # validation
   if (!is.character(sim_name)) {
      stop("'sim_name' has to be of type character", call. = FALSE)
   } 
   
   if (!is.character(sim_path)) {
      stop("'sim_path' has to be of type character", call. = FALSE)
   } 
   
   if (!is.integer(sim_id)) {
      stop("'sim_id' has to be of type integer", call. = FALSE)
   } 
   
   # define ogs5-obj
   x <- list(input=list(), output=list())
   
   structure(
      x,
      class = "ogs5",
      sim_name = sim_name,
      sim_id = sim_id,
      sim_path = sim_path
   )
}


# validator for ogs5-base class

valid_ogs5 <- function(x){
   
   if (!class(x)=="ogs5") {
      stop("x is not of class 'ogs5' ", call. = FALSE)
   }
   if (is.null(x$input)) {
     stop("'input' list missing.", call. = FALSE)
   }
   if (is.null(x$output)) {
    stop("'output' list missing.", call. = FALSE)
   }
   
}

# validator for ogs5-obj before execution of run 
# define when all input objects have been defined
# shoul dbe used right before printing og5-obj, or running simulations
