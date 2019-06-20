
# a constructor for the ogs5 base-class
create_ogs5 <- function(
                  x = list(input=list(), output=list(mod)),
                  sim_name = character(NULL),
                  sim_id = integer(NULL),
                  sim_path = character(NULL)
               ) {
   
   stopifnot(is.list(x))
   
   structure(
      x,
      class = "ogs5",
      sim_name = sim_name,
      sim_id = sim_id,
      sim_path = sim_path
   )
}
