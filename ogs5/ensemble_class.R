# definitions for the ensemble-class ----------------------------------

# this class is used to gather individual ogs objects and provides wrapper
# functions to define, execute and process mulitple simulations.
# For instance, if runs with parameter sets are desired, this class provides
# functoins to defined ogs objects with the desired parameters values.

create_ens <- function(
    base_sim = list(),
    sim_plan = tibble(),
    ens_name = character(NULL),
    ens_path = character(NULL)
) {

    # validation
    if (!valid_ogs5(base_sim)) {
        stop("'base_sim' has to be a valid ogs5-object", call. = FALSE)
    }

    if (!is.character(ens_name)) {
        stop("'ens_name' has to be of type character", call. = FALSE)
    }

    if (!is.character(ens_name_path)) {
        stop("'ens_name' has to be of type character", call. = FALSE)
    }

    # valid sim_plan if it contains a parameter that is contained in the
    # ogs5 object

    # define ens-obj
    x <- list()

    structure(
        x,
        class = "ens",
        base_sim = base_sim,
        sim_plan = sim_plan,
        ens_name = ens_name,
        ens_path = ens_path
    )
}

# validator for ens class

valid_ens <- function(x){

    if (!class(x)=="ens") {
        stop("x is not of class 'ens' ", call. = FALSE)
    }
}
