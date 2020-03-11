# definitions for the ensemble-class ----------------------------------

# this class is used to gather individual ogs objects and provides wrapper
# functions to define, execute and process mulitple simulations.
# For instance, if runs with parameter sets are desired, this class provides
# functoins to defined ogs objects with the desired parameters values.

create_ens <- function(
    base_sim = list(),
    parameter_tbl = tbl(),
    name = character(NULL),
    path = character(NULL)
) {

    # validation
    valid_ogs5(base_sim)

    if (!is.character(name)) {
        stop("'name' has to be of type character", call. = FALSE)
    }

    if (!is.character(path)) {
        stop("'path' has to be of type character", call. = FALSE)
    }

    # valid sim_plan if it contains a parameter that is contained in the
    # ogs5 object
    # ex:
    # parameter_tree    parameter_name      parameter_value
    # ...$input$MMP$gravel$MASS_DISPERSION  MASS_DISPERSION   1.0 0.1
    # ...$input$MMP$gravel$MASS_DISPERSION  MASS_DISPERSION   1.5 0.15
    # if (!(names(parameter_tbl) %in% c("parameter_tree", "parameter_name",
    #                                 "parameter_value"))){
    #     stop("names of 'parameter_tbl' are not following 'parameter_tree',
    #          'parameter_name', 'parameter_value'", call. = FALSE)
    # }
    # find validator functoin to check if the parameter exist in the base_sim

    # create sim_plan
    sim_plan <- parameter_tbl %>%
                add_column(sim_name = paste0(name,
                                             1:dim(.)[1]))

    # remove any output contained in base_sim
    base_sim$output <- list(NULL)

    # define ens-obj
    x <- list()

    structure(
        x,
        class = "ens",
        base_sim = base_sim,
        sim_plan = sim_plan,
        name = name,
        path = paste0(path, "/", name)
    )
}

# validator for ens class

valid_ens <- function(x){

    if (!class(x)=="ens") {
        stop("x is not of class 'ens' ", call. = FALSE)
    }
}

