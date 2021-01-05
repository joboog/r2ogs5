# definitions for the ensemble-class ----------------------------------

#

#' create_ens
#' @description Constructor for class *ens*. Takes an instance of class *ogs5*
#'   as basis to create a collection of *ogs5* instances to be used in ensemble
#'   simulations. Class *ens* provides wrapper functions to define, execute and
#'   process multiple simulations.
#' @param base_sim Instance of class *ogs5* to be used as basis. *ogs5*
#' @param parameter_tbl *tibble()*, *tbl()* or *data.frame()* defining the parameters and values for ensemble
#'   runs.
#' @param name *character* Name for the ensemble.
#' @param path *character* Path where to place all individual ensemble runs.
#'
#' @return Instance of class *ens*
#' @export
#'
#' @examples
#' para_df <- expand.grid(mass_dispersion = seq(0.1, 2.5, length.out = 10),
#'               tracer_input_concentration = seq(0.1, 1, length.out = 5)) %>%
#'               tibble::as_tibble()
#'
#' ens1 <- create_ens(base_sim = ex1, parameter_tbl = para_df,
#'                   name = "tracersim", path = "path/to/my/ens")
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
                tibble::add_column(sim_name = paste0(name,
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


#' valid_ens
#' @description Validates instance of class *ens*.
#' @param x Instance of class *ens*.
valid_ens <- function(x){

    if (!class(x)=="ens") {
        stop("x is not of class 'ens' ", call. = FALSE)
    }
}
