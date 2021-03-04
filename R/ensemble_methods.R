# support functions to create ens object ----------------------------------

#' ens_add_ogs5
#' @description Adds an *ogs5* instance to an existing *ens* instance.
#' @param x Instanc of class *ens*.
#' @param ogs5_obj Instance of class *ogs5* to be added to *x*.
#'
#' @return Extended instance *x*.
#' @export
ens_add_ogs5 <- function(x, ogs5_obj){

    # validate
    valid_ens(x)
    valid_ogs5(ogs5_obj)

    # validate if ogs5_obj is derived from attributes(x)$base_im
    if (attributes(ogs5_obj)$sim_name != attributes(attributes(x)$base_sim)$sim_name){
        stop("The ogs5 object provided is not derived from the base_sim of ",
             x, call. = FALSE)
    }
    if (attributes(ogs5_obj)$sim_path != attributes(attributes(x)$base_sim)$sim_path){
        stop("The ogs5 object provided is not derived from the base_sim of ",
             x, call. = FALSE)
    }

    # add ogs5_obj to x
    i <- length(x) + 1
    attributes(ogs5_obj)$sim_name <- paste0(attributes(x)$name, i)
    attributes(ogs5_obj)$sim_path <- paste0(attributes(x)$path, "/",
                                       attributes(ogs5_obj)$sim_name)
    x[[paste0("sim", i)]] <- ogs5_obj

    return(x)
}

#' ens_write_inputfiles
#' @description Wrapper function to write input files for all *ogs5* simulation
#'   objects belonging to an *ens* instance.
#' @param x Instanc of class *ens* containing the simulation objects.
#' @param type *character* Type of input files to write, see [ogs5_write_inputfiles()].
#' @export
ens_write_inputfiles <- function(x, type = "all"){

    valid_ens(x)

    for (i in 1:length(x)){
        ogs5_write_inputfiles(x[[i]], type)
    }
}

#' ens_run
#' @description Wrapper to execute [ogs5_run()] for all included *ogs5* objects
#'   included.
#' @param x  Instanc of class *ens* containing the simulation objects.
#' @param ogs_exe *character* Path to ogs5 executable.
#' @param log_output *logical* Write log output?
#' @param wait *logical* If *TRUE* R starts the individual simulations runs sequentially,
#'   to say, it waits until a run finishes before starting the next. If *FALSE*
#'   all runs will be started the same time.
#' @export
#' @examples
#' \dontrun{
#' ens_run(ens1,
#'         ogs_exe = "../inst/ogs/ogs_5.76",
#'         log_output = TRUE,
#'         wait = FALSE)
#' }
ens_run <- function(x, ogs_exe, log_output = TRUE , wait = FALSE){


    valid_ens(x)

    for (i in 1:length(x)){
        ogs5_run(x[[i]],
                 ogs_exe = ogs_exe,
                 run_path = NULL,
                 log_output = log_output,
                 wait = wait)
    }
}

#' ens_get_output
#' @description Wrapper to retrieve output for all included *ogs5* simulation
#'   objects. It calls on the functions [ogs5_get_output_all()] and
#'   [ogs5_get_output_specific()] of the **r2ogs** package.
#' @param x  Instanc of class *ens* containing the simulation objects.
#' @param type *character* If 'all' retrieve all produced output, if 'specific'
#'   retrieve output stated in a certain *OUT* bloc of the *ogs5* simulation
#'   objects.
#' @param ... *list(character)* If *type* = 'specific', state names  of the *OUT*
#'   blocs you wish to retrieve output for. Arguments will be passed to
#'   ' ogs5_get_output_specific()'.
#'
#' @return Extended *x* with added output.
#' @export
#' @examples
#' \dontrun{
#' ens1 <- ens_get_output(ens1, type = "all")
#' #' ens1 <- ens_get_output(ens1,
#'                           type = "specific",
#'                           outbloc_names = c("OUTPUT1", "OUTPUT2))
#' }
ens_get_output <- function(x, type = "all", ...){

    valid_ens(x)

    for (i in 1:length(x)){
        x[[i]]$output <- list(NULL)

        x[[i]] <-
            switch(type,
                   "all" = ogs5_get_output_all(x[[i]]),
                   "specific" = ogs5_get_output_specific(x[[i]], ...))
    }
    return(x)
}

# unfinished  -------------------------------------------------------------
ens_def_ogs5sims_from_simplan <- function(x = list()){

    valid_ens(x)

    # define ogs5 objects
    sim_plan_df <- attributes(x)$sim_plan
    base_sim <- attributes(x)$base_sim

    ll <- list(rep(base_sim, times = dim(sim_plan_df)[1]))
    # change names

    for (i in 1:length(ll)){
        attributes(ll[i])$sim_name = sim_plan_df$sim_name[i]

        # add par value
        ll[i]

    }

    # change parameters

}


update_parameter <- function(ogs5, parameter_tree, parameter_value){
    # think about function that takes parameter values from sim_plan
    # and creates new ogs5-obj from it
    #
    # pars =stringr::str_split((parameter_tree, "$"))
    # loop through pars
}

# maybe just use a simple nested loop to go over list elements in step in sublist if names is in character vector

# loop through ogs5-obj list and find list tree to parameter
#
# ff <- function(x){
#     if (class(x)=="list"){
#         if (quote(x) %>%
#          as.character() %>%
#         stringr::str_c(collapse = "") %>%
#         stringr::str_detect("x") == TRUE){
#             x<-1
#             lapply(x, ff)
#         } else {lapply(x,ff)}
# }}

# ll1 <- list(x = list(par1 = 1, par2 = 44), y = list(xx=33, xd=34) )
# ll2 <- list(x = list(par1 = 12, par2 = 44), y = list(xx=66, xd=34) )

# compare actual ogs5-obj to base_sim
# compare elements in two unlisted lists and
# convert to tbl

compare_list_tree <- function(ogs1 = list(), ogs2 = list()){

    # validate
    #valid_ogs5(ogs1)
    #valid_ogs5(ogs2)

    # compare
    df <- unlist(ogs2)[!(unlist(ogs2) %in% unlist(ogs1))] #%>%
            #as.list() %>%
            #tibble::as_tibble()
    return(df)
}

# ex2 <- ex1
# ex2$input$mmp$gravel$MASS_DISPERSION <- "3 0.3"
# ex3 <- ex1
# ex3$input$mcp$Tracer$MOBILE <- "0"
# there was a problem with comparing ex21 and ex3 --> return character(0)
