ogs5_generate_script <- function(benchmark_dir,
                                 script_path) {

    # function to help creation of scripts for bm files

    # INPUT:
    #   * bm directory^
    #   * script directory
    #   * ogs5_obj name, usally the benchmark name (
        # eg. for C/Engesgaard/slow_kin_pqc it would be "pds")

    # OUTPUT: raw script for one benchmark

    # example:
    # ogs5_generate_script("examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc",
    #                      script_path = "examples/bm1_script.R")

# -------------------------------------------------------------------------

    # check if directory exists
    if (!dir.exists(benchmark_dir)) {
        stop(paste0("directory ", benchmark_dir, " seems not to exist"))
    }

    # get all filenames and extensions in filepath
    all_filenames <- list.files(benchmark_dir) %>%
        stringr::str_split("\\.")

    # check that directory is not empty
    if (length(all_filenames) < 1) {
        stop(paste0("directory ", benchmark_dir, " seems to be empty"))
    }

    script_dir <- script_path %>% stringr::str_remove("/\\w+\\.\\w+")
    if(!dir.exists(script_dir)) {
        dir.create(script_dir, recursive = TRUE)
    }


    all_file_ext <- all_filenames %>%
        lapply(FUN = dplyr::last)
    files <- all_filenames %>%
        lapply(FUN = paste0, collapse = ".")

    # list of files that can be handled by this function
    blocs_no_name <- list("bc", "ic", "num",
                          "out", "pcs",
                          "rei", "rfd", "st", "tim"
                          )

    blocs_with_name <- list("mcp", "mfp", "mmp", "msp"
                            )

    # Assign simple ogs-object name
    ogs5_obj_name <- "ogs5_obj"

    # identation
    ident5 <- "\t\t\t\t\t"

    # write line tp create new ogs5 object
    sink(script_path, append = FALSE)
    cat(paste0(ogs5_obj_name, " <- create_ogs5(sim_name = \"",
                                                    ogs5_obj_name, "\",\n",
                                    ident5, "sim_path = \"",
                                                    script_dir,  "/tmp\",\n",
                                    ident5, "sim_id = 1L",
                                        ")\n\n"))
    sink()

    for (i in seq_len(all_file_ext %>% length())) {



        file_ext <- all_file_ext[[i]]

        # create section labels
        sink(script_path, append = TRUE)
        cat(paste0("# ", file_ext, " "))
        cat(paste0(rep("-", 70), collapse = ""))
        cat(paste0("\n"))
        sink()

        # check if script can be generated automatically
        if (file_ext %in% blocs_with_name | file_ext %in% blocs_no_name) {
            print(paste0("generating functions for ", file_ext))
        } else
            if (file_ext == "msh" | file_ext == "gli"|file_ext == "fct") {

                # enter function call to create ogs5 from file
                sink(script_path, append = TRUE)
                cat(
                    paste0(ogs5_obj_name," <- input_add_blocs_from_file(",
                       ogs5_obj_name, ",\n",
                       ident5, "sim_basename = \"",
                       basename(script_dir), "\",\n",
                       ident5, "filename = \"", files[[i]],
                       "\",\n", ident5, "file_dir = \"", benchmark_dir,
                       "\")\n\n"))
                sink() # sink here and skip the rest!
            next

        } else {
            next  # has to be edited manually
                  # - section label was entered above as a reminder
        }

        # Create ogs5_list for one input file
        ogs5_list <- ogs5_read_inputfile_tolist(paste0(benchmark_dir,
                                                       "/",
                                                       files[[i]]))

        # loop over blocs
        for (b in seq_len(ogs5_list %>% length())) {

            # check if processable
            if (file_ext %in% blocs_no_name) {
                # create bloc name
                bloc_name_string <- paste0(file_ext, "_name = ",
                                           "\"", names(ogs5_list)[b],
                                           "\",\n", ident5)
            } else if (file_ext %in% blocs_with_name){

                # check if there is already a NAME key in the bloc
                if ("NAME" %in% names(ogs5_list[[b]])) {
                    bloc_name_string <- ""
                } else if (file_ext == "mfp"){
                    # exception for mfp
                    bloc_name_string <- paste0("FLUID_NAME = \"",
                                               names(ogs5_list)[b],
                                               "\",\n", ident5)
                } else {
                    bloc_name_string <- paste0("NAME = \"",
                                               names(ogs5_list)[b],
                                               "\",\n", ident5)
                }
            }

            if (file_ext == "rfd" &
                stringr::str_detect(names(ogs5_list)[b], "CURVES")) {

                    tim_vec <- ogs5_list[[b]] %>%
                        stringr::str_split(" ") %>%
                        lapply(dplyr::first) %>%
                        unlist
                    val_vec <- ogs5_list[[b]] %>%
                        stringr::str_split(" ") %>%
                        lapply(dplyr::last) %>%
                        unlist

                    arg_string <-
                        paste0("mkey = \"CURVES\",\n", ident5,
                                "data = tibble::tibble(\n\t\t\t", ident5,
                               "time = c(", paste0(tim_vec,
                                collapse = paste0(",\n", ident5, ident5
                                                  )),
                                "),\n\t\t\t", ident5,
                               "value = c(", paste0(val_vec,
                                collapse = paste0(",\n", ident5, ident5
                                                  )),
                                         "))")
                } else {
                arg_string <- ogs5_generate_arg_string(ogs5_list[[b]],
                                                       file_ext)
                }
            sink(script_path, append = TRUE)
            cat(paste0(ogs5_obj_name, " <- input_add_", file_ext, "_bloc(",
                       ogs5_obj_name, ", ",
                       bloc_name_string,
                       arg_string, "\n)\n"))
            sink()


        }
    }
    sink(script_path, append = TRUE)
    cat(paste0("# write input files "))
    cat(paste0(rep("-", 70), collapse = ""))
    cat(paste0("\n"))
    cat(paste0("ogs5_write_inputfiles(", ogs5_obj_name, ",\"all\")\n"))
    cat(paste0("# execute ogs "))
    cat(paste0(rep("-", 70), collapse = ""))
    cat(paste0("\n"))
    cat(paste0("ogs5_run(",
               ogs5_obj_name, ", ogs_exe = \"../r2ogs/inst/ogs/ogs_5.76\",\n",
               ident5, "run_path = NULL,\n",
               ident5, "log_output = TRUE,\n",
               ident5, "log_path = \"", paste0(script_dir, "/log"), "\")\n"))
    sink()

}


ogs5_generate_arg_string <- function(sub_list, file_ext) {

    # INPUT: ogs5 list, file type
    # OUTPUT: string that can be evaluated as function call
    arg_string <- NULL
    for (i in seq_len(sub_list %>% length())) {
        arg_name <- names(sub_list)[i]
        if (is.null(arg_name)) {
            arg_name <- "null"
        }
        # check if arg_name is a valid Skey
        if (!arg_name %in% ogs5_get_keywordlist()[[paste(file_ext)]]$skey) {
            arg_name <- paste0("# ", arg_name, " = ! is not a valid skey !")
        } else {
            arg_name <- paste0(arg_name, " = ")
        }

        arg <- paste0("\"",
                      paste0(sub_list[[i]] %>% unlist(), collapse = " "),
                      "\"")

        arg_string <- c(arg_string, paste0(arg_name, arg))

    }
    return(paste0(arg_string, collapse = ",\n\t\t\t\t\t"))
}
