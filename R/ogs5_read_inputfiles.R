
ogs5_read_inputfile_tolist <- function(filepath){

    # Basis function to store everything in lists.
    # reads:
    # bc, pcs, ic, mcp, mfp, mmp, msh, msp, num, out, tim

    # get character vector of file
    chr <- scan(
            file = filepath,
            what = "character",
            blank.lines.skip = TRUE,
            sep = "\n",
            quiet = TRUE
            # specify encoding??
            )

    # remove comments // and ;
    chr <-  chr %>%
        stringr::str_remove("//.+|\\;.+") %>%
        stringr::str_squish()

    # iterate over character vector
    l <- list()
    i <-1
    mkey <- 1
    skey <- 0
    while (!(stringr::str_detect(chr[i], "#STOP"))) {

        # identify main keys  and sub keys
        is_mkey <- stringr::str_starts(chr[i], "#")
        is_skey <- stringr::str_starts(chr[i], "\\$")

        # create list if mkey found
        if (is_mkey) {
            tmp1 <- chr[i] %>%
                    stringr::str_extract_all("\\w+") %>%
                    stringr::str_c(mkey)
            l[[paste0(tmp1)]] <- list()
            mkey <- mkey + 1
            skey <- 0 # set back to 0
            i <- i + 1
        } else {
            # create sublist if skey found
            if (is_skey) {
                tmp2 <- chr[i] %>% stringr::str_extract_all("\\w+")
                l[[paste0(tmp1)]][[paste0(tmp2)]] <- list()
                i <- i + 1
                skey <- skey + 1
            } else {
                # add entry to list
                j <- 1
                while(!any(c(is_skey, is_mkey))) {
                    tmp3 <- chr[i] %>%
                            stringr::str_squish()

                    if (chr[i] != "" & skey != 0){
                        l[[paste0(tmp1)]][[skey]][j] <- tmp3
                        j <- j + 1
                    } else { # if skey is 0
                        if (chr[i] != "") {
                            l[[paste0(tmp1)]][j] <- tmp3
                            j <- j + 1
                        }
                    }
                    i <- i + 1 # next line
                    is_mkey <- stringr::str_starts(chr[i], "#")
                    is_skey <- stringr::str_starts(chr[i], "\\$")
                }
            }
        }
    }
    return(l)
}


ogs5_read_pqc_input_tolist <- function(filepath) {

    # same function as before, for *.pqc and phreeqc.dat files

    # get character vector of file
    chr <- scan(
        file = filepath,
        what = "character",
        blank.lines.skip = TRUE,
        sep = "\n",
        quiet = TRUE
        # specify encoding??
    ) %>% stringr::str_squish()

    chr <- chr[which(chr != "#")] # remove '#' lines

    pqc_mkeys <- ogs5_get_keywordlist()$pqc$mkey[
                    which(!ogs5_get_keywordlist()$pqc$mkey == "ende")
                    ]
    l <- list()
    i <- 1
    while (!stringr::str_detect(chr[[i]], "END")) {

        # check if mkey
        if (chr[[i]] %in% pqc_mkeys) {
            mkey_name <- chr[[i]]
            l[[paste0(mkey_name)]] <- list()
            # step to next line
            i <- i + 1

        } else {                               # add subkeys
            j <- 1                             # subkey list index
            while(!chr[[i]] %in% pqc_mkeys) {

                l[[paste0(mkey_name)]][j] <- chr[[i]]
                j <- j + 1
                i <- i + 1
            }
        }
    }
    return(l)
}

ogs5_add_input_bloc_from_addfile <- function(ogs5_obj,
                                             filepath,
                                             overwrite){

    valid_ogs5(ogs5_obj)

    # look if ogs5-obj$input$additinoal exists and valid, otherwise create
    if (!("additional" %in% names(ogs5_obj$input))) {

        ogs5_obj$input$additional <- create_ogs5_additional()

    } else {

        valid_ogs5_additional(ogs5_obj$input$additional)
    }

    # check if ogs5-obj$input$additinoal subbloc exists and may skip reading
    existing_blocs <- names(ogs5_obj$input$additional)
    basefilename = basename(filepath)

    if ((basefilename %in% existing_blocs) & !overwrite) return(ogs5_obj)


    # add bloc
    if (basefilename == "phreeqc.dat"){

        new_bloc <- ogs5_read_pqc_input_tolist(filepath) %>%
                        lapply(function(x) {
                            x <- structure(x, class = "ogs5_pqc_skeybloc")
                            return(x)}) %>%
                        structure(class = "ogs5_pqc_filebloc")

    } else {

        new_bloc <- try(scan(
                        file = filepath,
                        what = "character",
                        blank.lines.skip = TRUE,
                        sep = "\n",
                        quiet = TRUE
                        # specify encoding??
                        ) %>%
                        stringr::str_replace_all("\\t", "    "))
    }


    ogs5_obj$input$additional[[paste0(basefilename)]] <- new_bloc

    return(ogs5_obj)
}


add_standard_blocs <- function(filepath,
                               sub_bloc_class = NULL,
                               bloc_class = NULL,
                               stack_args = FALSE) {

    file_ext <- filepath %>%
                stringr::str_split("\\.") %>%
                unlist() %>%
                dplyr::last()

     if (is.null(bloc_class)) {
        bloc_class <- paste0("ogs5_", file_ext)
     }
    if (is.null(sub_bloc_class)) {
        sub_bloc_class <- paste0("ogs5_", file_ext, "_bloc")
    }

    blocs <- ogs5_read_inputfile_tolist(filepath) %>%
            lapply(function(bloc) {

                bloc <- bloc %>%
                        lapply(function(sub_bloc, stack_args) {

                            # unlist
                            sub_bloc <- unlist(sub_bloc)
                            # stack skey arguments (insert \n) if required
                            if (length(sub_bloc) > 1 & stack_args) {
                                sub_bloc <- paste0(sub_bloc, collapse = "\n ")
                            }
                            return(sub_bloc)
                        },
                        stack_args)

                bloc <- structure(bloc, class = sub_bloc_class)
                return(bloc)
                }) %>%
            structure(class = bloc_class)
            return(blocs)
}



ogs5_add_input_bloc_from_ogs5list <- function(ogs5_obj,
                                              filepath,
                                              file_ext,
                                              overwrite) {

    # The function calls ogs5_read_inputfile_tolist or
    # ogs5_read_pqc_input_tolist and assings the correct classes and
    # data formats depending on the file_ext to sublists (main keys) and adds
    # ogs5 blocs to an ogs5 object


    valid_ogs5(ogs5_obj)

    existing_blocs <- ogs5_obj$input[[paste0(file_ext)]]



    # add the list of blocs in the right format & class from ogs5_list
    new_blocs <-

        switch(file_ext,

               "bc" = add_standard_blocs(filepath,
                                         "ogs5_bc_condition"),

# .gli input file ---------------------------------------------------------
               "gli" =                              # loop over bloc names!
                names(ogs5_list <- ogs5_read_inputfile_tolist(filepath)) %>%
                lapply(function(blc_name) {

                    bloc <- ogs5_list[[paste0(blc_name)]]

                    # POINTS: convert into tibble
                    if (stringr::str_detect(blc_name, "POINTS")) {

                     bloc <- bloc %>%
                        stringr::str_split(" ")
                     # check for missing names
                     bloc <- bloc %>%
                         lapply(function(subbloc) {

                             if (!any(subbloc == "$NAME")) {
                                 subbloc <- c(subbloc, "$NAME", "")
                             }
                             # make sure only rownames, xyz and names are there
                             subbloc <- c(subbloc[1:4], tail(subbloc, 2))
                             return(subbloc)
                         })
                     # pack into tibble
                suppressWarnings(
                 suppressMessages(
                     bloc <- bloc %>%
                     unlist %>%
                     matrix(nrow = length(bloc), byrow = TRUE) %>%
                     tibble::as_tibble(.name_repair = "unique") %>%
                     dplyr::rename_at( # one after $NAME
                         .vars = which(stringr::str_detect(., "\\$")) + 1,
                         .funs = ~"name") %>%
                     dplyr::select_if(!stringr::str_detect(., "\\$")) %>%
                     dplyr::rename_at(2:4, ~c("x", "y", "z")) %>%
                     dplyr::mutate_at(.vars = c("x", "y", "z"),
                                      .funs = as.double) %>%
                     tibble::as_tibble() %>%
                     tibble::column_to_rownames("...1") %>%
                     dplyr::select(x, y, z, name)
                            ))

                    } else {

                    #  unlist all other blocs (POLYLINE, SURFACE) & add class
                    # POLYLINE
                    if (stringr::str_detect(blc_name, "POLYLINE")) {
                        bloc <- structure(bloc, class = "ogs5_gli_polyline")

                    # SURFACE
                    } else if (stringr::str_detect(blc_name, "SURFACE")) {
                        bloc <- structure(bloc, class = "ogs5_gli_surfaces")
                    }
                       }
                       return(bloc) # return bloc
                }) %>%
                'names<-' (c(names(ogs5_list))) %>% # restore bloc names
                structure(class = "ogs5_gli"),      # add input class

# .fct input file ---------------------------------------------------------
                "fct" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(bloc) {
                    bloc[["DATA"]] <- bloc[["DATA"]] %>%
                        lapply(stringr::str_split, " ") %>%
                        unlist %>%
                        as.double %>%
                        matrix(nrow = length(bloc[["DATA"]]), byrow = TRUE) %>%
                        'colnames<-' (c("x", "y")) %>%
                        tibble::as_tibble()

                    other_skeys <- which(names(bloc) != "DATA")
                    bloc[other_skeys] <- bloc[other_skeys] %>%
                        lapply(unlist)
                    bloc <- structure(bloc, class = "ogs5_fct_bloc")

                }) %>% structure(class = "ogs5_fct"),

# -------------------------------------------------------------------------
                "ic" = add_standard_blocs(filepath,
                                          "ogs5_ic_condition"),

                "mcp" = add_standard_blocs(filepath,
                                           "ogs5_mcp_component"),

                "mfp" = add_standard_blocs(filepath),

                "mmp" = add_standard_blocs(filepath),

# .msh input file ---------------------------------------------------------
               "msh" =                                   # loop over blocs
                ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(bloc) {

                # Convert NODES into tibble of double
                nds <- bloc[["NODES"]][-1] # leave out first line
                bloc[["NODES"]] <- nds %>%
                                   lapply(stringr::str_split, " ") %>%
                                   unlist %>%
                                   as.double %>%
                                   matrix(nrow = length(nds),
                                          byrow = TRUE) %>%
                                   'colnames<-' (c("n", "x", "y", "z")) %>%
                                   tibble::as_tibble() %>%
                                   dplyr::select(-n) # remove numbering column

                # Convert ELEMENTS into tibble
                emts <- bloc[["ELEMENTS"]][-1] # leave out first line
                mat <-  emts %>%
                        lapply(stringr::str_split, " ") %>%
                        unlist %>%
                        matrix(nrow = length(emts), byrow = TRUE)
                colnames(mat) <- c("n",
                                   "material_id",
                                   "ele_type",
                                   paste0("node", 1:(ncol(mat) - 3)))
                bloc[["ELEMENTS"]] <-
                        mat %>%
                        tibble::as_tibble() %>%
                        dplyr::select(-n) %>%
                        dplyr::mutate_at(dplyr::vars(-ele_type),
                                              list(as.double))

                # unlist all other blocs
                other_skeys <- which(!(names(bloc) == "NODES" |
                                       names(bloc) == "ELEMENTS"))
                bloc[other_skeys] <-
                    bloc[other_skeys] %>%
                    lapply(unlist)

                # add class
                bloc <- structure(bloc, class = "ogs5_msh_bloc")
                return(bloc)                               # return bloc
                }) %>%                                     # add input class
                structure(class = "ogs5_msh"),

# -------------------------------------------------------------------------
                "msp" = add_standard_blocs(filepath),

                "num" = add_standard_blocs(filepath),

                "out" = add_standard_blocs(filepath,
                                           stack_args = TRUE),

                "pcs" = ogs5_read_inputfile_tolist(filepath) %>%
                    lapply(function(x) {
                        x <- lapply(x, function(x){
                                return(unlist(stringr::str_split(x, " ")))
                        })
                        x <- structure(x, class = "ogs5_pcs_process")
                        return(x)}) %>%
                    structure(class = "ogs5_pcs"),

                "pqc" = ogs5_read_pqc_input_tolist(filepath) %>%
                    lapply(function(x) {
                        x <- structure(x, class = "ogs5_pqc_filebloc")
                        return(x)}) %>%
                    structure(class = "ogs5_pqc"),

                "rei" = add_standard_blocs(filepath),

# .rfd input file ---------------------------------------------------------
                "rfd" =
                names(ogs5_list <- ogs5_read_inputfile_tolist(filepath)) %>%
                lapply(function(blc_name) {
                bloc <- ogs5_list[[paste0(blc_name)]]

                if (stringr::str_detect(blc_name, "CURVES")) {

                    # check for column names
                    clnme <- bloc %>%
                            names() %>%
                            stringr::str_split("\"") %>%
                            unlist() %>%
                            stringr::str_extract_all("\\w+") %>%
                            unlist()
                    data_key_ind <- which(tolower(clnme) == "data")

                    if (length(clnme) != data_key_ind) {
                        # columnames exist
                        clnme <- clnme[(data_key_ind + 1):length(clnme)]

                    } else {
                        # assing default columnames
                        clnme <- c("time", "value")
                    }

                    # coerce to tibble
                    bloc <- bloc %>%
                           sapply(stringr::str_split, " ") %>%
                           unlist %>%
                           as.double() %>%
                           matrix(nrow = length(bloc[[1]]), byrow = TRUE) %>%
                           # assign column names
                           'colnames<-' (clnme) %>%
                           tibble::as_tibble()

                } else {
                # other eventual blocs
                    bloc <- lapply(bloc, unlist)
                    bloc <- structure(bloc, class = "ogs5_rfd_bloc")
                }

                return(tibble::as_tibble(bloc))
                }) %>%
                append("CURVES", after = 0) %>%   # insert mkey = "CURVES"
                'names<-' (c("mkey", "data")) %>%
                structure(class = "ogs5_rfd_bloc") %>%
                list() %>% # wrap in list
                'names<-' (c("CURVES1")) %>% # name list
                structure(class = "ogs5_rfd"),

# ------------------------------------------------------------------------
                "st" = add_standard_blocs(filepath,
                                          "ogs5_st_condition"),

                "tim" = add_standard_blocs(filepath),

                NULL # all other, eg. cct, fct, krc, gem at the moment
               )


    if (!overwrite) {

        # check if input exists (overwrite == FALSE)
        if (is.null(existing_blocs)) {

            # input file does not yet exist, assign whole set of blocs
            ogs5_obj$input[[paste0(file_ext)]] <- new_blocs
        }

        # else proceed to return ogs5 object as-is

    } else {

        # overwrite
        ogs5_obj$input[[paste0(file_ext)]] <- new_blocs

    }

    return(ogs5_obj)

}


input_add_blocs_from_file <- function(ogs5_obj,
                                      sim_basename,
                                      filename,
                                      file_dir = "",
                                      overwrite = FALSE) {

    # function that calls upon ogs5_assign_classes_to_ogslist which in turn
    # calls ogs5_read_inputfile_tolist to convert inputfiles into ogs5 objects.
    # filename can be a list of filenames (name.extension), a single filename or
    # 'all'

    # ! if existing ogs5 object is handed in,
    # the specified input bloc will be overwritten !

    if (!dir.exists(file_dir)) {
        stop(paste0("Cannot find directory \"", file_dir, "\""))
    }

    if (is.null(ogs5_obj)) {

        # create new ogs5 object pointing to directory
        ogs5_obj <- create_ogs5(sim_name = filename %>%
                                    stringr::str_extract("\\w+"),
                                sim_id = 1L,
                                sim_path = file_dir)
    }


    if (filename[[1]] == "all") { # browse whole repository for input files

        # get all filenames and extensions in filepath
        all_filenames <- list.files(file_dir)
        all_basefilenames <- all_filenames %>% stringr::str_remove("\\..*")
        all_file_ext <- all_filenames %>% stringr::str_remove(".*\\.")

        possible_ext <- names(ogs5_get_keywordlist())

        # filter out ogs5 input files and extensions
        input_filenames <-
            all_filenames[(all_file_ext %in% possible_ext) &
                          (all_basefilenames == sim_basename)] #%>%

        input_file_ext <- input_filenames %>% stringr::str_remove(".*\\.")

        # filter additional files for third-party software e.g. phreeqc.dat
        add_filenames = all_filenames[!(all_filenames %in% input_filenames)]

        # read ogs5 input files
        for (i in 1:length(input_filenames)) {

            filepath <- paste0(file_dir, "/", input_filenames[i], sep = "")

            print(paste("Reading file", input_filenames[i]))

            ogs5_obj <-
                ogs5_add_input_bloc_from_ogs5list(ogs5_obj =
                                                      ogs5_obj,
                                                  filepath,
                                                  file_ext =
                                                      input_file_ext[i],
                                                  overwrite)
        }

        # read additional files
        if (length(add_filenames>0)) {

            for (i in 1:length(add_filenames)) {

                filepath <- paste0(file_dir, "/", add_filenames[i], sep = "")

                print(paste("Reading file", add_filenames[i]))

                ogs5_obj <-
                    ogs5_add_input_bloc_from_addfile(ogs5_obj = ogs5_obj,
                                                     filepath,
                                                     overwrite)
            }
        }

    } else { # read list or single file ------------------------------------

        file_ext <- filename %>%
            stringr::str_split("\\.") %>%
            lapply(FUN = dplyr::last)

        for (i in 1:length(filename)) {

            filepath <- paste0(file_dir, "/", filename[[i]], sep = "")
            if (!file.exists(filepath)) {
                stop(paste0("file ", filename[[i]], " does not exist"))
            }
            print(paste("Reading file", filename[[i]]))

            ogs5_obj <-
                ogs5_add_input_bloc_from_ogs5list(ogs5_obj = ogs5_obj,
                                               filepath,
                                               file_ext = file_ext[[i]],
                                               overwrite)
        }
    }
    return(ogs5_obj)
}

