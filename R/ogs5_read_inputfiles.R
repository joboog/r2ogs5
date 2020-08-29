
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
            #print(paste0("mkey: ", tmp1, " i: ", i))
            mkey <- mkey + 1
            skey <- 0 # set back to 0
            i <- i + 1
        } else {
            # create sublist if skey found
            if (is_skey) {
                tmp2 <- chr[i] %>% stringr::str_extract_all("\\w+")
                l[[paste0(tmp1)]][[paste0(tmp2)]] <- list()
                #print(paste0("skey: ", tmp2, " i: ", i))
                i <- i + 1
                skey <- skey + 1
            } else {
                # add entry to list
                j <- 1
                while(!any(c(is_skey, is_mkey))) {
                    tmp3 <- chr[i] %>% stringr::str_squish()
                    if (chr[i] != "" & skey != 0){
                        l[[paste0(tmp1)]][[skey]][j] <- tmp3
                        #(paste0(": ", tmp3, " i: ", i))
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

    pqc_mkeys <- ogs5_keywordlist$pqc$mkey[
                    which(!ogs5_keywordlist$pqc$mkey == "ende")
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


ogs5_get_ogs5_from_list <-
    function(ogs5_obj, filepath, ogs5_list = NULL, file_ext) {

    # function that handles all the exceptions depending on file_ext

    valid_ogs5(ogs5_obj)

    # add the list of blocs in the right format & class from ogs5_list
    ogs5_obj$input[[paste0(file_ext)]] <-

        switch(file_ext,

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

# .rfd input file ---------------------------------------------------------

                "rfd" =
                names(ogs5_list <- ogs5_read_inputfile_tolist(filepath)) %>%
                lapply(function(blc_name) {
                bloc <- ogs5_list[[paste0(blc_name)]]

                if (stringr::str_detect(blc_name, "CURVES")) {
                # coerce to tibble
                    bloc <- bloc %>%
                        sapply(stringr::str_split, " ") %>%
                        unlist %>%
                        as.double() %>%
                        matrix(nrow = length(bloc[[1]]), byrow = TRUE) %>%
                        # assign column names
                        'colnames<-' (c("time", "conc")) %>%
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
                list() %>% # wrap in list
                'names<-' (c("CURVES1")) %>% # name list
                structure(class = "ogs5_rfd"),

# .gli input file ---------------------------------------------------------
               "gli" =                              # loop over bloc names!
                names(ogs5_list <- ogs5_read_inputfile_tolist(filepath)) %>%
                lapply(function(blc_name) {

                bloc <- ogs5_list[[paste0(blc_name)]]

                # POINTS: convert into tibble
                if (stringr::str_detect(blc_name, "POINTS")) {
                suppressWarnings(
                suppressMessages(
                    bloc <- bloc %>%
                        sapply(stringr::str_split, " ") %>%
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
                return(bloc)                                # return bloc
                }) %>%
                'names<-' (c(names(ogs5_list))) %>% # restore bloc names
                structure(class = "ogs5_gli"),      # add input class


# bc, ic, mmp, msp, mfp ---------------------------------------------------

                "bc" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        # unlist the bottom entry & assing class
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_bc_condition")
                        return(x)}) %>%
                        structure(class = "ogs5_bc"),

                "ic" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        # unlist the bottom entry & assing class
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_ic_condition")
                        return(x)}) %>%
                        structure(class = "ogs5_ic"),

               "pcs" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_pcs_process")
                        return(x)}) %>%
                        structure(class = "ogs5_pcs"),

               "mmp" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_mmp_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_mmp"),

                "mcp" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_mcp_component")
                        return(x)}) %>%
                        structure(class = "ogs5_mcp"),

                "mfp" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_mfp_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_mfp"),

                "msp" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_msp_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_msp"),
                "num" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_num_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_num"),
                "out" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, function(x){
                            if (length(x) > 1) {
                                x[-length(x)]  <- paste0(x[-length(x)], "\n")
                                return(x)
                            }
                            return(unlist(x))})
                        x <- structure(x, class = "ogs5_out_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_out"),

                "tim" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_tim_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_tim"),

                "st" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_st_condition")
                        return(x)}) %>%
                        structure(class = "ogs5_st"),

                "rei" = ogs5_read_inputfile_tolist(filepath) %>%
                lapply(function(x) {
                        x <- lapply(x, unlist)
                        x <- structure(x, class = "ogs5_rei_bloc")
                        return(x)}) %>%
                        structure(class = "ogs5_rei"),


# pqc and phreeqc.dat -----------------------------------------------------

                "pqc" = ogs5_read_pqc_input_tolist(filepath) %>%
                        lapply(function(x) {
                            x <- structure(x, class = "ogs5_pqc_filebloc")
                            return(x)}) %>%
                        structure(class = "ogs5_pqc"),

                "dat" = ogs5_read_pqc_input_tolist(filepath) %>%
                        lapply(function(x) {
                            x <- structure(x, class = "ogs5_dat_bloc")
                            return(x)}) %>%
                        structure(class = "ogs5_phreeqc_dat"),

                NULL # all other, eg. cct, fct, rei, krc, gem at the moment
               )

    return(ogs5_obj)

}


input_add_blocs_from_file <-

    function(ogs5_obj, filename, file_dir = "") {


    # ! if existing ogs5 object is handed in,
    # the specified input bloc will be overwritten !

    if (is.null(ogs5_obj)) {

        # create new ogs5 object pointing to directory
        ogs5_obj <- create_ogs5(sim_name = filename %>%
                                           stringr::str_extract("\\w+"),
                                sim_id = 1L,
                                sim_path = file_dir)
    }

    if (length(filename) > 1) {

        # if list of filenames supplied
        file_ext <- filename %>%
            stringr::str_split("\\.") %>%
            lapply(FUN = dplyr::last)

        for (i in 1:length(filename)) {
            print(paste("Reading file", filename[[i]]))

            filepath <- paste0(file_dir, "/", filename[[i]], sep = "")

            ogs5_obj <-
                ogs5_get_ogs5_from_list(ogs5_obj = ogs5_obj,
                                        filepath,
                                        file_ext = file_ext[[i]])
        }



    } else if (filename == "all") { # browse whole repository for input files

        # get all filenames and extensions in filepath
        all_filenames <- list.files(file_dir) %>%
                         stringr::str_split("\\.")
        all_file_ext <- all_filenames %>%
                        lapply(FUN = dplyr::last)

        possible_ext <- names(ogs5_get_keywordlist())

        # filter out input files, ignore all others
        input_filenames <-
            all_filenames[which(all_file_ext %in% possible_ext)] %>%
            lapply(FUN = paste0, collapse = ".")

        # get existing input file extensions
        input_file_ext <- all_file_ext[which(all_file_ext %in% possible_ext)]

        for (i in 1:length(input_filenames)) {
            print(paste("Reading file", input_filenames[[i]]))

            filepath <- paste0(file_dir, "/", input_filenames[[i]], sep = "")

            ogs5_obj <- ogs5_get_ogs5_from_list(ogs5_obj = ogs5_obj,
                                                filepath,
                                                file_ext = input_file_ext[[i]])
        }

    } else { # read single file ------------------------------------------------
        filepath <- paste0(file_dir, "/", filename, sep = "")

        if(!file.exists(filepath)) {
            stop(paste0("file or directory \"", filepath, "\" does not exist"))
        }
        file_ext <- filename %>%
                    stringr::str_split("\\.") %>%
                    lapply(dplyr::last) %>%
                    unlist()

        # import as list and add bloc to ogs5 object
        ogs5_obj <- ogs5_get_ogs5_from_list(ogs5_obj = ogs5_obj,
                                            filepath,
                                             file_ext = file_ext)


    }
        return(ogs5_obj)
}

