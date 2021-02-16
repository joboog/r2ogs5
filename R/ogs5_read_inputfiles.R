
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
    chr <- chr[which(chr != "")]

    mkey_positions <- stringr::str_which(chr, "#")
    skey_positions <- chr %>%
        stringr::str_starts("\\$") %>%
        which()

    # iterate over character vector
    l <- list()
    i <- 1
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
                    unlist()

            if (length(tmp1) > 1) {
                tmp1 <- tmp1 %>% paste0(collapse = " ")
            } else {
                tmp1 <- tmp1 %>%
                    stringr::str_c(mkey)
            }

            l[[paste0(tmp1)]] <- list()
            mkey <- mkey + 1
            skey <- 0 # set back to 0

            if (length(skey_positions) != 0 |
                stringr::str_starts(chr[i + 1], "\\$")) {

                # determine positions of skeys under this mkey
                next_mkey <- mkey_positions[which(mkey_positions - i > 0)]
                next_mkey <- next_mkey[next_mkey == min(next_mkey)]
                skey_pos <- skey_positions[i < skey_positions &
                                               skey_positions < next_mkey]

                l[[paste0(tmp1)]] <- vector(mode = "list", length(skey_pos))
                names(l[[paste0(tmp1)]]) <- chr[skey_pos] %>%
                    stringr::str_remove("\\$")
            }
            i <- i + 1
        } else {
            # create sublist if skey found
            if (is_skey) {
                i <- i + 1
                skey <- skey + 1
            } else {
                # add entry to list
                j <- 1
                while(!any(c(is_skey, is_mkey))) {
                    tmp3 <- chr[i] %>%
                        stringr::str_squish()

                    if(!exists("tmp1")) { # for file without # nor $
                        l[j] <- tmp3
                    } else {
                        if (chr[i] != "" & skey != 0) {
                            l[[paste0(tmp1)]][[skey]][j] <- tmp3
                        } else if (chr[i] != "") {
                            l[[paste0(tmp1)]][j] <- tmp3
                        }
                    }
                    j <- j + 1
                    i <- i + 1 # next line
                    is_mkey <- stringr::str_starts(chr[i], "#")
                    is_skey <- stringr::str_starts(chr[i], "\\$")

                    if (is.na(is_mkey) | is.na(is_skey)) {
                        # this happens only when #STOP is missing
                        # at the end of the file
                        return(l)
                    }
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
                               convert_fun = I) {

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
                        lapply(function(sub_bloc) {

                            # unlist
                            sub_bloc <- unlist(sub_bloc) %>%
                                        convert_fun()


                            return(sub_bloc)
                        })

                bloc <- structure(bloc, class = sub_bloc_class)
                return(bloc)
                }) %>%
            structure(class = bloc_class)
            return(blocs)
}

add_fct_bloc <- function(bloc){


    if (!(is.null(bloc[["DATA"]]))){
        bloc[["DATA"]] <- bloc[["DATA"]] %>%
            lapply(stringr::str_split, " ") %>%
            unlist %>%
            as.double %>%
            matrix(nrow = length(bloc[["DATA"]]),
                   byrow = TRUE) %>%
            'colnames<-' (c("x", "y")) %>%
            tibble::as_tibble()
    }
    if (!(is.null(bloc[["MATRIX"]]))){
        bloc[["MATRIX"]] <- bloc[["MATRIX"]] %>%
            lapply(stringr::str_split, " ") %>%
            unlist %>%
            as.double %>%
            matrix(nrow = length(bloc[["MATRIX"]]),
                   byrow = TRUE) %>%
            tibble::as_tibble()
    }
    other_skeys <- which(!names(bloc) %in% c("DATA", "MATRIX"))
    bloc[other_skeys] <- bloc[other_skeys] %>%
        lapply(unlist)
    bloc <- structure(bloc, class = "ogs5_fct_bloc")
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
# .cct input file ---------------------------------------------------------
               "cct" = ogs5_read_inputfile_tolist(filepath) %>%
                   lapply(function(bloc) {

                       neighbor_skeys <- which(names(bloc) == "NEIGHBOR")
                       other_skeys <- which(names(bloc) != "NEIGHBOR")

                       bloc[neighbor_skeys] <- bloc[neighbor_skeys] %>%
                           lapply(function(skey_bloc) {
                               list(skey_bloc[1:2], # first two single numbers
                                    skey_bloc[-c(1, 2)] %>%
                                    lapply(stringr::str_split, " ") %>%
                                    unlist %>%
                                    as.double %>%
                                    matrix(nrow = length(skey_bloc[-c(1:2)]),
                                           byrow = TRUE) %>%
                                    'colnames<-' (c("x", "y")) %>%
                                    tibble::as_tibble()
                               )
                           })

                       bloc[other_skeys] <- bloc[other_skeys] %>%
                           lapply(unlist)
                       bloc <- structure(bloc, class = "ogs5_cct_bloc")

                   }) %>% structure(class = "ogs5_cct"),

                 "ddc" = add_standard_blocs(filepath,
                                          convert_fun = as.numeric),


# .fct input file ---------------------------------------------------------
                "fct" = ogs5_read_inputfile_tolist(filepath) %>%
                            lapply(function(bloc) {
                                add_fct_bloc(bloc)
                            }) %>%
                            structure(class = "ogs5_fct"),

# .gem file ---------------------------------------------------------------
                "gem" = add_standard_blocs(filepath),
# .gli input file ---------------------------------------------------------
               "gli" =                              # loop over bloc names!
                names(ogs5_list <- ogs5_read_inputfile_tolist(filepath)) %>%
                lapply(function(blc_name) {

                    bloc <- ogs5_list[[paste0(blc_name)]]

                    # POINTS: convert into tibble
                    if (stringr::str_detect(blc_name, "POINTS")) {

                     # make sure only rownames, xyz and names are there
                     bloc <- bloc %>%
                         stringr::str_split(" ") %>%
                         lapply(function(sub) {
                             dollars <- stringr::str_which(sub, "\\$")
                             if (all(is.na(dollars))) {
                                 sub <- sub[1:4]
                             } else {
                                 sub <- c(sub[1:4], sub[dollars[1]:length(sub)])
                             }
                             return(sub)
                         })
                     # find dollar sing(s)
                     dollars <- bloc %>%
                         lapply(function(sub) {
                             sub <- sub %>%
                                 stringr::str_which("\\$")
                                 })
                     dollars <- dollars[
                        which(!dollars %>% sapply(function(d) length(d) == 0))
                        ] %>%
                         unlist %>%
                         unique

                     names <- bloc %>%
                              lapply(function(sub) {
                                  sub[dollars] %>%
                                      stringr::str_remove("\\$") %>%
                                      stats::na.omit()}) %>%
                              unlist %>%
                              tolower %>%
                              unique

                     # add "" for missing $NAME or $MD
                     max_sublength <- bloc %>% lapply(length) %>% unlist %>% max
                     bloc <- bloc %>%
                         lapply(function(sub) {
                             len_dif <-  max_sublength - length(sub)
                             if (len_dif != 0) {
                                 sub <- c(sub, rep("", len_dif))
                             }
                             return(sub)
                         })

                     # pack into tibble
                suppressWarnings(
                 suppressMessages(
                     bloc <- bloc %>%
                     unlist %>%
                     matrix(nrow = length(bloc), byrow = TRUE) %>%
                     tibble::as_tibble(.name_repair = "unique") %>%
                     dplyr::rename_at( # one after $NAME
                         .vars = dollars + 1,
                         .funs = ~ names) %>%
                     dplyr::select_if(!stringr::str_detect(., "\\$")) %>%
                     dplyr::rename_at(2:4, ~c("x", "y", "z")) %>%
                     dplyr::mutate_at(.vars = c("x", "y", "z"),
                                      .funs = as.double) %>%
                     tibble::as_tibble() %>%
                     tibble::column_to_rownames("...1") %>%
                     dplyr::select(.data$x, .data$y, .data$z, {{names}})
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
                # find words
                geometries <- stringr::str_extract(emts, "[:alpha:]+") %>%
                                unique()
                bloc[["ELEMENTS"]] <- vector(mode = "list", length(geometries))
                names(bloc[["ELEMENTS"]] ) <- geometries

                start <- 1
                end <- 0
                for (g in geometries) {

                    n <- sum(stringr::str_detect(emts, g))
                    g_ind <- stringr::str_which(emts, g)
                    mat <-  emts[g_ind] %>%
                        lapply(stringr::str_split, " ") %>%
                        unlist %>%
                        matrix(nrow = n, byrow = TRUE)

                    colnames(mat) <- c("nr",
                                       "material_id",
                                       "ele_type",
                                       paste0("node", 1:(ncol(mat) - 3)))

                    bloc[["ELEMENTS"]][[paste0(g)]] <- mat %>%
                        tibble::as_tibble() %>%
                        dplyr::mutate_at(dplyr::vars(-.data$ele_type),
                                         list(as.double))
                }

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

                "out" = add_standard_blocs(filepath),

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

                if (stringr::str_detect(blc_name, "CURVE")) {

                    # check for column names
                    clnme <- bloc %>%
                            names() %>%
                            stringr::str_split(" ") %>%
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

                    bloc <- list(mkey = "CURVES", data = bloc)

                } else {
                # other eventual blocs
                    bloc <- lapply(bloc, unlist)
                }
                bloc <- structure(bloc, class = "ogs5_rfd_bloc")

                return(bloc)
                }) %>%
                'names<-' (names(ogs5_list)) %>%
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


#' Add ogs5 blocs directly from existing inputfile
#'
#' @description Function to add input files directly from ogs5 input files
#' to an instance of class  *ogs5*.
#' @param ogs5_obj Instance of class *ogs5*. If set to **NULL**, a new ogs5
#' instance is created with filename and file_dir as simulation name and path
#' respectively.
#' @param sim_basename (*character*) The simulation name, usually the filename of
#' the input files without extension.
#' @param filename (*character*) Can be the name of the file (with extension) to be
#' read in, a list of file names or "all".
#' @param file_dir (*character*) A path to the directory where the inputfiles are.
#' @param overwrite (*logical*) If set to TRUE, existing blocs of the same type
#' (input file extension) will be overwritten. In order to add several blocs of
#' the same type, the option needs to be set to FALSE.
#'
#' @details If [filename] is set to "all", the whole folder specified in
#'  [file_dir] will be searched for possible input files excluding [*.tec],
#'  [*.vtu] [*.vtk] and [*.pvd] files. However, the presence of files that are
#'  not ogs5 input files such as for example [*.log]-files might have the
#'  function crashing. For [filenames] that are different from [sim_basename], blocs of
#'  the class [ogs5_additional] are added.
#'
#' @return returns an instance of *ogs5* with the new input blocs added
#' as instances of their respective class.
#'
#' @export
#'
#' @examples
#' ex1 <- input_add_blocs_from_file(ogs5_obj = NULL,
#'                        sim_basename = "decal",
#'                        filename = "decal.mfp",
#'                        file_dir = "examples/benchmarks/ConcreteCrack",
#'                        overwrite = FALSE)
input_add_blocs_from_file <- function(ogs5_obj = NULL,
                                      sim_basename,
                                      filename,
                                      file_dir = "",
                                      overwrite = FALSE) {


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
        all_dirs <- list.dirs(file_dir, full.names = FALSE)
        all_filenames <- list.files(file_dir)
        all_filenames <- all_filenames[!(all_filenames %in% all_dirs[-1])]

        all_basefilenames <- all_filenames %>% stringr::str_remove("\\..*")
        all_file_ext <- all_filenames %>% stringr::str_remove(".*\\.")

        possible_ext <- names(ogs5_get_keywordlist())
        ignore_ext <- c("vtu", "vtk", "pvd", "tec") # to be continued...

        # filter out ogs5 input files and extensions
        input_filenames <-
            all_filenames[(all_file_ext %in% possible_ext) &
                          (all_basefilenames == sim_basename)] #%>%

        input_file_ext <- input_filenames %>% stringr::str_remove(".*\\.")

        # filter additional files for third-party software e.g. phreeqc.dat
        add_filenames = all_filenames[!(all_filenames %in% input_filenames) &
                                          !(all_file_ext %in% ignore_ext)]

        # read ogs5 input files
        for (i in 1:length(input_filenames)) {

            filepath <- paste0(file_dir, "/", input_filenames[i], sep = "")

            print(paste("Reading file", input_filenames[i]))

            ogs5_obj <-
                ogs5_add_input_bloc_from_ogs5list(ogs5_obj = ogs5_obj,
                                                  filepath,
                                                  file_ext = input_file_ext[i],
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

            base_name_i <- basename(filepath) %>%
                stringr::str_remove(paste0(".", file_ext[i]))
            if(base_name_i == sim_basename) {
                ogs5_obj <-
                    ogs5_add_input_bloc_from_ogs5list(ogs5_obj = ogs5_obj,
                                                      filepath,
                                                      file_ext = file_ext[[i]],
                                                      overwrite)
            } else {
                # additional files with other basenames
                ogs5_obj <-
                    ogs5_add_input_bloc_from_addfile(ogs5_obj = ogs5_obj,
                                                     filepath,
                                                     overwrite)
            }


        }
    }
    return(ogs5_obj)
}

