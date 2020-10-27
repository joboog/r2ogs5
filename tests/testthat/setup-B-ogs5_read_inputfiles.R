# read in ex1 input files -------------------------------------------------



# helper function to test read/write functionality ------------------------

read_write_test <-
    function(ogs5_obj, bm_dir, bm_name) {


        # get filenames of written input files
        read_filenames <- list.files(attributes(ogs5_obj)$sim_path)
        read_basefilenames <- read_filenames %>% stringr::str_remove("\\..*")
        read_file_ext <- read_filenames %>% stringr::str_remove(".*\\.")

        # loop over filenames and read-write compare
        for (i in 1:length(read_filenames)) {

            test_that(
                paste0("Reading and writing inputfile ",
                       read_basefilenames[i],
                        " changes nothing"),
                {

                   # read read file (see setup-B)

                   read_filepath <- paste0(attributes(ogs5_obj)$sim_path, "/",
                                           read_filenames[i])

                   read_vec <- scan(file = read_filepath,
                                    what = "character",
                                    blank.lines.skip = TRUE,
                                    sep = "\n",
                                    quiet = TRUE) %>%
                                stringr::str_squish()
                   # extra remove empty lines
                   read_vec <- read_vec[which(read_vec != "")]
                   # remove '#' lines
                   read_vec <- read_vec[which(read_vec != "#")]

                   # read original file
                   # remove read_basefilename, this is important for
                   # additional files, e.g. ogs_simname_whatever.msh
                   if (stringr::str_detect(read_filenames[i],
                                         attributes(ogs5_obj)$sim_name)){

                       # ogs_simname_whatever.msh -> bm_name_whatever.msh
                       orig_filebasename = paste0(bm_name,
                                                  read_filenames[i] %>%
                                                 stringr::str_remove(
                                                 attributes(ogs5_obj)$sim_name))
                   } else {
                       # e.g. phreeqc.dat
                       orig_filebasename <- read_filenames[i]
                   }

                   orig_filepath <- paste0(bm_dir, "/", orig_filebasename)

                   orig_vec <- scan(file = orig_filepath,
                                    what = "character",
                                    blank.lines.skip = TRUE,
                                    sep = "\n",
                                    quiet = TRUE) %>%
                                stringr::str_squish()
                   orig_vec <- orig_vec[which(orig_vec != "")]
                   orig_vec <- orig_vec[which(orig_vec != "#")]

                   # remove elements after 'STOP'
                   if (length(orig_vec) != length(read_vec)) {
                       stop_ind <- which(orig_vec == "#STOP" |
                                         orig_vec == "END")
                       orig_vec <- orig_vec[
                           -c((stop_ind + 1):length(orig_vec))
                       ]
                   }
                    browser()
                   # TEST
                   expect_equal(orig_vec, read_vec)
               })
    }
}

###########################################################################
####                read read and write ex1 input files                 ###
###########################################################################

# create ogs5 object to compare to ex1 in subfolder ex1_read
# tmp <- "examples/tmp/ex1"
ex1_read <- create_ogs5(sim_name = "ex1_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1_read"))

ex1_read <- input_add_blocs_from_file(ex1_read,
                                      sim_basename = "ex1",
                                      filename = "all",
                                      file_dir = paste0(tmp, "/ex1"))

# debug(ogs5_list_output.ogs5_rfd)
ogs5_write_inputfiles(ex1_read, type = "all")


###########################################################################
####           read and write Engesgaard benchmark input files          ###
###########################################################################


# Engsesgaard/2Kin/slow_kin_pqc -------------------------------------------

eg1_dir <- "../../examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc"
# eg1_dir <- "examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc"

# create new ogs5 object in temporary directory
eg1_read <- create_ogs5(sim_name = "eg1_read", sim_id = 1L,
                   sim_path = paste0(tmp, "/eg1_read"))


# read in input files from benchmark folder
eg1_read <- input_add_blocs_from_file(eg1_read,
                                      sim_basename = "pds",
                                      filename = list("pds.msh",
                                                      "pds.bc",
                                                      "pds.gli",
                                                      "pds.ic",
                                                      "pds.mcp",
                                                      "pds.mfp",
                                                      "pds.mmp",
                                                      "pds.msh",
                                                      "pds.msp",
                                                      "pds.num",
                                                      "pds.out",
                                                      "pds.pcs",
                                                      "pds.rei",
                                                      "pds.tim",
                                                      "pds.st",
                                                      "phreeqc.dat",
                                                      "pds.pqc"),
                                      file_dir = eg1_dir)

ogs5_write_inputfiles(eg1_read, type = "all")

# Engsesgaard/2Kin/slow_kin_pqc_kcr -------------------------------------------


eg2_dir <- "../../examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc_krc"
#eg2_dir <- "examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc_krc"
# tmp <- "examples/tmp"
# create new ogs5 object in temporary directory
eg2_read <- create_ogs5(sim_name = "eg2_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/eg2_read"))



# read in input files from benchmark folder
eg2_read <- input_add_blocs_from_file(eg2_read,
                                      sim_basename = "pds",
                                      filename = list("pds.msh",
                                                      "pds.bc",
                                                      "pds.gli",
                                                      "pds.ic",
                                                      "pds.mcp",
                                                      "pds.mfp",
                                                      "pds.mmp",
                                                      "pds.msh",
                                                      "pds.msp",
                                                      "pds.num",
                                                      "pds.out",
                                                      "pds.pcs",
                                                      "pds.rei",
                                                      "pds.tim",
                                                      "pds.st",
                                                      "phreeqc.dat",
                                                      "pds.pqc",
                                                      "pds.krc"),
                                      file_dir = eg2_dir)

ogs5_write_inputfiles(eg2_read, type = "all")












# read GROUNDWATER_FLOW/Transient_flow ------------------------------------

fct_dir <- "../../examples/benchmarks/Transient_flow"
fct_read <- create_ogs5(sim_name = "fct_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/fct_read"))

fct_read <- input_add_blocs_from_file(fct_read,
                                      sim_basename = "trans_bd_homo",
                          filename = "all",
                          file_dir = fct_dir)

# disable scientific numbers when calling this function
nosci_op <- options(scipen = 999)
withr::defer(options(nosci_op))

ogs5_write_inputfiles(fct_read, type = "all")
