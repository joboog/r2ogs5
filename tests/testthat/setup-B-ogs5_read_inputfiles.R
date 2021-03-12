
# helper function to test read/write functionality ------------------------

read_write_test <-
    function(ogs5_obj, bm_dir, bm_name) {


        # get filenames of written input files
        read_filenames <- list.files(attributes(ogs5_obj)$sim_path)
        # exclude log folder
        read_filenames <- read_filenames[which(read_filenames != "log")]
        read_basefilenames <- read_filenames %>% stringr::str_remove("\\..*")
        read_file_ext <- read_filenames %>% stringr::str_remove(".*\\.")

        # loop over filenames and read-write compare
        for (i in 1:length(read_filenames)) {

            if(!read_file_ext[i] %in%
               c("bc", "cct", "ddc", "fct", "gli", "gem", "ic", "krc", "mcp",
                 "mfp", "mmp", "msh", "msp", "num", "out", "pcs", "pqc", "rei",
                 "st", "tim", "dat")) {
                next
            }

            test_that(
                paste0("Reading and writing inputfile ",
                       read_filenames[i],
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
                   # TEST
                   expect_equal(orig_vec, read_vec)
               })
    }
}

###########################################################################
####                read read and write ex1 input files                 ###
###########################################################################

# create ogs5 object to compare to ex1 in subfolder ex1_read
# tmp <- "inst/examples/tmp/ex1"
ex1_read <- create_ogs5(sim_name = "ex1_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1_read"))

ex1_read <- input_add_blocs_from_file(ex1_read,
                                      sim_basename = "ex1",
                                      filename = list(
                                                      "ex1.pcs",
                                                      "ex1.bc",
                                                      "ex1.gli",
                                                      "ex1.ic",
                                                      "ex1.mcp",
                                                      "ex1.mfp",
                                                      "ex1.mmp",
                                                      "ex1.msh",
                                                      "ex1.msp",
                                                      "ex1.num",
                                                      "ex1.out",
                                                      "ex1.rfd",
                                                      "ex1.st",
                                                      "ex1.tim"),
                                      file_dir = paste0(tmp, "/ex1"))

ogs5_write_inputfiles(ex1_read, type = "all")




# set folders for benchmark input files -----------------------------------
bm_prefix = "../../inst/"
if (!(dir.exists(bm_prefix))) bm_prefix=""

# read and write Engesgaard benchmark input files -------------------------

# Engsesgaard/2Kin/slow_kin_pqc -------------------------------------------

eg1_dir <- paste0(bm_prefix,
                  "extdata/ogs5_benchmarks/Engesgaard/2Kin/slow_kin_pqc")
# eg1_dir <- "inst/extdata/ogs5_benchmarks/Engesgaard/2Kin/slow_kin_pqc"

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

eg2_dir <- paste0(bm_prefix,
                  "extdata/ogs5_benchmarks/Engesgaard/2Kin/slow_kin_pqc_krc")
#eg2_dir <- "inst/extdata/ogs5_benchmarks/Engesgaard/2Kin/slow_kin_pqc_krc"

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

fct_dir <- paste0(bm_prefix,
                  "extdata/ogs5_benchmarks/Transient_flow")
fct_read <- create_ogs5(sim_name = "fct_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/fct_read"))

fct_read <- input_add_blocs_from_file(fct_read,
                                      sim_basename = "trans_bd_homo",
                          filename = list("trans_bd_homo.bc",
                                          "trans_bd_homo.fct",
                                          "trans_bd_homo.gli",
                                          "trans_bd_homo.ic",
                                          "trans_bd_homo.mfp",
                                          "trans_bd_homo.mmp",
                                          "trans_bd_homo.msh",
                                          "trans_bd_homo.num",
                                          "trans_bd_homo.out",
                                          "trans_bd_homo.pcs",
                                          "trans_bd_homo.st",
                                          "trans_bd_homo.tim"),
                          file_dir = fct_dir)

# disable scientific numbers when calling this function
nosci_op <- options(scipen = 999)
withr::defer(options(nosci_op))

ogs5_write_inputfiles(fct_read, type = "all")

# read ConcreteCrack benchmark --------------------------------------------

# cct_dir <- "/inst/extdata/ogs5_benchmarks/ConcreteCrack"
cct_dir <- paste0(bm_prefix,
                  "extdata/ogs5_benchmarks/ConcreteCrack")
cct_read <- create_ogs5(sim_name = "decal", sim_id = 1L,
                        sim_path = paste0(tmp, "/cct_read"))
#debug(input_add_blocs_from_file)
cct_read <-  input_add_blocs_from_file(cct_read,
                                       sim_basename = "decal",
                                       filename = list(
                                           "cement_bc-dat.lst",
                                           "cement_bc-dbr-0-0000.dat",
                                           "cement_bc-dch.dat",
                                           "cement_bc-ipm.dat",
                                           "decal.bc",
                                           "decal.cct",
                                           "decal.gem",
                                           "decal.gli",
                                           "decal.ic",
                                           "decal.mcp",
                                           "decal.mfp",
                                           "decal.mmp",
                                           "decal.msh",
                                           "decal.msp",
                                           "decal.num",
                                           "decal.out",
                                           "decal.pcs",
                                           "decal.tim",
                                           "decal_partitioned_cfg4.msh",
                                           "decal_partitioned_nodes_4.msh",
                                           "decal_partitioned_elems_4.msh"
                                           ),
                                       overwrite = TRUE,
                                       file_dir = cct_dir)

ogs5_write_inputfiles(cct_read, type = "all")


# read McWorter benchmark --------------------------------------------
ddc_dir <- paste0(bm_prefix,
                  "extdata/ogs5_benchmarks/McWhorter")
ddc_read <- create_ogs5(sim_name = "ddc_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/ddc_read"))

ddc_read <- input_add_blocs_from_file(ddc_read,
                                      sim_basename = "mcwt",
                                      filename = list("mcwt.ddc",
                                                      "mcwt.gli",
                                                      "mcwt.ic",
                                                      "mcwt.mfp",
                                                      "mcwt.mmp",
                                                      "mcwt.msh",
                                                      "mcwt.num",
                                                      "mcwt.out",
                                                      "mcwt.pcs",
                                                      "mcwt.st",
                                                      "mcwt.tim",
                                                      "mcwt.bc"),
                                      file_dir = ddc_dir,
                                      overwrite = TRUE)

ogs5_write_inputfiles(ddc_read, type = "all")
