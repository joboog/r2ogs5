# read in ex1 input files -------------------------------------------------

# create ogs5 object to compare to ex1 in subfolder ex1_read
# tmp <- "examples/tmp/ex1"
ex1_read <- create_ogs5(sim_name = "ex1_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1_read"))

ex1_read <- input_add_blocs_from_file(ex1_read,
                                      filename = "all",
                                      file_dir = tmp)

# debug(ogs5_list_output.ogs5_rfd)
ogs5_write_inputfiles(ex1_read, type = "all")


###########################################################################
####                read in Engesgaard benchmark input files            ###
###########################################################################


# Engsesgaard/2Kin/slow_kin_pqc -------------------------------------------

bm_dir <- "../../examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc"
#bm_dir <- "examples/benchmarks/Engesgaard/2Kin/slow_kin_pqc"

# create new ogs5 object in temporary directory
eg1_read <- create_ogs5(sim_name = "eg1_read", sim_id = 1L,
                   sim_path = paste0(tmp, "/eg1_read"))

# read in input files from benchmark folder
eg1_read <- input_add_blocs_from_file(eg1_read,
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
                                      file_dir = bm_dir)

ogs5_write_inputfiles(eg1_read, type = "all")

# Engsesgaard/2Kin/slow_kin_pqc_kcr -------------------------------------------

# TODO
