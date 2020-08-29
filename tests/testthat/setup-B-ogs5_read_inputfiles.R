
# create ogs5 object to compare to ex1 in subfolder ex1_read
# tmp <- "examples/tmp/ex1"
ex1_read <- create_ogs5(sim_name = "ex1_read", sim_id = 1L,
                        sim_path = paste0(tmp, "/ex1_read"))

# debug(ogs5_get_ogs5_from_list)
ex1_read <- input_add_blocs_from_file(ex1_read,
                                      filename = "all",
                                      file_dir = tmp)

# debug(ogs5_list_output.ogs5_rfd)
ogs5_write_inputfiles(ex1_read, type = "all")
