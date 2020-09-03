context("reading and writing of input files - EX1")

# ex1_read object from setup-B
ex1_input <- ex1_read$input

test_that("respective classes are assigned", {

    #input <- ex1_read$input
    # bc classes
    expect_silent(valid_ogs5_bc(ex1_input$bc))
    expect_silent(valid_ogs5_bc_condition(
        ex1_input$bc$BOUNDARY_CONDITION1
    ))
    # cct classes
    # MISSING
    # fct classes
    # MISSING
    # gem clases
    # MISSING
    # gli classes
    expect_silent(valid_ogs5_gli(ex1_input$gli))
    expect_silent(valid_ogs5_gli_points(ex1_input$gli$POINTS1))

    # ic classes
    expect_silent(valid_ogs5_ic(ex1_input$ic))
    expect_silent(valid_ogs5_ic_condition(
        ex1_input$ic$INITIAL_CONDITION1
    ))
    # krc classes
    # MISSING
    # mcp classes
    expect_silent(valid_ogs5_mcp(ex1_input$mcp))
    expect_silent(valid_ogs5_mcp_component(
        ex1_input$mcp$COMPONENT_PROPERTIES1
    ))
    # mfp classes
    expect_silent(valid_ogs5_mfp(ex1_input$mfp))
    expect_silent(valid_ogs5_mfp_bloc(
        ex1_input$mfp$FLUID_PROPERTIES1
    ))
    # mmp classes
    expect_silent(valid_ogs5_mmp(ex1_input$mmp))
    expect_silent(valid_ogs5_mmp_bloc(
        ex1_input$mmp$MEDIUM_PROPERTIES1
    ))
    # msh classes
    expect_silent(valid_ogs5_msh(ex1_input$msh))
    expect_silent(valid_ogs5_msh_bloc(ex1_input$msh$FEM_MSH1))

    # msp classes
    expect_silent(valid_ogs5_msp(ex1_input$msp))
    expect_silent(valid_ogs5_msp_bloc(
        ex1_input$msp$SOLID_PROPERTIES1
    ))
    # num classes
    expect_silent(valid_ogs5_num(ex1_input$num))
    expect_silent(valid_ogs5_num_bloc(
        ex1_input$num$NUMERICS1
    ))
    # out classes
    expect_silent(valid_ogs5_out(ex1_input$out))
    expect_silent(valid_ogs5_out_bloc(
        ex1_input$out$OUTPUT1
    ))
    # pcs classes
    expect_silent(valid_ogs5_pcs(ex1_input$pcs))
    expect_silent(valid_ogs5_pcs_process(ex1_input$pcs$PROCESS1))

    # pqc classes

    # rei classes

    #rfd class
    expect_silent(valid_ogs5_rfd(ex1_input$rfd))

    # tim classes
    expect_silent(valid_ogs5_tim(ex1_input$tim))
    expect_silent(valid_ogs5_tim_bloc(
        ex1_input$tim$TIME_STEPPING1
    ))
    # st classes
    expect_silent(valid_ogs5_st(ex1_input$st))
    expect_silent(valid_ogs5_st_condition(
        ex1_input$st$SOURCE_TERM1
    ))
    # mmp classes
    expect_silent(valid_ogs5_mmp(ex1_input$mmp))
    expect_silent(valid_ogs5_mmp_bloc(
        ex1_input$mmp$MEDIUM_PROPERTIES1
    ))
})

# loop over input files and compare
for (file_ext in names(ex1_input)) {

    test_that(
        paste0("Reading and writing inputfile .",
               file_ext,
               " changes nothing"), {

        # read-in and written vector (see setup-B)
        read_vec <- scan(file = paste0(tmp, "/ex1_read/ex1_read." , file_ext),
                         what = "character",
                         blank.lines.skip = TRUE,
                         sep = "\n",
                         quiet = TRUE)

        # original vector from ex1
        orig_vec <- scan(file = paste0(tmp, "/ex1.", file_ext),
                         what = "character",
                         blank.lines.skip = TRUE,
                         sep = "\n",
                         quiet = TRUE)
        # TEST
        expect_equal(orig_vec, read_vec)
    })
}

# produce OGS5 output to compare
ogs5_run(ogs5_obj = ex1_read, ogs_exe = "../../inst/ogs/ogs_5.76",
         run_path = NULL,
         log_output = TRUE,
         log_path = paste0(tmp, "/ex1_read/log"))

# extract results
tec_df_ex1read <-
    ogs5_read_many_tecplots(filepath = attributes(ex1_read)$sim_path,
                            geo_object = "domain")
# load original result
tracer_sol <- unlist(read.table(file = "../../data/ex1_tracer_result.txt"))

test_that("read-in input files can be used to run ogs5", {
   # compare to solution
    expect_equal(as.numeric(tec_df_ex1read$Tracer),
                 as.numeric(tracer_sol))

})


# Engesgaard read input tests -----------------------------------------------

context("Read Engesgaard benchmakr input files")

# loop over input files and compare
for (file_ext in names(eg1_read$input)) {

    test_that(
        paste0("Reading and writing inputfile .",
               file_ext,
               " changes nothing"), {

        # read-in and written vector (see setup-B)
        read_vec <- scan(file = paste0(tmp, "/eg1_read/eg1_read." , file_ext),
                         what = "character",
                         blank.lines.skip = TRUE,
                         sep = "\n",
                         quiet = TRUE) %>%
                    stringr::str_squish()
        # extra remove empty lines
        read_vec <- read_vec[which(read_vec != "")]
        read_vec <- read_vec[which(read_vec != "#")] # remove '#' lines

        if (file_ext == "dat") {
            filepath <- paste0(bm_dir, "/phreeqc.dat")
        } else {
            filepath <- paste0(bm_dir, "/pds.", file_ext)
        }
        # original vector from ex1
        orig_vec <- scan(file = filepath,
                         what = "character",
                         blank.lines.skip = TRUE,
                         sep = "\n",
                         quiet = TRUE) %>%
                    stringr::str_squish()
        orig_vec <- orig_vec[which(orig_vec != "")]
        orig_vec <- orig_vec[which(orig_vec != "#")]

        # remove elements after 'STOP'
        if (length(orig_vec) != length(read_vec)) {
            orig_vec <- orig_vec[
                -c((which(orig_vec == "#STOP") + 1):length(orig_vec))
            ]
        }



        # TEST
        expect_equal(orig_vec, read_vec)
               })
}



