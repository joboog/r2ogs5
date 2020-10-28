context("ogs5 classes")

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
    expect_silent(valid_ogs5_fct(fct_read$input$fct))
    expect_silent(valid_ogs5_fct_bloc(
       fct_read$input$fct$FUNCTION1
    ))
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
    expect_silent(valid_ogs5_rei(eg1_read$input$rei))
    expect_silent(valid_ogs5_rei_bloc(eg1_read$input$rei$REACTION_INTERFACE1))

    #rfd class
    expect_silent(valid_ogs5_rfd(ex1_input$rfd))
    expect_silent(valid_ogs5_rfd_bloc(ex1_input$rfd$CURVES1))

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


context("Read ex1 input files")
read_write_test(ex1_read, bm_dir =  paste0(tmp, "/ex1"), bm_name = "ex1")

# test overwrite barrier --------------------------------------------------
context("Existing input files are recognized and not overwritten")

ex1.2 <- input_add_blocs_from_file(ex1, filename = "all",
                                   sim_basename = "ex1",
                                   file_dir = paste0(tmp, "/ex1"),
                                   overwrite = FALSE)

for (file_ext in names(ex1.2$input)) {

    # intuition: if existing blocs are not overwritten, the names stay the same
    test_that(paste0("existing blocs are overwritten in file *.", file_ext),
              expect_equal(names(ex1.2$input[[paste0(file_ext)]]),
                           names(ex1$input[[paste0(file_ext)]]))
    )
}


# Engesgaard read input tests -----------------------------------------------

context("Read Engesgaard pqc benchmark input files")
read_write_test(eg1_read, bm_dir = eg1_dir, bm_name = "pds")

context("Read Engesgaard pqc_kcr benchmark input files")
read_write_test(eg2_read, bm_dir = eg2_dir, bm_name = "pds")












# *.fct input files -------------------------------------------------------

context("Read fct input files")
read_write_test(fct_read, bm_dir = fct_dir, bm_name = "trans_bd_homo")
