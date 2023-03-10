# Setup file to run test-ogs5_run_ex1

# Setup files are automatically run when calling test(), before the test files.
# A temporary folder for input- and outputfiles will be created
# in tests/testthat and after execution automatically erased.

tmp <- tempdir() # temporary location
# tmp <- "../../examples/tmp/ex1" # permanent location (easier for debugging)
# define ogs5 obj ---------------------------------------------------------
ex1 <- create_ogs5(sim_name = "ex1", sim_id = 1L,
                   sim_path = paste0(tmp, "/ex1"))

## add input file blocs ----------------------------------------------------
ex1 <- input_add_pcs_bloc(x=ex1, pcs_name = "waterflow",
                          PCS_TYPE = "LIQUID_FLOW",
                          PRIMARY_VARIABLE = "PRESSURE1")

ex1 <- input_add_pcs_bloc(x=ex1, pcs_name = "tracer",
                          PCS_TYPE = "MASS_TRANSPORT",
                          PRIMARY_VARIABLE = "Tracer1",
                          RELOAD = c("1", "1"))

ex1 <- input_add_bc_bloc(x=ex1, bc_name = "pressure_out",
                         PRIMARY_VARIABLE = "PRESSURE1",
                         DIS_TYPE = "CONSTANT 9810",
                         PCS_TYPE = "LIQUID_FLOW",
                         GEO_TYPE = "POINT point1")

ex1 <- input_add_bc_bloc(x=ex1, bc_name = "tracer_in",
                         PRIMARY_VARIABLE = "Tracer",
                         DIS_TYPE = "CONSTANT 1.19438",
                         PCS_TYPE = "MASS_TRANSPORT",
                         GEO_TYPE = "POINT point0",
                         TIM_TYPE = "CURVE 1")

ex1 <- input_add_ic_bloc(x = ex1, ic_name = "pressure",
                         PCS_TYPE = "LIQUID_FLOW",
                         PRIMARY_VARIABLE = "PRESSURE1",
                         GEO_TYPE = "DOMAIN",
                         DIS_TYPE = "CONSTANT 9810")

ex1 <- input_add_ic_bloc(x = ex1, ic_name = "tracer",
                         PCS_TYPE = "MASS_TRANSPORT",
                         PRIMARY_VARIABLE = "Tracer",
                         GEO_TYPE = "DOMAIN",
                         DIS_TYPE = "CONSTANT 0.0")

ex1 <- input_add_mcp_bloc(x = ex1,
                          NAME = "Tracer",
                          MOBILE = 1)

ex1 <- input_add_mfp_bloc(x = ex1, FLUID_NAME = "water",
                          FLUID_TYPE = "LIQUID",
                          DENSITY = "1 1.0E+3",
                          VISCOSITY = "1 1.0E-3")

ex1 <- input_add_mmp_bloc(x = ex1, GEOMETRY_DIMENSION = "1",
                          GEOMETRY_AREA = "1.2",
                          POROSITY = "1 0.38",
                          PERMEABILITY_TENSOR = "ISOTROPIC 1.0E-8",
                          MASS_DISPERSION = "1 1.0",
                          TORTUOSITY = "1 1.0",
                          NAME = "gravel")

ex1 <- input_add_msp_bloc(x = ex1, NAME = "gravel" ,
                          DENSITY = "1 1.8E+3")

ex1 <- input_add_num_bloc(x = ex1, num_name = "waterflow",
                          PCS_TYPE = "LIQUID_FLOW",
                          LINEAR_SOLVER = "2 6 1.0E-14 100 1.0 100 4")

ex1 <- input_add_num_bloc(x = ex1, num_name = "tracer",
                          PCS_TYPE = "MASS_TRANSPORT",
                          LINEAR_SOLVER = "2 6 1.0E-14 100 1.0 100 4")

ex1 <- input_add_out_bloc(x = ex1, out_name = "waterflow",
                          PCS_TYPE = "LIQUID_FLOW",
                          NOD_VALUES = "PRESSURE1\n VELOCITY_X1",
                          GEO_TYPE = "DOMAIN",
                          DAT_TYPE = "PVD",
                          TIM_TYPE = "STEPS 10")

ex1 <- input_add_out_bloc(x = ex1, out_name = "tracer",
                          PCS_TYPE = "MASS_TRANSPORT",
                          NOD_VALUES = "Tracer",
                          GEO_TYPE = "DOMAIN",
                          DAT_TYPE = "PVD",
                          TIM_TYPE = "STEPS 10")

ex1 <- input_add_out_bloc(x = ex1, out_name = "tracer_tec",
                          PCS_TYPE = "MASS_TRANSPORT",
                          NOD_VALUES = "Tracer",
                          GEO_TYPE = "DOMAIN",
                          DAT_TYPE = "TECPLOT", TIM_TYPE = "STEPS 1")

ex1 <- input_add_st_bloc(x = ex1, st_name = "waterflow",
                         PCS_TYPE = "LIQUID_FLOW",
                         PRIMARY_VARIABLE = "PRESSURE1",
                         GEO_TYPE = "POINT point0",
                         DIS_TYPE = "CONSTANT_NEUMANN 7.8611E-06")

ex1 <- input_add_tim_bloc(x = ex1,  tim_name = "waterflow",
                          PCS_TYPE = "LIQUID_FLOW",
                          TIME_START = "0",
                          TIME_END = "36000000",
                          TIME_STEPS = "229 3600")

ex1 <- input_add_tim_bloc(x = ex1,  tim_name = "tracer",
                          PCS_TYPE = "MASS_TRANSPORT",
                          TIME_START = "0",
                          TIME_END = "36000000",
                          TIME_STEPS = "229 3600")
# add rfd
ex1 <- input_add_rfd_bloc(x = ex1, rfd_name = "tracer", mkey = "CURVES",
                          data = tibble::tibble(time=c(0, 3600, 3600.1, 720, 36000000),
                                                conc=c(1,1,0,0,0)))
# add gli
ex1 <- input_add_gli_points(x = ex1,
                            ogs5_points = tibble::tibble(x = c(0, 4.7),
                                                         y = c(0,0),
                                                         z = c(0,0),
                                                         name = c("point0", "point1")))
# ad msh bloc
mesh_lst <- create_structured_mesh_nodes_ele(lx = 4.7, nx = 94)
ex1 <- input_add_msh_bloc(x = ex1, msh_name = "base_mesh",
                          NODES = mesh_lst[[1]],
                          ELEMENTS = mesh_lst[[2]])

# write input files -------------------------------------------------------
ogs5_write_inputfiles(ex1, "all")
