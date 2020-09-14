# Benchmark: Theis_2D


# define ogs5 obj ---------------------------------------------------------
theis_2D <- create_ogs5(sim_name = "theis_2D", sim_id = 1L,
                        sim_path = "examples/tmp/theis_2D")


# add input file blocs ----------------------------------------------------
theis_2D <- input_add_pcs_bloc(x = theis_2D, pcs_name = "groundwaterflow",
                               PCS_TYPE = "GROUNDWATER_FLOW",
                               NUM_TYPE = "NEW",
                               PRIMARY_VARIABLE = "HEAD")

theis_2D <- input_add_bc_bloc(x = theis_2D, bc_name = "groundwater_left",
                              PRIMARY_VARIABLE = "HEAD",
                              DIS_TYPE = "CONSTANT 20.0",
                              PCS_TYPE = "GROUNDWATER_FLOW",
                              GEO_TYPE = "POLYLINE  left_bc")

theis_2D <- input_add_bc_bloc(x = theis_2D, bc_name = "groundwater_right",
                              PRIMARY_VARIABLE = "HEAD",
                              DIS_TYPE = "CONSTANT 20.0",
                              PCS_TYPE = "GROUNDWATER_FLOW",
                              GEO_TYPE = "POLYLINE  right_bc")

theis_2D <- input_add_ic_bloc(x = theis_2D, ic_name = "groundwater",
                              PRIMARY_VARIABLE = "HEAD",
                              PCS_TYPE = "GROUNDWATER_FLOW",
                              GEO_TYPE = "DOMAIN",
                              DIS_TYPE = "CONSTANT 20.0")

theis_2D <- input_add_mfp_bloc(x = theis_2D, FLUID_NAME = "groundwater",
                               FLUID_TYPE = "LIQUID",
                               DENSITY = "1 0.000000e+0",
                               VISCOSITY = "1 1.0")

theis_2D <- input_add_mmp_bloc(x = theis_2D, NAME = "porousmedium",
                               GEOMETRY_DIMENSION = "2",
                               GEOMETRY_AREA = "20.000000e+000",
                               STORAGE = "1 0.00001",
                               PERMEABILITY_TENSOR = "ISOTROPIC 5.787037037037e-4",
                               PERMEABILITY_SATURATION = "1 1.0")

theis_2D <- input_add_num_bloc(x = theis_2D, num_name = "groundwaterflow",
                               PCS_TYPE = "GROUNDWATER_FLOW",
                               LINEAR_SOLVER = "2 5 1.e-014 1000 1.0 100 4",
                               RENUMBER = "2 -1")

theis_2D <- input_add_out_bloc(x = theis_2D, out_name = "groundwaterflow_profile",
                               PCS_TYPE = "GROUNDWATER_FLOW",
                               NOD_VALUES = "HEAD",
                               GEO_TYPE = "DOMAIN",
                               DAT_TYPE = "TECPLOT",
                               TIM_TYPE = "STEPS 1")

theis_2D <- input_add_out_bloc(x = theis_2D, out_name = "groundwaterflow_points",
                               PCS_TYPE = "GROUNDWATER_FLOW",
                               NOD_VALUES = "HEAD",
                               GEO_TYPE = "POINT POINT5",
                               DAT_TYPE = "TECPLOT",
                               TIM_TYPE = "STEPS 1")

theis_2D <- input_add_st_bloc(x = theis_2D, st_name = "groundwaterflow",
                              PCS_TYPE = "GROUNDWATER_FLOW",
                              PRIMARY_VARIABLE = "HEAD",
                              GEO_TYPE = "POINT POINT4",
                              DIS_TYPE = "CONSTANT -1000")

theis_2D <- input_add_tim_bloc(x = theis_2D, tim_name = "groundwaterflow",
                          PCS_TYPE = "GROUNDWATER_FLOW",
                          TIME_START = "0.0",
                          TIME_END = "0.00175",
                          TIME_STEPS = "146 1.2e-5",
                          TIME_UNIT = "DAY")


# add gli
theis_2D <- input_add_gli_points(x = theis_2D,
                                 ogs5_points = tibble::tibble(x = c(0.0, 0.0, 1000, 1000, 500, 600),
                                                              y = c(0.0, 750.0, 750, 0, 375, 375),
                                                              z = c(0.00000000000000e+00, 0.00000000000000e+00, 0.00000000000000e+00,
                                                                    0.00000000000000e+00, 0.00000000000000e+00, 0.00000000000000e+00),
                                                              name = c("", "", "", "", "POINT4", "POINT5")))

theis_2D <- input_add_gli_polyline(x = theis_2D, ply_name = "left_bc",
                                   TYPE = 0,
                                   EPSILON = NULL,     # have to specify EPSILON = NULL
                                   POINTS = "0 1")

theis_2D <- input_add_gli_polyline(x = theis_2D, ply_name = "right_bc",
                                   TYPE = 0,
                                   EPSILON = NULL,     # have to specify EPSILON = NULL
                                   POINTS = "2 3")

theis_2D <- input_add_gli_polyline(x = theis_2D, ply_name = "top_bc",
                                   TYPE = 0,
                                   EPSILON = NULL,     # have to specify EPSILON = NULL
                                   POINTS = "1 2")

theis_2D <- input_add_gli_polyline(x = theis_2D, ply_name = "bottom_bc",
                                   TYPE = 0,
                                   EPSILON = NULL,     # have to specify EPSILON = NULL
                                   POINTS = "3 0")


# ad msh bloc
# load from local directory
# nodes
msh_nodes <- readr::read_delim("og5s_benchmarks/H/Theis_2D/Thies_quad_2d.msh",
                               delim = " ", col_types = "_ddd",
                               col_names = c("x", "y", "z"),
                               skip = 5, n_max = 7500) %>% tibble::as_tibble()

# elements
msh_elements <- readr::read_delim("og5s_benchmarks/H/Theis_2D/Thies_quad_2d.msh",
                               delim = " ", col_types = "_ccdddd",
                               col_names = c("material_id", "ele_type", "node1", "node2", "node3", "node4"),
                               skip = 7507, n_max = 7326) %>% tibble::as_tibble()

theis_2D <- input_add_msh_bloc(x = theis_2D, msh_name = "mesh",
                               ELEMENTS = msh_elements,
                               NODES = msh_nodes)


# write input files -------------------------------------------------------
ogs5_write_inputfiles(theis_2D, "all")


# run ogs5 simulation -----------------------------------------------------
ogs5_run(ogs5_obj = theis_2D, ogs_exe = "inst/ogs/ogs_5.8",
         run_path = NULL,
         log_output = TRUE,
         log_path = "examples/tmp/theis_2D/log")


# read output -------------------------------------------------------------
# reference
tec_ref <- ogs5_read_tecplot(filename = "https://raw.githubusercontent.com/ufz/ogs5-benchmarks_ref/master/H/Theis_2D/Thies_quad_2d_time_POINT5_GROUNDWATER_FLOW.tec",
                                 geo_object = 'POINT')

# output
tec_r2ogs <- ogs5_read_tecplot(filename = "examples/tmp/theis_2D/theis_2D_time_POINT5_GROUNDWATER_FLOW.tec",
                               geo_object = 'POINT')


# compare r2ogs5 output to reference output -------------------------------------------------------------
dplyr::all_equal(tec_ref, tec_r2ogs,
                 ignore_row_order = FALSE)


# visualise the comparison
data.frame(rbind(tec_ref, tec_r2ogs),
           output = c(rep("ref", nrow(tec_ref)),
                      rep("r2ogs", nrow(tec_r2ogs)))) %>%
    ggplot2::ggplot(ggplot2::aes(x = TIME, y = HEAD)) +
    ggplot2::geom_point(ggplot2::aes(color = output, shape = output)) +
    ggplot2::scale_color_manual(values= c("red", "blue")) +
    ggplot2::scale_shape_manual(values= c(2, 16)) +
    ggplot2::theme_bw()


