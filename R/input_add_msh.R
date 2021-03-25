
#' input_add_msh_bloc
#' @description Adds a sub-bloc to **msh** bloc of *ogs5* for defining meshes.
#'   Only simple geometric meshes such as lines,
#'   rectangles and cubes can be created here. For more complicated geometries
#'   please refer to third party software such as **gmsh**.
#'   For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/msh.html).
#' @param x Simulation object of class *ogs5*.
#' @param msh_name Name of the mesh. *character*
#' @param AREA ogs5 **msh** bloc sub key word.
#' @param AXISYMMETRY ogs5 **msh** bloc sub key word.
#' @param CROSS_SECTION ogs5 **msh** bloc sub key word.
#' @param ELEMENTS A *tibble* with columns c("ele_id", "material_id",
#'                 "ele_type", "node1", "node2" ...).
#' @param GEO_NAME ogs5 **msh** bloc sub key word.
#' @param GEO_TYPE ogs5 **msh** bloc sub key word.
#' @param LAYER ogs5 **msh** bloc sub key word.
#' @param NODES A *tibble* with columns c("node_id", "x", "y", "z").
#' @param PCS_TYPE ogs5 **msh** bloc sub key word.
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' mesh_lst <- create_structured_mesh_nodes_ele(lx = 4.7, nx = 94)
#' ogs5_obj <- input_add_msh_bloc(x = ogs5_obj, msh_name = "base_mesh",
#'                                NODES = mesh_lst[[1]],
#'                                ELEMENTS = mesh_lst[[2]])
input_add_msh_bloc <-

  function(
    x = list(),
    msh_name = character(),
    AREA = NULL,
    AXISYMMETRY = NULL,
    CROSS_SECTION = NULL,
    ELEMENTS =tibble::tibble(), # material_id ele_type node1 node2 (node3, node4
    GEO_NAME = NULL,
    GEO_TYPE = NULL,
    LAYER = NULL,
    NODES = tibble::tibble(), # x y z
    PCS_TYPE = "NO_PCS"
  ){

    # validate input
    valid_ogs5(x)

    # look if ogs5-obj$input$msh exists and valid, otherwise create
    if (!("msh" %in% names(x$input))) {
      x$input$msh <- create_ogs5_msh()
    } else {

      valid_ogs5_msh(x$input$msh)

      if (msh_name %in% names(x$input$msh)) {
        stop("msh_name does already exist", call. = FALSE)
      }

      # check if basemesh (mesh without PCS definitoin exist, there is only one
      # basemesh allowed)
      exist_base_mesh = FALSE
      for (mesh in x$input$msh) {
        if (mesh$PCS_TYPE == "NO_PCS") exist_base_mesh = TRUE
      }
      if (exist_base_mesh == TRUE && PCS_TYPE == "NO_PCS"){
        stop("A base mesh (mesh without PCS_TYPE defined) does already exist. You can have only one base mesh.", call. = FALSE)
      }

      if (PCS_TYPE!="MASS_TRANSPORT" &&
          PCS_TYPE %in% sapply(x$input$msh, "[[", 1)
      ) {
        stop("PCS_TYPE does already exist", call. = FALSE)
      }
    }

    # check ELEMENTS
    if (!(tibble::is_tibble(ELEMENTS))){
      stop("ELEMENTS has to be a 'tibble' ",
           call. = FALSE)
    }

    if (!all(c("ele_id", "material_id", "ele_type", "node1", "node2") %in% names(ELEMENTS))){
      stop("ELEMENTS has to have headers: 'ele_id', 'material_id', 'ele_type', 'node1', 'node2', ...",
           call. = FALSE)
    }

    if (ELEMENTS$ele_id[1] != 0){
      ELEMENTS$ele_id <- ELEMENTS$ele_id %>% as.numeric() %>% -1L
    }

    # check NODES
    if (!(tibble::is_tibble(NODES))){
      stop("NODES has to be a 'tibble' ",
           call. = FALSE)
    }

    if (!all(c("node_id", "x", "y", "z") %in% names(NODES))){
      stop("NODES has to have headers: 'node_id', 'x', 'y', 'z'",
           call. = FALSE)
    }

    if (NODES$node_id[1] != 0){
      NODES$node_id <- NODES$node_id %>% as.numeric() %>% -1L
    }

    # create and add sublist to bc-list
    x$input$msh[[paste(msh_name)]] <- list(

      "AREA" = AREA,
      "AXISYMMETRY" = AXISYMMETRY,
      "CROSS_SECTION" = CROSS_SECTION,
      "ELEMENTS" = ELEMENTS,
      "GEO_NAME" = GEO_NAME,
      "GEO_TYPE" = GEO_TYPE,
      "LAYER" = LAYER,
      "NODES" = NODES,
      "PCS_TYPE" = PCS_TYPE

    ) %>%
      purrr::discard(is.null) %>%
      purrr::discard(BBmisc::isFALSE) %>%
      structure(class = "ogs5_msh_bloc")

    valid_ogs5_msh_bloc(x$input$msh[[paste(msh_name)]])

    return(x)

  }
