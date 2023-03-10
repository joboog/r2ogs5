
#' input_add_gli_points
#' @description Adds a sub-bloc to **gli** bloc of *ogs5* for defining point
#'   geometries. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/gli/h_points).
#'   or have a look at the input scripts from the [r2ogs5-benchmarks repository](
#'   https://gitlab.opengeosys.org/ag-hydinf/boog-group/r2ogs5-benchmarks).
#' @param x Simulation object of class *ogs5*.
#' @param ogs5_points *tibble* with columns c("x", "y", "z", "name").
#'
#' @return Updated *ogs5* object.
#' @export
#' @examples
#' tmp <- tempdir()
#' ogs5_obj <- create_ogs5(sim_name = "ex1", sim_id = 1L,
#'                         sim_path = paste0(tmp, "/ex1"))
#'
#' ogs5_obj <- input_add_gli_points(x = ogs5_obj,
#'                 ogs5_points = tibble::tibble(x = c(0, 4.7),
#'                                              y = c(0,0),
#'                                              z = c(0,0),
#'                                              name = c("point0", "point1")))
input_add_gli_points <-

   function(
      x = list(),
      ogs5_points =tibble::tibble() # x y z name
   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$gli exists and valid, otherwise create
      if (!("gli" %in% names(x$input))) {
         x$input$gli <- create_ogs5_gli()
      } else {

         valid_ogs5_gli(x$input$gli)
      }

      if ("points" %in% names(x$input$gli)) {
         stop("points do already exist", call. = FALSE)
      }

      if (!(ogs5_points$x %>% is.numeric())){
         stop("ogs5_points$x is not of type 'numeric' ", call. = FALSE)
      }

      if (!(ogs5_points$y %>% is.numeric())){
         stop("ogs5_points$y is not of type 'numeric' ", call. = FALSE)
      }

      if (!(ogs5_points$z %>% is.numeric())){
         stop("ogs5_points$z is not of type 'numeric' ", call. = FALSE)
      }

      if (!(ogs5_points$name %>% is.character())){
         stop("ogs5_points$name is not of type 'character' ", call. = FALSE)
      }

      if (ogs5_points %>%
            dplyr::select(.data$x, .data$y, .data$z) %>%
            is.na() %>%
            any()){
         stop("ogs5_points coordinates contain NA ", call. = FALSE)
      }

      # create and add sublist to gli-list
      df <- ogs5_points %>%
              tibble::as_tibble() %>%
              dplyr::mutate(
                     pnt_id = ogs5_points %>% rownames() %>%
                              as.integer() %>% -1L
               ) %>%
              dplyr::select(.data$pnt_id, .data$x, .data$y, .data$z, .data$name)
      #rownames(df) <- df %>% rownames() %>% as.numeric() %>% -1

      x$input$gli$points <- df

      valid_ogs5_gli_points(x$input$gli$points)

      return(x)

   }


#' input_add_gli_polyline
#' @description Adds a sub-bloc to **gli** bloc of *ogs5* for defining a
#'  polyline. For additional documentatoin of the input parameters see
#'   the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/gli/h_polyline).
#' @param x Simulation object of class *ogs5*.
#' @param EPSILON Error tolerance of the specified points to the mesh nodes.
#' @param ID ogs5 **gli** bloc sub key word.
#' @param MAT_GROUP ogs5 **gli** bloc sub key word.
#' @param ply_name Name of the polyline.
#' @param POINTS Points to be used for the polyline.
#' @param POINT_VECTOR ogs5 **gli** bloc sub key word.
#' @param TYPE ogs5 **gli** bloc sub key word.
#'
#' @return Updated *ogs5* object.
#' @export
input_add_gli_polyline <-

   function(
      x = list(),
      EPSILON = character(),
      ID = NULL,
      MAT_GROUP = NULL,
      ply_name = character(),
      POINTS = character(),
      POINT_VECTOR = NULL,
      TYPE = NULL
   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$gli exists and valid, otherwise break
      if (!("gli" %in% names(x$input))) {
         stop("you have to add points first", call. = FALSE)
      } else {
         valid_ogs5_gli(x$input$gli)
      }

      if (!("points" %in% names(x$input$gli))) {
         stop("you have to add points first", call. = FALSE)
      }

      # check if POINTS exist
      ply_points <-stringr::str_split(POINTS, " ") %>% .[[1]] %>% as.integer()
      if (!(ply_points %in% x$input$gli$points$pnt_id %>% all())) {
         stop("required POINTS missing", call. = FALSE)
      }

      # check if ogs5-obj$input$gli$polylines exists and validate, otherwise create
      if (!("polylines" %in% names(x$input$gli))) {
         x$input$gli$polylines <- create_ogs5_gli_polylines()
      } else {
         valid_ogs5_gli_polylines(x$input$gli$polylines)
      }

      if (ply_name %in% names(x$input$gli$polylines)) {
         stop("Name alreaedy exists", call. = FALSE)
      }

      # create and add sublist to polylines-list
      x$input$gli$polylines[[paste(ply_name)]] <- list(

         "EPSILON" = EPSILON,
         "ID" = ID,
         "MAT_GROUP" = MAT_GROUP,
         "NAME" = ply_name,
         "POINTS" = POINTS,
         "POINT_VECTOR" = POINT_VECTOR,
         "TYPE" = TYPE
         )%>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_gli_polyline")

      valid_ogs5_gli_polyline(x$input$gli$polylines[[paste(ply_name)]])

      return(x)

   }


#' input_add_gli_surface
#' @description Adds a sub-bloc to **gli** bloc of *ogs5* for defining a
#'  surface based on plylines. For additional documentatoin of the input
#'  parameters see the [ogs5 keyword docs](
#'   https://ogs5-keywords.netlify.app/ogs/wiki/public/doc-auto/by_ext/gli/h_surface).
#' @param x Simulation object of class *ogs5*.
#' @param EPSILON Error tolerance of the specified points to the mesh nodes.
#' @param ID ogs5 **gli** bloc sub key word.
#' @param MAT_GROUP ogs5 **gli** bloc sub key word.
#' @param srf_name Name of the surface. *character*
#' @param POLYLINES Names of the polylines that create the surface. *character*
#' @param TYPE ogs5 **gli** bloc sub key word.
#' @param TIN ogs5 **gli** bloc sub key word.
#'
#' @return Updated *ogs5* object.
#' @export
input_add_gli_surface <-

   function(
      x = list(),
      EPSILON = character(),
      ID = NULL,
      MAT_GROUP = NULL,
      srf_name = character(),
      POLYLINES = character(),
      TYPE = NULL,
      TIN = NULL

   ){

      # validate input
      valid_ogs5(x)

      # look if ogs5-obj$input$gli exists and valid, otherwise break
      if (!("gli" %in% names(x$input))) {
         stop("you have to add points first", call. = FALSE)
      } else {
         valid_ogs5_gli(x$input$gli)
      }

      if (!("points" %in% names(x$input$gli))) {
         stop("you have to add points first", call. = FALSE)
      }

      # check if ogs5_gli_polylines exist
      if (!("polylines" %in% names(x$input$gli))) {
         stop("you have to add polylines first", call. = FALSE)
      }

      # check if the required polylines exist
      plys <-stringr::str_split(POLYLINES, " ") %>% .[[1]]
      if (!(plys %in% names(x$input$gli$polylines) %>% all())){
         stop("required POLYLINES missing", call. = FALSE)
      }

      # check if ogs5-obj$input$gli$surfaces exists and validate, otherwise create
      if (!("surfaces" %in% names(x$input$gli))) {
         x$input$gli$surfaces <- create_ogs5_gli_surfaces()
      } else {
         valid_ogs5_gli_surfaces(x$input$gli$surfaces)
      }

      if (srf_name %in% names(x$input$gli$surfaces)) {
         stop("Name alreaedy exists", call. = FALSE)
      }

      # create and add sublist to surfaces-list
      x$input$gli$surfaces[[paste(srf_name)]] <- list(

         "EPSILON" = EPSILON,
         "ID" = ID,
         "MAT_GROUP" = MAT_GROUP,
         "NAME" = srf_name,
         "POLYLINES" = POLYLINES,
         "TYPE" = TYPE,
         "TIN" = TIN
      )%>%
         purrr::discard(is.null) %>%
         purrr::discard(BBmisc::isFALSE) %>%
         structure(class = "ogs5_gli_surface")

      valid_ogs5_gli_surface(x$input$gli$surfaces[[paste(srf_name)]])

      return(x)

   }
