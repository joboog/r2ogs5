# create regular meshes

# input: origin, domain size, num_elem or ele_size

# origin = c(-1,0,0)
# 
# lx <- 5
# nx <- 10
# sx <- NULL
# ly <- NULL
# ny <- NULL
# sy <- NULL
# lz <- NULL
# sz <- NULL
# nz <- NULL

# computes axis parallel vector based on size_ele or n_ele
comp_coordinate_vector <- function(origin = numeric(), d = numeric(),
                                   dist = numeric(), n_ele = NULL, s_ele = NULL){
  
  if (!(is.null(dist))){
    if (!(is.null(n_ele))){
      # compute coords based on n_ele
      end_coord = origin[d] + dist
      x_cords = seq(origin[d], end_coord, length.out = n_ele+1)
    }
    else{
      if (!(is.null(s_ele))){
        # compute coords based on s_ele
        end_coord = origin[d] + dist
        x_cords = seq(origin[d], end_coord, by = s_ele)
        if (dist%%s_ele != 0) x_cords <- c(x_cords, end_coord)
      }
    }
  }
  return(x_cords)
}

# compute hex element df ----------------------------------------------------
generateRegHexEle <- function(x_vector = numeric(), y_vector = numeric(),
                              z_vector = numeric()){
  # this code is adapted from ogs6::generateRegularHexMesh()
  
  n_nodes_x <- length(x_vector)
  n_nodes_y <- length(y_vector)
  n_nodes_z <- length(z_vector)
  n_ele_x <- length(x_vector)-1
  n_ele_y <- length(y_vector)-1
  n_ele_z <- length(z_vector)-1
  ele_df <- tibble(material_id = numeric(), ele_type = character(),
                   node1 = numeric(), node2 = numeric(),
                  node3 = numeric(), node4 = numeric(),node5 = numeric(),
                  node6 = numeric(), node7 = numeric(), node8 = numeric())
  
  for (i in 0:(n_ele_z-1)){
    
    offset_z1 <- i * n_nodes_x * n_nodes_y
    offset_z2 <- (i+1) * n_nodes_x * n_nodes_y
    for (j in 0:(n_ele_y-1)){
      
      offset_y1 <- j * n_nodes_x 
      offset_y2 <- (j+1) * n_nodes_x
      for (k in 0:(n_ele_x-1)){
        ele_df <- add_row(.data = ele_df,
                          material_id = 0,
                          ele_type = "hex",
                          node1 = offset_z1 + offset_y1 + k,
                          node2 = offset_z1 + offset_y1 + k + 1,
                          node3 = offset_z1 + offset_y2 + k + 1,
                          node4 = offset_z1 + offset_y2 + k,
                          node5 = offset_z2 + offset_y1 + k,
                          node6 = offset_z2 + offset_y1 + k + 1,
                          node7 = offset_z2 + offset_y2 + k + 1,
                          node8 = offset_z2 + offset_y2 + k)
      }
    }
  }
  return((ele_df))
}

# compute quad element df ----------------------------------------------------
generateRegQuadEle <- function(vector1 = numeric(), vector2 = numeric()){
  # this code is adapted from ogs6::generateRegularHexMesh()
  
  n_nodes1 <- length(vector1)
  n_nodes2 <- length(vector2)
  n_ele1 <- length(vector1)-1
  n_ele2 <- length(vector2)-1
  ele_df <- tibble(material_id = numeric(), ele_type = character(),
                   node1 = numeric(), node2 = numeric(),
                  node3 = numeric(), node4 = numeric())
  
  for (j in 0:(n_ele2-1)){
    
    offset_v1_1 <- j * n_nodes1
    offset_v2_2 <- (j+1) * n_nodes1
    for (k in 0:(n_ele1-1)){
      ele_df <- add_row(.data = ele_df,
                        material_id = 0,
                       ele_type = "quad",
                       node1 = offset_v1_1 + k,
                       node2 = offset_v1_1 + k + 1,
                       node3 = offset_v2_2 + k + 1,
                       node4 = offset_v2_2 + k)
    }
  }
  return((ele_df))
}

# main functoin -----------------------------------------------------------

create_structured_mesh_nodes_ele <- 
  
  function(
    origin = c(0,0,0),
    lx = NULL, nx = NULL, sx = NULL,
    ly = NULL, ny = NULL, sy = NULL,
    lz = NULL, nz = NULL, sz = NULL
  ){
  # returns a list with a tibble for nodes and elements
    
  # set mesh_dim
  mesh_dim <- c(FALSE, FALSE, FALSE)
  
  # comp base vectors
  if (!(is.null(lx)) & !(is.null(nx))){
    x_vector <- comp_coordinate_vector(origin = origin, d = 1,
                                       dist = lx, n_ele = nx)
    mesh_dim[1] <- TRUE
  } 
  
  if (!(is.null(lx)) & !(is.null(sx))){
    x_vector <- comp_coordinate_vector(origin = origin, d = 1,
                                       dist = lx, s_ele = sx)
    mesh_dim[1] <- TRUE
  } 
  
  if (!(is.null(ly)) & !(is.null(ny))){
    y_vector <- comp_coordinate_vector(origin = origin, d = 2,
                                       dist = ly, n_ele = ny)
    mesh_dim[2] <- TRUE
  } 
  
  if (!(is.null(ly)) & !(is.null(sy))){
    y_vector <- comp_coordinate_vector(origin = origin, d = 2,
                                       dist = ly, s_ele = sy)
    mesh_dim[2] <- TRUE
  } 
  
  if (!(is.null(lz)) & !(is.null(nz))){
    z_vector <- comp_coordinate_vector(origin = origin, d = 3,
                                       dist = lz, n_ele = nz)
    mesh_dim[3] <- TRUE
  } 
  
  if (!(is.null(lz)) & !(is.null(sz))){
    z_vector <- comp_coordinate_vector(origin = origin, d = 3,
                                       dist = lz, s_ele = sz)
    mesh_dim[3] <- TRUE
  } 
  
  # compute nodes_df
  if (mesh_dim[1] == FALSE) x_vector <- 0
  if (mesh_dim[2] == FALSE) y_vector <- 0
  if (mesh_dim[3] == FALSE) z_vector <- 0
  
  xyz_vector <- list(x_vector, y_vector, z_vector)
  
  nodes_df <- as_tibble(expand.grid(x = xyz_vector[[1]], y = xyz_vector[[2]],
                                    z = xyz_vector[[3]]))
  
  # create element_df
  # 1d
  if (length(mesh_dim[mesh_dim == TRUE]) == 1) {
    
    ele_type <- "line"
    ele_df <- tibble(material_id = vector("numeric", length(nodes_df[[1]])-1),
                     ele_type = rep(ele_type, times = length(nodes_df[[1]])-1),
                     node1 = seq(0, length(nodes_df[[1]])-2,1),
                     node2 = node1 + 1)
  }
  
  # 2d
  if (length(mesh_dim[mesh_dim == TRUE]) == 2) {
    dims <- which(mesh_dim == TRUE)
    ele_df <- generateRegQuadEle(vector1 = xyz_vector[dims[[1]]],
                                 vector2 = xyz_vector[dims[[2]]])
  }
  
  # 3d
  if (length(mesh_dim[mesh_dim == TRUE]) == 3) {
    ele_df <- generateRegHexEle(x_vector = xyz_vector[[1]], 
                                y_vector = xyz_vector[[2]],
                                z_vector = xyz_vector[[3]])
  }
  
  return(list(NODES = nodes_df, ELEMENTS = ele_df))
}