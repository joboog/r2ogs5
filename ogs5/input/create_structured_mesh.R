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

# create 1d df of nodes
create_1d_node_df <- function(base_vector = numeric(), dimension = i){
  
  node_df <- tibble(x = vector(mode = "numeric", length = length(base_vector)),
                    y = vector(mode = "numeric", length = length(base_vector)),
                    z = vector(mode = "numeric", length = length(base_vector)))
  
  node_df[,i] <- base_vector
  return(node_df)
}



# main function
create_structured_mesh_nodes_ele <- 
  
  function(
    origin = c(0,0,0),
    lx = NULL, nx = NULL, sx = NULL,
    ly = NULL, ny = NULL, sy = NULL,
    lz = NULL, nz = NULL, sz = NULL
  ){
  # returns a list with a tibble for nodes and elements
    
  # comp base vectors
  if (!(is.null(lx)) & !(is.null(nx))){
    x_vector <- comp_coordinate_vector(origin = origin, d = 1,
                                       dist = lx, n_ele = nx)
  } 
  
  if (!(is.null(lx)) & !(is.null(sx))){
    x_vector <- comp_coordinate_vector(origin = origin, d = 1,
                                       dist = lx, s_ele = sx)
  } 
  
  if (!(is.null(ly)) & !(is.null(ny))){
    y_vector <- comp_coordinate_vector(origin = origin, d = 2,
                                       dist = ly, n_ele = ny)
  } 
  
  if (!(is.null(ly)) & !(is.null(sy))){
    y_vector <- comp_coordinate_vector(origin = origin, d = 2,
                                       dist = ly, s_ele = sy)
  } 
  
  if (!(is.null(lz)) & !(is.null(nz))){
    z_vector <- comp_coordinate_vector(origin = origin, d = 3,
                                       dist = lz, n_ele = nz)
  } 
  
  if (!(is.null(lz)) & !(is.null(sz))){
    z_vector <- comp_coordinate_vector(origin = origin, d = 3,
                                       dist = lz, s_ele = sz)
  } 
  
  # check dimension
  mesh_dim <- c(FALSE, FALSE, FALSE)
  
  if (exists("x_vector")){
    mesh_dim[1] = TRUE 
  }
  
  if (exists("y_vector")){
    mesh_dim[2] = TRUE 
  }
  
  if (exists("z_vector")){
    mesh_dim[3] = TRUE 
  }
  
  # create 1d line mesh -----------------------------------------------------
  if (length(mesh_dim[mesh_dim==TRUE]) == 1) {
    # call fct to compute line mesh
    for (i in 1:3){
      if (mesh_dim[i] == TRUE) break
    }
    
    # create node_df
    if (i == 1) node_df <- create_1d_node_df(base_vector = x_vector, dimension = i)
    if (i == 2) node_df <- create_1d_node_df(base_vector = y_vector, dimension = i)
    if (i == 3) node_df <- create_1d_node_df(base_vector = z_vector, dimension = i)
    
    # create element_df
    ele_df <- tibble(material_id = vector("numeric", length(node_df[[1]])-1),
                     ele_type = rep("line", times = length(node_df[[1]])-1),
                     node1 = seq(0, length(node_df[[1]])-2,1),
                     node2 = node1 + 1)
  }
  
  return(list(NODES = node_df, ELEMENTS = ele_df))
}



