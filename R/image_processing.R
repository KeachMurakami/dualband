show <- pri::view

# image processing

grids <-
  function(x_cent, y_cent, size){
    x <- (x_cent-size):(x_cent+size)
    y <- (y_cent-size):(y_cent+size)
    return(list(x, y))
  }

trim <-
  function(img, x_cent, y_cent, size){
    dims <- dim(img)
    grid <- grids(x_cent, y_cent, size)
    xxx <- grid[[1]][between(grid[[1]], 1, dims[1])]
    yyy <- grid[[2]][between(grid[[2]], 1, dims[2])]
    img[xxx, yyy, , drop = F]
  }

trim2d <-
  function(img_2d, x_cent, y_cent, size){
    dims <- dim(img_2d)
    grid <- grids(x_cent, y_cent, size)
    xxx <- grid[[1]][between(grid[[1]], 1, dims[1])]
    yyy <- grid[[2]][between(grid[[2]], 1, dims[2])]
    img_2d[xxx, yyy]
  }

overdraw <-
  function(img, x_cent, y_cent, size, by = 0){
    dims <- dim(img)
    grid <- grids(x_cent, y_cent, size)
    xxx <- grid[[1]][between(grid[[1]], 1, dims[1])]
    yyy <- grid[[2]][between(grid[[2]], 1, dims[2])]
    img[xxx, yyy, ] <- by
    return(img)
  }


stack <-
  function(list_of_matrix_2d){
    list_of_matrix_2d %>%
      abind::abind()
  }

unstack <-
  function(array_3d){
    array_3d %>%
      array_branch(3)
  }

add_dim <-
  function(array){
    dim(array) <- c(dim(array), 1)
    return(array)
  }
