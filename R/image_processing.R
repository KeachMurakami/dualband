show <- pri::view

# image processing

trim <-
  function(img, x_cent, y_cent, size){
    img[(x_cent-size):(x_cent+size), (y_cent-size):(y_cent+size), , drop = F]
  }

trim2d <-
  function(img_2d, x_cent, y_cent, size){
    img_2d[(x_cent-size):(x_cent+size), (y_cent-size):(y_cent+size)]
  }

overdraw <-
  function(img, x_cent, y_cent, size, by = 0){
    img[(x_cent-size):(x_cent+size), (y_cent-size):(y_cent+size), ] <- by
    return(img)
  }

