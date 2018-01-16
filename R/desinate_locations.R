# designate the locations in image

assign_xy <-
  function(path, interactive = F, default_size = 10, ...){
    path %>%
      read_img %>%
      show(browser = F)

    results <-
      locator() %>%
      map_df(round) %>%
      mutate(path = path, size = default_size, ...)

    while(interactive == T){
      key_name <-
        readline("name merge key (e.g. leaf_id) \n") %>%
        rlang::sym()
      key_value <-
        readline("set merge key values (e.g. c(1, 5:7, 10)) \n") %>%
        parse(text = .) %>%
        eval

      print(results)

      if(length(key_value) == nrow(results)){
        results <-
          results %>%
          dplyr::mutate(rlang::UQ(key_name) := key_value)
        if(readline("Finish?: Y/n \n") == "Y") interactive <- F
      } else {
        cat("length(key values) != nrow(points). Retry. \n")
      }
    }
    return(results)
  }

assign_location <-
  function(file, .suffix = c("_leaf", "_ref")){
    cat("set references: add link (merge key)\n")
    ref_dat <- assign_xy(file, interactive = T)
    check_xy(ref_dat, x, y, size, by = 0)

    cat("set targets: add link (merge key) and id\n")
    leaf_dat <- assign_xy(file, interactive = T)
    check_xy(leaf_dat, x, y, size, by = 0)

    ref_dat %>%
      select(x, y, size, id) %>%
      left_join(leaf_dat, ., by = "id", suffix = .suffix)
  }



check_xy <-
  function(.tbl, x = x, y = y, size = size, by = 0, .silent = F, ...){
    x <- rlang::quo(x)
    y <- rlang::quo(y)
    size <- rlang::quo(size)

    x_i <- pull(.tbl, rlang::UQ(x))
    y_i <- pull(.tbl, rlang::UQ(y))
    size_in <- pull(.tbl, rlang::UQ(size))[1]

    img <-
      .tbl$path[1] %>%
      read_img

    size_ <-
      if_else(is.null(size_in), 5, as.double(size_in))
    for(i in seq_along(.tbl$path)){
      img <- overdraw(img, x_i[i], y_i[i], size_, by)
    }
    if(.silent){
      return(img)
    } else {
      show(img, ...)
    }
  }

check_leaf_xy <-
  function(.tbl, by, ...){
    check_xy(.tbl, x_leaf, y_leaf, size_leaf, by, ...)
  }

check_grey_xy <-
  function(.tbl, by, ...){
    check_xy(.tbl, x_grey, y_grey, size_grey, by, ...)
  }



save_location <-
  function(.tbl, out_dir, ...){
    out_path <- paste0(out_dir, "/", basename(.tbl$path[1]))
    .tbl %>%
      readr::write_csv(., path = out_path, ...)
  }




check_centers <-
  function(.tbl, window = 5){
    img <-
      .tbl$filepath[1L] %>%
      read_img

    zero_img <-
      function(x, y, window){
        img <- array(0, dim = dim(img))
        img[(x-window):(x+window), (y-window):(y+window),] <- 1
        return(img)
      }

    centers_leaf <-
      .tbl %>%
      split(.$location_id) %>%
      map2(.x = ., .y = window, ~ zero_img(.x$x_leaf, .x$y_leaf, .y)) %>%
      Reduce(f = `+`, x = .)

    centers_grey <-
      .tbl %>%
      split(.$location_id) %>%
      map2(.x = ., .y = window, ~ zero_img(.x$x_grey, .x$y_grey, .y)) %>%
      Reduce(f = `+`, x = .)

    img <-
      abind::abind(img, img)
    indicates <-
      abind::abind(centers_leaf, centers_grey)
    return(img + indicates)
  }

pick_centers <-
  function(.tbl, window_leaf = 5, window_grey = 5, ...){

    .tbl %>%
      distinct(!!!quos(...))

    img <-
      .tbl$filepath[1L] %>%
      read_img

    picks <-
      function(x, y, window){
        img[(x-window):(x+window), (y-window):(y+window),,drop = F]^3 %>%
        {tibble(mean = apply(., MARGIN = 3, FUN = mean),
                median = apply(., MARGIN = 3, FUN = median),
                sd = apply(., MARGIN = 3, FUN = sd))} %>%
          mutate(pixel = 4*window^2)
      }

    values_leaf <-
      .tbl %>%
      split(.$location_id) %>%
      map2_df(.x = ., .y = window_leaf, ~ picks(.x$x_leaf, .x$y_leaf, .y) %>% mutate(grey_id = .x$grey_id[1]), .id = "location_id")

    values_grey <-
      .tbl %>%
      distinct(grey_id, .keep_all = T) %>%
      split(.$grey_id) %>%
      map2_df(.x = ., .y = window_grey, ~ picks(.x$x_grey, .x$y_grey, .y), .id = "grey_id") %>%
      mutate(grey_id = as.numeric(grey_id))

    results <-
      left_join(values_leaf, values_grey, by = "grey_id", suffix = c("_leaf", "_grey")) %>%
      mutate(r_mean = mean_leaf / mean_grey,
             r_median = median_leaf / median_grey)

    .tbl %>%
      distinct(!!!quos(...)) %>%
      cbind(results, .)

  }

