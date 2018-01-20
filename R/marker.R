# detect_marker

get_marker <-
  function(img, x, y, size, outer_size, percent = "95%", occupancy = .1, show_binary = F){

    trimed_img <-
      trim(img, x, y, size)

    img_bin <-
      trimed_img %>%
        pri::add_dimension() %>%
        imager::threshold(percent) %>%
        # thresh(., w = size * window, h = size * window, offset) %>%
        .[,,,1] %>%
        fillHull

    if(show_binary){
      show(img_bin, browser = T)
    }

    marker_center <-
      img_bin %>%
      bwlabel %>%
      array_branch(margin = 3) %>%
      map(function(img_split){
          marker_detect <-
            computeFeatures.shape(img_split) %>%
            as_tibble %>%
            mutate(row_index = seq_along(s.area), area = s.area,
                   size_for_calc = round(s.radius.mean * .25)) %>%
            arrange(desc(area)) %>%
            filter(area > size^2 * occupancy^2)

          if(nrow(marker_detect) != 1){
            cat(paste0("markers may be splitted. detected ",
                       nrow(marker_detect),
                       " locations in binarized image.\n"))
          }

          computeFeatures.moment(img_split) %>%
            as_tibble %>%
            slice(marker_detect$row_index[1]) %>%
            transmute(xx = round(m.cx), yy = round(m.cy), ss = marker_detect$size_for_calc[1])
    })

    marker_int <-
      trimed_img %>%
      array_branch(margin = 3) %>%
      map2_df(.x = ., .y = marker_center,
           ~ trim2d(.x, .y$xx, .y$yy, .y$ss) %>%
             {tibble(mean = mean(.), median = median(.))})

    # set outer_boundary to calculate pixel values nearby the marker
    null_img <- array(0, dim = dim(trimed_img))
    centers <- round(dim(trimed_img)[1:2] / 2)
    outer_rect <- overdraw(null_img, centers[1], centers[2], outer_size, by = 1)
    non_marker <-
      img_bin %>%
      dilate(makeBrush(size / 5, shape="diamond")) %>%
      fillHull %>%
      `!`
    target_pixel = sum(outer_rect * non_marker)

    # view(outer_rect * non_marker)

    target_int <-
      (trimed_img * outer_rect * non_marker) %>%
      array_branch(margin = 3) %>%
      map_df(function(mtrx){
        mtrx[mtrx > 0] %>%
          {tibble(mean = mean(., na.rm = T), median = median(., na.rm = T))}
      })

    marker_center %>%
      bind_rows %>%
      transmute(x = xx + x - size, y = yy + y - size,
                size_inner = ss, size_outer = outer_size,
                pixel_marker = 4*ss^2, pixel_target = target_pixel) %>%
      list(location = .,
           marker = marker_int,
           target = target_int,
           reflectance = target_int / marker_int) %>%
      return()
  }
