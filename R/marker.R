# detect_marker

get_marker <-
  function(img, x, y, size, outer_size, percent = "95%", occupancy = .1, show_mapping = F){

    trimed_img <-
      trim(img, x, y, size)

    img_bin <-
      trimed_img %>%
        pri::add_dimension() %>%
        imager::threshold(percent) %>%
        # thresh(., w = size * window, h = size * window, offset) %>%
        .[,,,1] %>%
        EBImage::fillHull()

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
    outer_rect <-
      null_img %>%
      array_branch(margin = 3) %>%
      map2(.x = ., .y = marker_center,
              ~ pri::add_dimension(.x) %>%
              overdraw(.y$xx, .y$yy, outer_size, 1)) %>%
      abind::abind()

    dilate_mask_size <-
      round(size / 10) %>% {if_else(. %% 2 == 0, . + 1, .)} # to get odd number for the mask
    non_marker <-
      img_bin %>%
      EBImage::dilate(EBImage::makeBrush(dilate_mask_size, shape="diamond")) %>%
      EBImage::fillHull() %>%
      `!`
    target_pixel = sum(outer_rect * non_marker)

    target_int <-
      (trimed_img * outer_rect * non_marker) %>%
      array_branch(margin = 3) %>%
      map_df(function(mtrx){
        mtrx[mtrx > 0] %>%
          {tibble(mean = mean(., na.rm = T), median = median(., na.rm = T))}
      })


    if(show_mapping){
      library(imager)

      ref_map <-
        array(0, dim = dim(trimed_img)) %>%
        array_branch(margin = 3) %>%
        map2(.x = ., .y = marker_center,
             ~ pri::add_dimension(.x) %>%
             overdraw(.y$xx, .y$yy, .y$ss, by = 1)) %>%
        abind::abind()

      target_map <-
        outer_rect * non_marker

      overlayed <-
        trimed_img -  ref_map * .5 - target_map * .5

      show(overlayed)
    }


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


marker_data <-
  function(index, loc_sw, loc_lw, gamma, img_sw = NULL, img_lw = NULL, .prefixes = c("530", "570"), ...){
    index_sw <- index %>% filter(prefix == .prefixes[1])
    index_lw <- index %>% filter(prefix == .prefixes[2])

    if(is.null(img_sw)){
      img_sw <- index_sw$path %>% read_imgs %>% `^`(., gamma)
    }
    if(is.null(img_lw)){
      img_lw <- index_lw$path %>% read_imgs %>% `^`(., gamma)
    }

    duplicate_flag_sw <- F
    duplicate_flag_lw <- F

    if(length(dim(img_sw)) == 2){
      duplicate_flag_sw <- T
      img_sw <-
        img_sw %>%
        pri::add_dimension() %>%
        {list(., .)} %>%
        abind::abind()
    }
    if(length(dim(img_lw)) == 2){
      duplicate_flag_lw <- T
      img_lw <-
        img_lw %>%
        pri::add_dimension() %>%
        {list(., .)} %>%
        abind::abind()
    }

    dat_sw <-
      loc_sw %>%
      split(.$location) %>%
      markers(img_sw, ., ...)

    dat_lw <-
      loc_lw %>%
      split(.$location) %>%
      markers(img_lw, ., ...)

    if(duplicate_flag_sw){
      dat_sw <-
        dat_sw %>%
        map(~ map(., ~ slice(., 1)))
    }
    if(duplicate_flag_lw){
      dat_lw <-
        dat_lw %>%
        map(~ map(., ~ slice(., 1)))
    }

    list(short = dat_sw, long = dat_lw,
         short_index = select(index_sw, group_vars(index)),
         long_index = select(index_lw, group_vars(index)))
  }

markers <-
  function(imgs, list_locations, ...){
    list_locations %>%
      map(function(batch){
        print(batch$location)
        get_marker(imgs, x = batch$x, y = batch$y, size = batch$size, ...)
      }
      )
  }


marker2pri <-
  function(list_marker_data){
    info <-
      list_marker_data[3:4] %>%
      map(~ungroup(.) %>% select(-prefix))

    if(identical(info[[1]], info[[2]])){
      add_info <- mutate(info[[1]], frame = 1:dim(info[[1]])[1])
    } else {
      stop("two band images are not paired appropriately")
    }

    map2_df(list_marker_data[[1]], list_marker_data[[2]],
            ~ calc_pri(.x$reflectance, .y$reflectance) %>%
              mutate(frame = seq_along(mean)), .id = "location") %>%
      left_join(., add_info, by = "frame")
  }
