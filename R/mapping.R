extract_marker <-
  function(array_piece, white_ratio = .95, occupancy = .001, erode_size = 1, .show = F,
           white_ref_ratio = 3, margin_white_ratio = 1.2, outer_white_ratio = 2, .verbose = F){
    centers <-
      array_piece %>%
      binarize(., white_ratio) %>%
      piece2center(., occupancy, erode_size, .show, .verbose)

    marker <-
      array_piece * mask_marker(centers, white_ref_ratio, .fill = NA)
    target <-
      array_piece * mask_target(centers, margin_white_ratio, outer_white_ratio, .fill = NA)

    centers %>%
      bind_rows %>%
      mutate(z = seq_along(x)) %>%
      list(marker = marker, target = target, center = .)
  }

summarise_marker <-
  function(per_marker){
    temp <-
      per_marker[1:2] %>%
      map(~ plyr::adply(., .margins = 3, .fun = c(median = median, mean = mean), na.rm = T, .id = "file") %>%
            mutate(z = seq_along(file)))

    inner_join(temp$marker, temp$target, by = c("file", "z"), suffix = c("_marker", "_target")) %>%
      mutate(median_reflectance = median_target / median_marker,
             mean_reflectance = mean_target / mean_marker) %>%
      select(file, z, everything())
  }


img2piece <-
  function(img, location){
    if(nrow(location) != 1){
      stop("Error in img2piece. nrow(location) != 1.")
    }

    piece <- trim(img, x = location$x, y = location$y, size = location$size)
    attributes(piece)$originals <- location
    return(piece)
  }

binarize <-
  function(img, white_ratio = .95){
    if(length(dim(img)) == 2){
      img <- add_dim(img)
    }
    img %>%
      add_dim %>%
      imager::threshold(paste0(white_ratio * 100, "%")) %>%
      # thresh(., w = size * window, h = size * window, offset) %>%
      .[,,,1] %>%
      EBImage::fillHull()
  }

check_pieces <-
  function(img, white_ratio = .95, occupancy = .001, erode_size = 1, .show = F, .verbose = F){
    img_2d_bw <-
      binarize(img, white_ratio) %>%
      EBImage::dilate(kern = EBImage::makeBrush(erode_size, shape = "disc")) %>%
      EBImage::bwlabel()

    view(img - (!img_2d_bw) * .5)
  }

piece2center <-
  function(img_bw, occupancy = .001, erode_size = 1, .show = F, .verbose = F){
    img_2d_bw <-
      img_bw %>%
      EBImage::dilate(kern = EBImage::makeBrush(erode_size, shape = "disc")) %>%
      EBImage::bwlabel() %>%
      unstack

    centers <-
      img_2d_bw %>%
      map(function(img_mtrx){
        computed_shape_info <-
          img_mtrx %>%
          EBImage::computeFeatures.shape() %>%
          as_tibble

        if(nrow(computed_shape_info) == 0){
          stop("Error in detecting marker-center: no white area was found")
        }

        marker_detected <-
          computed_shape_info %>%
          mutate(row_index = seq_along(s.area),
                 expected_size = dim(img_bw)[1] * dim(img_bw)[2] * occupancy) %>%
          filter(s.area > expected_size) %>%
          arrange(desc(s.area))

        if(nrow(marker_detected) != 1 & .show){
          cat(paste0("markers may be splitted. detected ",
                     nrow(marker_detected),
                     " locations in binarized image.\n"))

          view(img_3d_bw)
          print(select(marker_detected, row_index, area, expected_size))
        }

        computed_moment_info <-
          EBImage::computeFeatures.moment(img_mtrx) %>%
          as_tibble

        row_index_of_maximum_area <- marker_detected$row_index[1]
        radius_of_maximum_area <- marker_detected$s.radius.mean[1]

        computed_moment_info[row_index_of_maximum_area,] %>%
          transmute(x = round(m.cx), y = round(m.cy), radius_white = radius_of_maximum_area, erode = erode_size)
      })

    attributes(centers)$piece_dim <- dim(img_bw)
    return(centers)
  }


mask_marker <-
  function(center, white_ref_ratio = 3, .fill = 0){
    out_dim <- c(attributes(center)$piece_dim[1:2], 1)
    center %>%
      map(function(center_z){
        overdraw(array(.fill, dim = out_dim), x_cent = center_z$x, y_cent = center_z$y,
                 size = center_z$radius_white / white_ref_ratio, by = 1)
        }) %>%
      stack
  }

mask_target <-
  function(center, margin_white_ratio = 1.2, outer_white_ratio = 2, .fill = 0){
    out_dim <- c(attributes(center)$piece_dim[1:2], 1)
    center %>%
      map(function(center_z){
        overdraw(array(.fill, dim = out_dim), x_cent = center_z$x, y_cent = center_z$y,
                 size = center_z$radius_white * outer_white_ratio, by = 1) %>%
        overdraw(x_cent = center_z$x, y_cent = center_z$y,
                 size = center_z$radius_white * margin_white_ratio, by = .fill)
      }) %>%
      stack
  }



#### False color mapping for visualization of the selected regions

piece_false_color <-
  function(img_raw, mask1, mask2, int_mask1 = 1, int_mask2 = 1){
    dims <- dim(img_raw)

    out_img <-
      EBImage::Image(img_raw) %>%
      EBImage::channel("rgb")

    out_img[,,1,] <- out_img[,,1,] + (mask1) * int_mask1
    out_img[,,3,] <- out_img[,,3,] + (mask2) * int_mask2

    attributes(out_img)$originals <- attributes(img_raw)$originals
    return(out_img)
  }

img_false_color <-
  function(img, list_pieces){

    result <-
      EBImage::Image(img) %>%
      EBImage::channel("rgb")

    for(i in seq_along(list_pieces)){
      piece <- list_pieces[[i]]
      piece_location <- attributes(piece)$originals

      x_range <-
        piece_location %>%
        {(.$x - .$size):(.$x + .$size)}
      y_range <-
        piece_location %>%
        {(.$y - .$size):(.$y + .$size)}

      result[x_range, y_range, ,] <- piece
      # result[x_range, y_range, 3,] <- piece[,, 3, ]
    }

    return(result)
  }
