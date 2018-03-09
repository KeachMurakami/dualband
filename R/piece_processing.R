# detecting, extracting, and summarising marker consists of grey-white plate

get_marker_geom <-
  function(){}

set_marker_geom <-
  function(){}


divide_piece <-
  function(location, img){
    if(nrow(location) != 1){
      stop("Error in divide_piece: nrow(location) != 1.\n `locations` should be split")
    }
    piece <- trim(img, x = location$x, y = location$y, size = location$size)

    # add geometrical attributes so that the pieces are able to be restored
    attributes(piece)$geometry <-
      lst(location, original_dim = dim(img))
    # add information required for binarization
    attributes(piece)$binarise <-
      lst(white_ratio = .95,
          morph_size = 1,
          occupancy = .001)
    # add information required for extraction
    attributes(piece)$center <- NULL

    # add information required for extraction
    attributes(piece)$extract <-
      lst(ref_white_ratio = 1/3,
          margin_white_ratio = 1.2,
          outer_white_ratio = 2)

    return(piece)
  }


set_center <-
  function(img, .check = F, white_ratio = NULL, occupancy = NULL, morph_size = NULL, ..., .verbose = F){
    if(is.null(white_ratio)){
      white_ratio <- attributes(img)$binarise$white_ratio
    } else {
      attributes(img)$binarise$white_ratio <- white_ratio
    }

    if(is.null(morph_size)){
      morph_size <- attributes(img)$binarise$morph_size
    } else {
      attributes(img)$binarise$morph_size <- morph_size
    }

    if(is.null(occupancy)){
      occupancy <- attributes(img)$binarise$occupancy
    } else {
      attributes(img)$binarise$occupancy <- occupancy
    }

    img_bw <-
      img %>%
      binarise(., wr = white_ratio, mrp = morph_size)

    if(.check){
      show(img_bw, ...)
      return(NULL)
    }

    centers <-
      img_bw %>%
      unstack %>%
      map(~ set_bw_center(., ocp = occupancy, .verbose = .verbose))

    attributes(img)$center <- centers

    return(img)
  }



binarise <-
  function(img_, wr, mrp){
    if(length(dim(img_)) == 2){
      img_ <- add_dim(img_)
    }

    img_ %>%
      add_dim %>%
      imager::threshold(paste0(wr * 100, "%")) %>%
      .[,,,1] %>%
      EBImage::dilate(kern = EBImage::makeBrush(mrp, shape = "disc")) %>%
      `*`(., 1) %>%
      EBImage::fillHull() %>%
      EBImage::erode(kern = EBImage::makeBrush(mrp, shape = "disc")) %>%
      EBImage::bwlabel() %>%
      return()
  }

set_bw_center <-
  function(mtrx_bw, ocp, .verbose){
    computed_shape_info <-
      mtrx_bw %>%
      EBImage::computeFeatures.shape() %>%
      as_tibble

    if(nrow(computed_shape_info) == 0){
      stop("Error in detecting marker-center: no white area was found")
    }

    white_areas <-
      computed_shape_info %>%
      mutate(row_index = seq_along(s.area),
             expected_size = dim(mtrx_bw)[1] * dim(mtrx_bw)[2] * ocp) %>%
      filter(s.area > expected_size) %>%
      arrange(desc(s.area))

    if(nrow(white_areas) > 1 & .verbose){
      warning("Warning in detecting marker-center: several white areas were detected")
    }

    computed_moment_info <-
      EBImage::computeFeatures.moment(mtrx_bw) %>%
      as_tibble

    row_index_of_maximum_area <- white_areas$row_index[1]
    radius_of_maximum_area <- white_areas$s.radius.mean[1]

    computed_moment_info[row_index_of_maximum_area,] %>%
      transmute(x = round(m.cx), y = round(m.cy), radius_white = radius_of_maximum_area) %>%
      return()
  }










set_masks <-
  function(img, .check = T, ref_white_ratio = NULL, margin_white_ratio = NULL, outer_white_ratio = NULL, ..., .verbose = F){
    if(is.null(ref_white_ratio)){
      ref_white_ratio <- attributes(img)$extract$ref_white_ratio
    } else {
      attributes(img)$extract$ref_white_ratio <- ref_white_ratio
    }

    if(is.null(margin_white_ratio)){
      margin_white_ratio <- attributes(img)$extract$margin_white_ratio
    } else {
      attributes(img)$extract$margin_white_ratio <- margin_white_ratio
    }

    if(is.null(outer_white_ratio)){
      outer_white_ratio <- attributes(img)$extract$outer_white_ratio
    } else {
      attributes(img)$extract$outer_white_ratio <- outer_white_ratio
    }

    if(.check){
      check_mask(img) %>% show(...)
      return(NULL)
    }

    return(img)
  }

mask_marker <-
  function(img_attr, ref_wht, .fill = 0){
    out_dim <- c(dim(img_attr)[1:2], 1)
    attributes(img_attr)$center %>%
      map(function(center_z){
        array(.fill, dim = out_dim) %>%
          overdraw(., x_cent = center_z$x, y_cent = center_z$y,
                   size = center_z$radius_white * ref_wht, by = 1)
      }) %>%
      stack
  }

mask_target <-
  function(img_attr, mar_wht, out_wht, .fill = 0){
    out_dim <- c(dim(img_attr)[1:2], 1)
    attributes(img_attr)$center %>%
      map(function(center_z){
        array(.fill, dim = out_dim) %>%
          overdraw(., x_cent = center_z$x, y_cent = center_z$y,
                   size = center_z$radius_white * out_wht, by = 1) %>%
          overdraw(.,x_cent = center_z$x, y_cent = center_z$y,
                   size = center_z$radius_white * mar_wht, by = .fill)
      }) %>%
      stack
  }

check_mask <-
  function(img_attr, map_rgb = c(marker = 1, target = 3), map_int = c(marker = 1, target = 1)){

    out_img <-
      EBImage::Image(img_attr) %>%
      EBImage::channel("rgb")

    marker <- extract_marker(img_attr, .fill = 0)
    target <- extract_target(img_attr, .fill = 0)

    out_img[,,map_rgb[1],] <- out_img[,,map_rgb[1],] + marker * map_int[1]
    out_img[,,map_rgb[2],] <- out_img[,,map_rgb[2],] + target * map_int[2]

    attributes(out_img)$geometry <- attributes(img_attr)$geometry
    return(out_img)
  }

extract_reflectance <-
  function(img_attr){
    marker <-
      extract_marker(img_attr, NA) %>%
      plyr::adply(.margins = 3, .fun = c(median_marker = median, mean_marker = mean), na.rm = T, .id = "file")
    target <-
      extract_target(img_attr, NA) %>%
      plyr::adply(.margins = 3, .fun = c(median_target = median, mean_target = mean), na.rm = T, .id = "file")


    inner_join(marker, target, by = c("file")) %>%
      mutate(median_refl = median_target / median_marker,
             mean_refl = mean_target / mean_marker,
             z = seq_along(file))
  }

extract_marker <-
  function(img_attr, .fill = NA){
    extract_prm <- attributes(img_attr)$extract

    img_attr * mask_marker(img_attr, extract_prm$ref_white_ratio, .fill)
  }

extract_target <-
  function(img_attr, .fill = NA){
    extract_prm <- attributes(img_attr)$extract

    img_attr * mask_target(img_attr, extract_prm$margin_white_ratio, extract_prm$outer_white_ratio, .fill)
  }

map_full <-
  function(list_pieces, full_size_img){

    range_full <- dim(full_size)[1:2]

    result <-
      EBImage::Image(full_size_img) %>%
      EBImage::channel("rgb")

    for(i in seq_along(list_pieces)){
      piece <- list_pieces[[i]]
      piece_location <- attributes(piece)$geometry$location

      x_range <-
        piece_location %>%
        {(.$x - .$size):(.$x + .$size)}
      y_range <-
        piece_location %>%
        {(.$y - .$size):(.$y + .$size)}

      x_range <- x_range[between(x_range, 1, range_full[1])]
      y_range <- y_range[between(y_range, 1, range_full[1])]

      result[x_range, y_range, ,] <- piece
    }

    return(result)
  }


### deal with attributes

set_attr <-
  function(attr, img){
    attributes(img)$geometry <- attr$geometry
    attributes(img)$binarise <- attr$binarise
    attributes(img)$extract <- attr$extract
    attributes(img)$center <- attr$center

    return(img)
  }
