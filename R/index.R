index_image <-
  function(path, ...){
    tibble(path = path,
           init_time = basename(path),
           time = file.mtime(path)) %>%
      separate(init_time, c("prefix", "init_time"), sep = "_") %>%
      mutate(init_time = ymd_hms(str_sub(init_time, 1, 17)))
  }

fetch_chains <-
  function(.tbl_gr, num = 5){
    .tbl_gr %>%
      summarise(count = n(), path = head(path, 1)) %>%
      filter(count == num) %>%
      select(-count) %>%
      ungroup
  }

fetch_daybreaks <-
  function(.tbl_gr, daybreak_hour, delta_min = 3, num = 5, .keep_date_hour = T){
    output <-
      .tbl_gr %>%
      mutate(hour = hour(time) + minute(time) / 60 + second(time) / 3600,
             date = lubridate::date(time)) %>%
      filter(between(hour, daybreak_hour - delta_min / 60, daybreak_hour + delta_min / 60, incbounds = T)) %>%
      group_by(date, add = T) %>%
      slice(1:num) %>%
      ungroup

    if(.keep_date_hour){
      return(output)
    } else {
      output %>%
      select(-date, -hour) %>%
      return
    }
  }
