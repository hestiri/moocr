
moocr_progress <- function(){
    
    progress <- function(x, y){
        
        temp <- x %>% 
            dplyr::filter(!is.na(course_progress_ts)) %>% 
            dplyr::group_by(jhu_user_id) %>%
            dplyr::filter(course_progress_ts == max(course_progress_ts)) %>% # only to record the last activity
            dplyr::left_join(y, by = "course_item_id", `copy`=TRUE) %>% # so that we know the name of the course item
            dplyr::filter(!is.na(course_item_name))
        
        prop.table(table(temp$course_item_order))
    }
    
last_activity <- purrr::map(1:numcourses, ~ progress(all_tables[["course_progress"]][[.x]], all_tables[["course_items"]][[.x]]))
names(last_activity) <- coursenames  
return(last_activity)    
}


temp <- all_tables[["course_progress"]][[1]] %>% 
    dplyr::filter(!is.na(course_progress_ts)) %>% 
    dplyr::group_by(jhu_user_id) %>%
    dplyr::filter(course_progress_ts == max(course_progress_ts)) %>% # only to record the last activity
    dplyr::left_join(all_tables[["course_items"]][[1]], by = "course_item_id", `copy`=TRUE) %>% # so that we know the name of the course item
    dplyr::filter(!is.na(course_item_name)) %>% 
    dplyr::count(course_item_order)


temp <- tbl_df(temp)
prop.table(table(temp$course_item_order))
