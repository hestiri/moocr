
moocr_progress <- function(){
    
    progress <- function(x, y){
        
        temp <- tbl_df(x) %>% 
            dplyr::group_by(jhu_user_id) %>%
            dplyr::filter(!is.na(course_progress_ts))
            dplyr::slice(which.max(as.Date(course_progress_ts, '%Y/%m/%d'))) %>% # only to record the last activity
            dplyr::left_join(tbl_df(y), by = "course_item_id") %>% # so that we know the name of the course item
            dplyr::filter(!is.na(course_item_name))
        
        prop.table(table(temp$course_item_order))
    }
    
progress <- purrr::map(1:numcourses, ~ progress(all_tables[["course_progress"]][[.x]], all_tables[["course_items"]][[.x]]))
names(progress) <- coursenames  
return(progress)    
}
