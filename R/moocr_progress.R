
moocr_progress <- function(){
    proceedstatus <- readline("The following procedure to import your data may take a while since the table course_progress is likely to be large. Do you want to proceed? (y/n): ")
    if (proceedstatus == "y") {
        progress <- function(x, y){
            
            temp <- x %>% 
                dplyr::filter(!is.na(course_progress_ts)) %>% 
                dplyr::group_by(jhu_user_id) %>%
                dplyr::filter(course_progress_ts == max(course_progress_ts)) %>% # only to record the last activity
                dplyr::left_join(y, by = "course_item_id", `copy`=TRUE) %>% # so that we know the name of the course item
                dplyr::filter(!is.na(course_item_name)) %>% 
                dplyr::group_by(course_item_order) %>% 
                dplyr::summarise(Total=n()) %>% 
                dplyr::arrange(desc(Total)) %>% 
                dplyr::mutate(Share=round(Total/sum(Total), 2))
        }
        
        last_activity <- purrr::map(1:numcourses, ~ progress(all_tables[["course_progress"]][[.x]], all_tables[["course_items"]][[.x]]))
        names(last_activity) <- coursenames  
        return(last_activity) 
    }
}
