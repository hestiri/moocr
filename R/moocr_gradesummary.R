# This renders a table showing average course grade of male/female individuals

moocr_gradesummary <- function(groupby = "gender") {
    message("Note that maximum grade possible is 1.")
    
    grading <- function(x, y, z) {
        temp <- z %>% 
            dplyr::left_join(x, by="jhu_user_id", `copy`=TRUE) %>%
            dplyr::left_join(y, by="jhu_user_id", `copy`=TRUE) %>%
            dplyr::filter(!is.na(course_grade_overall))
        if (groupby == "gender"){
            temp %>%
                dplyr::filter(!is.na(reported_or_inferred_gender)) %>% 
                dplyr::group_by(reported_or_inferred_gender) %>% 
                dplyr::summarise(average=mean(course_grade_overall)) %>% 
                dplyr::collect(nrows=Inf)
        } else if (groupby == "education"){
            temp %>%
                dplyr::filter(!is.na(educational_attainment)) %>% 
                dplyr::group_by(educational_attainment) %>% 
                dplyr::summarise(average=mean(course_grade_overall)) %>% 
                dplyr::collect(n=Inf)
        } else if (groupby == "stustatus") {
            temp %>%
                dplyr::filter(!is.na(student_status)) %>% 
                dplyr::group_by(student_status) %>% 
                dplyr::summarise(average=mean(course_grade_overall)) %>% 
                dplyr::collect(n=Inf)
        } else if (groupby == "empstatus") {
            temp %>%
                dplyr::filter(!is.na(employment_status)) %>% 
                dplyr::group_by(employment_status) %>% 
                dplyr::summarise(average=mean(course_grade_overall)) %>% 
                dplyr::collect(n=Inf)
        } else if (groupby == "country") {
            temp %>%
                dplyr::filter(!is.na(country_cd)) %>% 
                dplyr::group_by(country_cd) %>% 
                dplyr::summarise(average=mean(course_grade_overall)) %>% 
                dplyr::collect(n=Inf)
        } else {
            message("Please enter a valid value for 'groupby' attribute. Possible values are gender, education, stustatus, empstatus, and country.")
        }
    }
    
    gradetable <- purrr::map(1:numcourses, ~ grading(all_tables[["course_memberships"]][[.x]], all_tables[["course_grades"]][[.x]], all_tables[["users"]][[.x]]))
    names(gradetable) <- coursenames  
    return(gradetable)
}
