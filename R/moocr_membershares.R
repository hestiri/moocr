moocr_membershares <- function() {
     # This renders a table that shows the share of people in each membership categories
     membershares <- function(x) {
         temp <- x %>%
             dplyr::filter(!is.na(course_membership_role)) %>% 
             dplyr::group_by(course_membership_role) %>% 
             dplyr::summarise(nrows=n()) %>% 
             dplyr::mutate(freq=nrows/sum(nrows)) 
     }
     membertable <- purrr::map(1:numcourses, ~ membershares(alltables[["course_memberships"]][[.x]]))
     names(membertable) <- coursenames
     return(membertable)
     #' ggplot(temp, aes(course_membership_role)) + geom_bar(aes(weight = freq)) 
 }

