moocr_import <- function(rmd = TRUE, workdir = getwd()) {
    
    proceedstatus <- readline("The following procedure to import your data may take a while. Do you want to proceed? (y/n)")
    if (proceedstatus == "y") {
        dirnames <- list.dirs(path = workdir, full.names = FALSE)
        dirnames <- dirnames[-1] # to remove the first empty folder name (check if this line will be an issue in windows)
        
        #This is to check whether the working directory is pointing at the right folder.
        filecheck <- list.files(file.path(workdir, dirnames), full.names = FALSE, recursive = TRUE, include.dirs = FALSE, pattern = "course_branch_grades.csv")
        if (length(filecheck) == 0) {
            stop("Please make sure you have set your working directory to where the Coursera data dump is located.")
        }
        numcourses <<- length(dirnames)
        
        # This creates databases
        createdbname <- function(x) {
            temp1 <- paste("dropdb", x, sep = " ")
            system(temp1)
            temp1 <- paste("createdb", x, sep = " ")
            system(temp1)
        }
        purrr::map(1:numcourses, ~ createdbname(dirnames[[.x]])) 
        
        # Populating databases using setup.sql and load.sql files and the .csv files
        populate <- function(x) {
            setwd(file.path(workdir, x))
            temp <- paste("psql -e -U postgres -d", x, sep = " ")
            temp1 <- paste(temp, "-f setup.sql", sep = " ")
            temp2 <- paste(temp, "-f load.sql", sep = " ")
            system(temp1)
            system(temp2)
        }
        purrr::map(1:numcourses, ~ populate(dirnames[[.x]]))
        setwd(workingdir)
        
        
        # This is for connecting R to the databases
        # First connect to your Postgresql client
        # in terminal run the command createdb mydb, which will create a database named mydb (you can use dropdb mydb to delete a database)
        # Then use psql -e -U postgres -d mydb -f setup.sql (this will setup all the tables included in the course data export.)
        # Then run psql -e -U postgres -d mydb -f load.sql (this will import all csv files into PostgreSQL)
        #' dbListTables(con)
        #' dbListFields(con, 'peer_submissions')
        #db_list_tables(rprogdb)
        #src_tbls(rprogdb)
        
        lapply(dbListConnections(PostgreSQL()), dbDisconnect) # This is for disconnecting all databases since there is a max of 16
        drv <- dbDriver("PostgreSQL")
        courses <- purrr::map(1:numcourses, ~ dbConnect(drv, host='localhost',
                                                        port='5432',
                                                        dbname=dirnames[.x],
                                                        user='postgres',
                                                        password=''))
        
        courses <- unlist(courses, recursive = TRUE, use.names = TRUE)
        # Now you can call it by using course[[1]]
        # e.g. tbl(courses[[1]], "users")
        
        # assign(paste(tablenames[1], "df", sep = "_"), purrr::map(1:numcourses, ~ tbl(courses[[.x]], tablenames[1])))
        tablenames <- dbListTables(courses[[1]])
        # create all the 99 tables
        assignment <- function(y) {
            assign(paste(y, "df", sep = "_"), purrr::map(1:numcourses, ~ tbl(courses[[.x]], y))) 
        }
        all_tables <<- purrr::map(1:length(tablenames), ~ assignment(tablenames[[.x]]))
        coursenames <- purrr::map(1:numcourses, ~ tbl_df(all_tables[[85]][[.x]])$course_name) # Extracts course names, 85 is the number of table associated with table courses
        names(courses) <- coursenames # Assigns course names to the list courses
        names(all_tables) <- tablenames
        
        
        # users <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "users"))
        # demoganswers <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "demographics_answers")) 
        # grades <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "course_grades"))
        # peerreviews <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "peer_reviews"))
        # peersubmissions <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "peer_submissions"))
        # peercomments <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "peer_comments"))
        # skips <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "peer_skips"))
        # membership <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "course_memberships"))
        # course <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "courses"))
        # coursenames <- purrr::map(1:numcourses, ~ tbl_df(course[[.x]])$course_name) # Extracts course names
        # names(courses) <- coursenames # Assigns course names to the list courses
        # comments <- purrr::map(1:numcourses, ~ tbl(courses[[.x]], "peer_comments"))
        
        
        # Since there are duplicates for membership roles (there are rows with the same jhu_user_id but different membership roles), the following lines
        # will calculate the latest membership role and keep that for the jhu_user_id and delete all other rows.
        slicing <- function(x) {
            x <- tbl_df(x) %>%
                dplyr::filter(course_membership_role!="INSTRUCTOR", course_membership_role!="MENTOR") %>%
                dplyr::group_by(jhu_user_id) %>%
                dplyr::slice(which.max(as.Date(course_membership_ts, '%Y/%m/%d')))
        }
        if (rmd == TRUE) {
            membership <- purrr::map(1:numcourses, ~ slicing(alltables[["course_memberships"]][[.x]]))  
        } else {
            print("Warning: There might be duplicate students since each student can take multiple roles", quote = FALSE)
        }
        print(paste0(numcourses, " database(s) created and running:"), quote = FALSE)
        print(paste0(coursenames))
        return(all_tables)
    }
}