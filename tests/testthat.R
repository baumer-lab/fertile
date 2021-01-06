
library(testthat)
library(fertile)

Sys.setenv("IN_TESTTHAT" = TRUE)

test_check("fertile")


proj_dirs <- c('project_miceps', 'project_noob',
               'project_random', 'project_randomseed', 'project_comments')

for (dir in proj_dirs){
  if(fs::dir_exists(dir)){
    fs::dir_delete(dir)
  }
}

Sys.setenv("IN_TESTTHAT" = FALSE)
