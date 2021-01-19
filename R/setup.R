#' Setup universes
#'
#' List app installations and setup universe repos.
#'
#' @export
setup_universes <- function(){
  all <- ghapps::gh_app_installation_list(app_id = '87942')
  installs <- vapply(all, function(x){x$account$login}, character(1))
  cat("Found installations for:", installs, sep = '\n - ')
  current <- gh::gh('/users/r-universe/repos', per_page = 100, .limit = 1e5)
  universes <- vapply(current, function(x){x$name}, character(1))
  universes <- tolower(universes)
  cat("Found universes for:", universes, sep = '\n - ')
  installs <- tolower(installs)
  newbies <- setdiff(installs, c(universes, skiplist))
  if(!length(newbies)){
    cat("No new installations found.\n")
    return()
  }
  cat("Found NEW installations:", newbies, sep = '\n - ')
  print(gh::gh_whoami())
  lapply(newbies, create_universe_repo)
  invisible()
}

# Ignore these orgs
skiplist <- 'ropenscilabs'

create_universe_repo <- function(owner){
  cat("Setup universe for:", owner, '\n')
  desc <- paste("Source universe for:", owner)
  homepage <- sprintf("https://%s.r-universe.dev", owner)
  gh::gh('/orgs/r-universe/repos', name = owner, description = desc,
         homepage = homepage, private = FALSE, .method = 'POST')
  cat(sprintf("Repo 'r-universe/%s' created! Waiting a few seconds before pushing...\n", owner))
  for(i in 10:1){cat(i, '\n'); Sys.sleep(1)}
  repo <- file.path(tempdir(), paste0(owner, '-universe'))
  remote <- paste0('https://github.com/r-universe/', owner)
  gert::git_clone('https://github.com/r-universe-org/universe-template', path = repo)
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(repo)
  gert::git_remote_add(remote, name = 'universe')
  gert::git_push('universe')
  cat("Done!\n")
}
