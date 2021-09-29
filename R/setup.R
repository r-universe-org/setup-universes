#' Setup universes
#'
#' List app installations and setup universe repos.
#'
#' @rdname setup_universes
#' @export
setup_universes <- function(){
  installs <- list_app_installations()
  cat("Found installations for:", installs, sep = '\n - ')
  universes <- list_universes()
  cat("Found universes for:", universes, sep = '\n - ')
  installs <- tolower(installs)
  newbies <- setdiff(installs, c(universes, skiplist))
  if(!length(newbies)){
    cat("No NEW installations found.\n")
  } else {
    cat("Found NEW installations:", newbies, sep = '\n - ')
    print(gh::gh_whoami())
    lapply(newbies, create_universe_repo)
  }
  deleted <- setdiff(universes, c(installs, testusers))
  if(length(deleted)){
    cat("Found DELETED installations:", deleted, sep = '\n - ')
    if(length(deleted) > 20){
      cat("This number looks too large. Not deleting anything.\n")
      stop("Failed to list app installations?")
    } else if(length(deleted) > 5) {
      cat("This number looks a bit large. Only deleting empty universes.\n")
      lapply(deleted, delete_universe_repo, only_if_empty = TRUE)
    } else {
      lapply(deleted, delete_universe_repo, only_if_empty = FALSE)
    }
  }
  delete_empty_universes()
  invisible()
}

list_app_installations <- function(){
  all <- ghapps::gh_app_installation_list(app_id = '87942')
  tolower(vapply(all, function(x){x$account$login}, character(1)))
}

list_universes <- function(){
  current <- gh::gh('/users/r-universe/repos', per_page = 100, .limit = 1e5)
  tolower(vapply(current, function(x){x$name}, character(1)))
}

# Ignore these orgs
skiplist <- 'ropenscilabs'
testusers <- c("azure", "bioconductor", "cboettig", "eddelbuettel", "hadley",
               "hrbrmstr", "karthik", "mmaechler", "r-music", "rcppcore", "richfitz",
               "rladies", "sckott", "statnet", "thomasp85", "tidymodels", "tidyverse",
               "yihui", "test")

#' @export
#' @rdname setup_universes
#' @param owner create universe for this github account
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

#' @export
#' @rdname setup_universes
#' @param only_if_empty only delete the universe if there are no deployed packages
delete_universe_repo <- function(owner, only_if_empty = FALSE){
  if(only_if_empty){
    url <- sprintf('https://%s.r-universe.dev/packages', owner)
    pkgs <- jsonlite::fromJSON(url)
    if(length(pkgs)){
      cat(sprintf("Skipping universe '%s' which contains packages: %s\n", owner, paste(pkgs, collapse = ', ')))
      return(invisible())
    }
  }
  cat("Deleting universe for:", owner, '\n')
  gh::gh(paste0('/repos/r-universe/', owner), .method = 'DELETE')
}

#' @export
#' @rdname setup_universes
find_stale_universes <- function(){
  #TODO we should delete stale *installations*
  universes <- find_universes()
  stats <- jsonlite::stream_in(url('https://r-universe.dev/stats/organizations'), verbose = FALSE)
  setdiff(universes, stats$organization)
}

#' @export
#' @rdname setup_universes
delete_empty_universes <- function(){
  stales <- find_stale_universes()
  if(length(stales) > 10){
    stop("Found more than 10 empty universes. Maybe this is not right.")
  }
  lapply(stales, function(username){
    cat("Uninstalling app for:", username, "\n")
    tryCatch(ghapps::gh_app_installation_delete(username), error = function(e){
      cat("Failed to delete app for:", username, "(already deleted?): ", e$message, "\n")
    })
  })
}

find_universes <- function(days = 10){
  res <- gh::gh('/orgs/r-universe/repos', .limit = 1e5)
  names <- vapply(res, function(x){x$name}, character(1))
  updated <- as.Date(as.POSIXct(vapply(res, function(x){x$updated_at}, character(1))))
  names[updated < (Sys.Date() - days)]
}
