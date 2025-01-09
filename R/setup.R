#' Setup universes
#'
#' List app installations and setup universe repos.
#'
#' @rdname setup_universes
#' @export
setup_universes <- function(){
  cat("Listing app installations:\n")
  print(installs <- list_app_installations())
  cat("Checking universe monorepos...")
  universes <- list_universes()

  # Check for NEW app installations first
  newbies <- setdiff(c(installs$name, testusers), c(universes, skiplist))
  if(!length(newbies)){
    cat("No NEW installations found.\n")
  } else {
    cat("Found NEW installations:", newbies, sep = '\n - ')
    print(gh::gh_whoami())
    lapply(newbies, create_universe_repo)
  }

  # Check for app installations that can be removed (no published packages)
  stats <- runiverse_stream_in('/stats/universes')
  oldies <- subset(installs, days > 10)
  empties <- setdiff(oldies$name, c(skiplist, stats$universe))
  cat("Found empty universes: ", paste(empties, collapse = ", "), "\n")
  if(length(empties) > 10 && Sys.getenv('FORCE_DELETE') == ""){
    stop("Found more than 10 empty installations. Maybe this is not right.")
  }
  for(username in empties){
    cat("Uninstalling app for:", username, "\n")
    tryCatch(ghapps::gh_app_installation_delete(username), error = function(e){
      cat("Failed to delete app for:", username, "(already deleted?): ", e$message, "\n")
    })
  }

  # Download crantogit registries
  crantogit <- jsonlite::fromJSON('https://r-universe-org.github.io/cran-to-git/index.json')
  cranrepos <- names(sort(unlist(crantogit), decreasing = TRUE))
  owners <- setdiff(cranrepos, skiplist)

  # Check for new CRAN owners, limit batch add to 20
  newcran <- setdiff(owners, universes)
  if(length(newcran)){
    cat("Found some new CRAN owners:\n", newcran, sep = '\n - ')
    lapply(utils::head(newcran, 200), create_universe_repo)
  }

  # Check for monorepos that are no longer needed
  deleted <- setdiff(universes, c(installs$name, testusers, owners))
  if(length(deleted)){
    cat("Cleaning monorepos without app installation or cran packages:", deleted, sep = '\n - ')
    if(length(deleted) > 30 && Sys.getenv('FORCE_DELETE') == ""){
      cat("This number looks too large. Not deleting anything.\n")
      stop("Failed to list app installations?")
    } else if(length(deleted) > 15) {
      cat("This number looks a bit large. Only deleting empty universes.\n")
      lapply(deleted, delete_universe_repo, only_if_empty = TRUE)
    } else {
      lapply(deleted, delete_universe_repo, only_if_empty = FALSE)
    }
  }

  # Remove packages that do not belong to an existing universe
  orphans <- setdiff(stats$universe, universes)
  if(length(orphans) > 10){
    stop("More than 10 orphan universes found:", paste(orphans, collapse = ', '))
  }
  lapply(orphans, function(x){
    try(delete_universe_repo(x))
  })
}

#' @export
delete_orphans <- function(){
  universes <- list_universes()
  files <- runiverse_stream_in('/stats/files')
  files$orphan <- is.na(match(files$user,  universes))
  deleted <- subset(files, orphan & !duplicated(paste0(files$user, '/', files$package)))
  for(i in seq_len(nrow(deleted))){
    cranlikeurl <- sprintf('https://%s.r-universe.dev/api/packages', deleted$user[i])
    delete_package(cranlikeurl, deleted$package[i])
  }
}

list_universes <- function(){
  res <- gh::gh('/users/r-universe/repos', per_page = 100, .limit = 1e5)
  names <- tolower(vapply(res, function(x){x$name}, character(1)))
  updated <- as.POSIXct(chartr('TZ', '  ', vapply(res, function(x){x$pushed_at}, character(1))))
  names[order(updated, decreasing = TRUE)]
}

# Ignore these orgs
skiplist <- c('ropenscilabs', 'ropensci-archive', 'r-universe', 'r-universe-org', 'actions', 'workflows')
testusers <- c("test", 'actions', 'workflows', 'cran', 'bioc', 'r-multiverse', 'r-multiverse-staging', 'ropensci-champions', 'cranhaven')

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
  pkgs <- jsonlite::fromJSON(sprintf('https://%s.r-universe.dev/api/ls', owner))
  if(length(pkgs)){
    if(only_if_empty){
      cat(sprintf("Skipping universe '%s' which contains packages: %s\n", owner, paste(pkgs, collapse = ', ')))
      return(invisible())
    } else {
      lapply(pkgs, function(pkg){
        try(delete_package(sprintf('https://%s.r-universe.dev/api/packages', owner), pkg))
      })
    }
  }
  cat("Deleting universe for:", owner, '\n')
  gh::gh(paste0('/repos/r-universe/', owner), .method = 'DELETE')
}

list_app_installations <- function(){
  all <- ghapps::gh_app_installation_list()
  names <- tolower(vapply(all, function(x){x$account$login}, character(1)))
  created <- as.POSIXct(chartr('TZ', '  ', vapply(all, function(x){x$created_at}, character(1))))
  updated <- as.POSIXct(chartr('TZ', '  ', vapply(all, function(x){x$updated_at}, character(1))))
  df <- data.frame(name = names, created = created, updated = updated, days = Sys.Date() - as.Date(updated))
  df[order(df$days),]
}

delete_package <- function(cranlike_url, package){
  message("Deleting: ", package)
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
  url <- sprintf("%s/%s", cranlike_url, package)
  out <- parse_res(curl::curl_fetch_memory(url, handle = h))
  stopifnot(identical(unique(out$Package), package))
}

parse_res <- function(res){
  text <- rawToChar(res$content)
  if(res$status >= 400)
    stop(text)
  jsonlite::fromJSON(text)
}

runiverse_stream_in <- function(path){
  urlstr <- sprintf('https://r-universe.dev%s?nocache=%f', path, rnorm(1))
  message("Getting ", urlstr)
  jsonlite::stream_in(url(urlstr), verbose = interactive())
}
