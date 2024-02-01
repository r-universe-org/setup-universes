find_old_registries <- function(){
  installs <- list_app_installations()
  installs$registry = NA_character_
  for(i in which(is.na(installs$registry))){
    try({
      res <- gh::gh(sprintf('/repos/r-universe/%s/contents/.registry', installs$name[i]))
      installs$registry[i] <- res$submodule_git_url
      message(installs$name[i], ' OK')
    })
  }
  oldies <- subset(installs, basename(installs$registry) != sprintf('%s.r-universe.dev', installs$name))
  oldies <- subset(oldies, basename(oldies$registry) != 'cran-to-git')
  return(oldies)
}

issue_title <- function(name){
  sprintf("Action required: please rename this repo to: %s.r-universe.dev", name)
}

issue_body <- function(name, has_cran_to_git){
txt <- gsub('{user}', name, fixed = TRUE, 'Dear r-universe user,

Last year we made a small security fix which requires your action:

To keep using your current [package list](https://github.com/{user}/universe/blob/HEAD/packages.json) for [https://{user}.r-universe.dev](https://{user}.r-universe.dev), __please [rename](https://github.com/{user}/universe/settings) the `{user}/universe` git repo from `universe` into `{user}.r-universe.dev`__, i.e. the full domain name of your universe.\n')

if(has_cran_to_git){
  txt <- paste(txt, gsub('{user}', name, fixed = TRUE, '\nAlternatively you can delete this repo, in which case {user}.r-universe.dev will show the automatically generated package list from: https://github.com/r-universe-org/cran-to-git/blob/HEAD/{user}.json\n'))
}

txt <- paste(txt, '\nFor more information [this blog post](https://ropensci.org/blog/2023/02/07/runiverse-registry-repo/).

Thank you for using r-universe! Feel free to reach out if you have any questions 🚀')
}

create_rename_issue <- function(name){
  tryCatch({
    req <- curl::curl_fetch_memory(sprintf('https://github.com/r-universe-org/cran-to-git/blob/HEAD/%s.json', name))
    has_cran_to_git <- req$status_code == 200
    token <- ghapps::gh_app_token(name)
    gh::gh('POST /repos/{owner}/universe/issues',
           owner = name,
           title = issue_title(name),
           body = issue_body(name, has_cran_to_git), .token = token)
    cat(name, 'created!\n')
    TRUE
  }, error = function(e) FALSE)
}


comment_in_issue <- function(name){
  tryCatch({
    token <- ghapps::gh_app_token(name)
    commits <- gh::gh('/repos/{owner}/universe/commits',
                  owner = name, .token = token, .limit = 1000)
    authors <- unique(vapply(commits, function(x){
      if(x$committer$login == 'web-flow'){
        x$author$login
      } else {
        x$committer$login
      }
    }, character(1)))

    issues <- gh::gh('/repos/{owner}/universe/issues',
                  owner = name,
                  creator='r-universe[bot]', .token = token)
    issue_number <- issues[[1]]$number
    if(basename(issues[[1]]$repository_url) != 'universe'){
      message("Repository seems already renamed?? ", issues[[1]]$repository_url)
      return(FALSE)
    }

    txt <- "Friendly reminder: this registry will soon be de-activated. You can re-active it at any time by renaming this repository as explaind above."
    txt <- paste(txt, '\n\ncc:',  paste0("@", unique(c(name, authors)), collapse = ' '))
    comment <- gh::gh('POST /repos/{owner}/universe/issues/{issue_number}/comments',
                  owner = name, issue_number = issue_number, body = txt)
    cat(name, 'Comment created!\n')
    TRUE
  }, error = function(e) {
    message(e)
    FALSE
  })
}
