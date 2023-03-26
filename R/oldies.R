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
  oldies <- subset(installs, basename(installs$registry) == 'universe')

}

issue_body <- function(name){
gsub('{user}', name, fixed = TRUE, 'Dear r-universe early adopter,

This is a one time message from [r-universe](https://r-universe.dev)!

We made a small change in the system which requires your action: To keep using
[this registry](https://github.com/{user}/universe/blob/main/packages.json) for [your universe](https://{user}.r-universe.dev), __please rename the current repository from `universe`
into `{user}.r-universe.dev`__, i.e. the full domain of your universe.

For more information see: https://ropensci.org/blog/2023/02/07/runiverse-registry-repo/

We appreciate your help. Thank you for using r-universe! 🚀

')
}
