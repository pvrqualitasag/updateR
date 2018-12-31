#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom utils download.file
#' @importFrom stringr str_extract_all
#' @importFrom dplyr select
#' @title Downloads and installs the latest version of R for Mac OS X.
#' @description
#' Update your version of R from inside R itself (Mac OS X only). When the admin_password
#' is omitted, the gui-installer of Mac OS X is started and the new version of R is installed.
#' When all packages should be re-installed, then we have to go back to the original caller
#' and then answer the question whether packages should be re-installed.
#'
#' @param admin_password \code{character}. The system-wide password of the user. The parameter will be only employed to execute commands gaining administrator privileges on the computer and will not be stored anywhere.
#' @author Andrea Cirillo, Robert Myles McDonnell, Peter von Rohr
#' @examples
#' updateR(admin_password = "****")
#' updateR::updateR()
#' @export
updateR <- function(admin_password = NULL, file = NA){

  # first test for on OS
  stopifnot(.Platform$OS.type == "unix")

  ### # write a data frame with all packages installed
  installed.packages() %>%
    as.data.frame() %>%
    select(Package) %>%
    as.vector() -> needed_packages # saving packages installed before updating R version
  needed_packages <- paste(unlist(needed_packages))
  needed_package_path <- "/tmp/needed_packages.RData"
  save(needed_packages, file = needed_package_path)

  ### # define the download source
  page_source = "https://cran.rstudio.com/bin/macosx/"

  css <- "body > table"
  if (is.na(file)){
    file <- xml2::read_html(page_source) %>%
      rvest::html_nodes(css) %>%
      rvest::html_text() %>%
      stringr::str_extract_all(pattern = "^[:print:]*\\.pkg") %>%
      .[[1]]
  }

  stopifnot(grepl(".pkg", file) == TRUE)

  latest_version <- as.numeric(paste(stringr::str_extract_all(pattern = "[:digit:]{1}",file)[[1]],collapse=""))
  installed_version <- as.numeric(paste(stringr::str_extract_all(pattern = "[:digit:]{1}",paste(version$major,version$minor))[[1]],collapse=""))
  if (installed_version >= latest_version) {
    message(paste("Update not necessary. Latest version ====",version$version.string,"==== already installed."))
    return()
  }

  url <- paste0(page_source, file)

  destpath <- paste(getwd(),"/",sep = "")
  fullpath <- paste(destpath,file,sep = "")
  # download package, set folder for download
  download.file(url, fullpath)

  #install .pkg file
  pkg <- gsub("\\.pkg" , "", file)
  message(paste0("Installing ", pkg, "...please wait"))

  ### # use old version, if admin password is given
  if ( is.null(admin_password)){
    command <- paste0("open ", fullpath)
  } else {
    command <- paste0("echo ", admin_password, " | sudo -S installer -pkg ",
                      "'", fullpath, "'", " -target /")
  }
  system(command, wait = TRUE)

  arg <- paste0("--check-signature ", fullpath)
  system2("pkgutil", arg, wait = TRUE)

  # install back the packages saved at the beginning of the process
  ### # path to Rscript
  rscript_path <- file.path(R.home(), "bin/Rscript")
  ### # install.package expression
  install_package_expr <- paste0(rscript_path, " -e 'load(file = \"", needed_package_path,
                                 "\");install.packages(as.vector(needed_packages), repos = \"https://cran.rstudio.com/\")'")

  ### # should libraries be re-installed
  if (ask.user.yn.question(question = "After installing the new version of R, re-install packages for new version?"))
    system(install_package_expr, wait = TRUE)


  # store version of R
  x <- system2("R", args = "--version", stdout = TRUE)
  x <- x[1]

  message(paste0("Everything went smoothly, R was updated to ", x))
  message(paste0("Also the packages installed on your previous version of R were restored"))
  message(paste0("Removing temporary package data in: ", needed_package_path))

}


## ------- Helper Functions below ---------------------------------------
#' Ask User Yes/No Questions
#'
#' @param question Title of menu
#' @param GUI should gui-version of menu be shown
#' @param add_lines_before print a separation line
#'
#' @return TRUE, for first version
ask.user.yn.question <- function(question, GUI = FALSE, add_lines_before = TRUE) {
  choices <- c("Yes", "No")

  if(add_lines_before & !GUI) cat("------------------------\n")
  the_answer <- menu(choices, graphics = GUI, title = question)

  ### # re-map the answer to true/false
  ifelse(the_answer == 1L, TRUE, FALSE)   # returns TRUE or FALSE
}
