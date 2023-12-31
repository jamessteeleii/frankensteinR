#' Create a template website for living meta-analysis
#'
#' @description
#' #' Create a template website for a living meta-analysis. You can add details of the methods/protocol, results, and allow others to contribute
#'
#' @param path path to the location for your website on your machine
#' @param title title of the living meta-analysis
#' @param authors authors of the living meta-analysis
#' @param description short description of the living meta-analysis
#' @param output_dir directory to render the website output to
#' @param license what license to add (CC-BY only for now)
#' @param google_analytics the google analytics address
#' @param margin_header defaults to ""
#' @param footer defaults to "license (YEAR) author"
#' @param light_theme "flatly" (see https://quarto.org/docs/output-formats/html-themes.html)
#' @param dark_theme "darkly"
#' @param df_print how to print tables (default, kable, tibble, paged)
#' @param open whether to activate the new project in RStudio
#' @param render whether to render the quarto website when opening
#'
#' @return sets up a project and renders the demo website
#' @export
#' @importFrom rlang is_interactive
#'
create_living_analysis <- function(path = "living_analysis",
                                   title = "Living Meta-Analysis",
                                   authors = "Me",
                                   description = "Description of Living Analysis",
                                   output_dir = "docs",
                                   license = "CC-BY",
                                   google_analytics = "",
                                   margin_header = "",
                                   footer = paste0(license, " (",
                                                   format(Sys.Date(), "%Y"),
                                                   ") ", authors),
                                   css = "",
                                   light_theme = "flatly",
                                   dark_theme = "darkly",
                                   df_print = "kable",
                                   open = rlang::is_interactive(),
                                   render = TRUE) {


  # checks ----
  requireNamespace("knitr")
  # prompt quarto install if not available
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto isn't installed, see https://quarto.org/docs/get-started/")
  }
  requireNamespace("quarto")
  version <- quarto::quarto_version()
  if (version < "1.3.56") {
    message(
      "You might want to update quarto to version 1.3.56 or later to avoid some bugs with the bibliography files, see https://quarto.org/docs/get-started/"
    )
  }

  requireNamespace("glossary")

  path <- path.expand(path)

  # create project -----
  usethis::ui_todo("Setting up project...")
  usethis::create_project(path = path, open = FALSE)

  # add content ----
  usethis::ui_todo("Adding content...")

  # sort out authors ----
  alist <- list()
  if (is.list(authors)) {
    # authors as list, e.g.,
    # authors <- list(c("James", "Steele"), c("Matthew", "B.", "Jane"))
    alist <- lapply(authors, function(a) {
      do.call(author, as.list(a))
    })
    auth_txt_list <- sapply(alist, function(a) {
      trimws(paste(a$name$given, a$name$family))
    })
    auth_txt <- comma_and(auth_txt_list)

  } else if (length(authors) > 1) {
    # single author as vector, e.g.,
    # authors <- c("James", "Steele")
    alist <- do.call(author, as.list(authors))
    auth_txt <- paste(alist$name$given, alist$name$family)
  } else {
    # authors as text, e.g.,
    # authors <- "James Steele & Matthew B. Jane"
    alist <- authors
    auth_txt <- authors
  }

  ## create _quarto.yml ----
  file <-
    system.file("quarto", "_quarto.yml", package = "frankensteinR")
  yml <- yaml::read_yaml(file)

  yml$project$`output-dir` = output_dir

  yml$website$title = title
  yml$website$description = description
  yml$website$`google-analytics` = google_analytics
  yml$website$`body-footer` = footer
  yml$website$`margin-header` = margin_header

  yml$format$html$`df-print` <- df_print
  yml$format$html$theme$light[[1]] <- light_theme
  yml$format$html$theme$dark[[1]] <- dark_theme

  write_yaml(yml, file.path(path, "_quarto.yml"))
  usethis::ui_done("Modified _quarto.yml")

  ## add license ----
  if (license == "CC-BY") {
    license_op <- utils::capture.output({
      xfun::in_dir(path, usethis::use_ccby_license())
    }, type = "message")
    usethis::ui_done("Added license")
  }

  ## copy qmd files ----
  qmd <- list.files(system.file("quarto", package = "frankensteinR"),
                    "\\.qmd$",
                    full.names = TRUE)
  sapply(qmd, file.copy, path)
  usethis::ui_done("Added demo qmd files")

  ## includes and R directories ----
  include <- system.file("quarto/include", package = "frankensteinR")
  file.copy(include, file.path(path), recursive = TRUE)
  gstyle <- utils::capture.output(glossary::glossary_style())
  gstyle <- gstyle[2:(length(gstyle)-1)]
  write(gstyle, file.path(path, "include", "glossary.css"))
  write(css, file.path(path, "include", "style.css"), append = TRUE)
  rfiles <- system.file("quarto/R", package = "frankensteinR")
  file.copy(rfiles, path, recursive = TRUE)
  images <- system.file("quarto/images", package = "frankensteinR")
  file.copy(images, path, recursive = TRUE)

  # .Rprofile ----
  rprofpath <- file.path(path, ".Rprofile")
  write("source(\"R/frankensteinR_setup.R\")", file = rprofpath, append = TRUE)
  if (df_print == "dt") {
    write("source(\"R/dt_tables.R\")", file = rprofpath, append = TRUE)
  }

  write("source(\"R/my_setup.R\")", file = rprofpath, append = TRUE)

  usethis::ui_done("Added auxillary files")

  # open project in RStudio ----
  if (open) usethis::proj_activate(path)

  # render book ----
  if (render) {
    usethis::ui_todo("Rendering website...")
    render_op <- tryCatch({
      utils::capture.output({
        xfun::in_dir(path, quarto::quarto_render(as_job = FALSE))
      })
    },
    error = function(e) {
      warning(e, call. = FALSE)
      return("")
    })

    ## check for success and show book ----
    if (!any(grepl("Output created: docs/index.html", render_op, fixed = TRUE))) {
      warning(render_op)
    } else {
      websitepath <- file.path(path, "docs", "index.html")
      usethis::ui_done("Website rendered at {normalizePath(websitepath)}")
      utils::browseURL(websitepath)
    }
  }

  if (!render & !open) {
    usethis::ui_done("Website created at {normalizePath(path)}")
  }

  invisible(path)
}

#' List to Text
#'
#' Convert a list or vector to text with human-readable separators, e.g., "A, B & C".
#'
#' @param x The list or vector to convert
#' @param comma The text to use to separate all but the last item
#' @param and The text to use to separate the last item
#' @param oxford Whether to use an oxford comma before the last item
#'
#' @return A character string
#' @export
#'
#' @examples
#' comma_and(LETTERS[1:5])
#' comma_and(LETTERS[1:5], and = " and ")
#' comma_and(LETTERS[1:5], comma = "; ")
#'
#' # change and to use an oxford comma
#' my_list <- list("Nelson Mandela",
#'                 "an 800-year-old demigod",
#'                 "a dildo collector")
#' comma_and(my_list) # probably not what you mean
#' comma_and(my_list, oxford = TRUE)
comma_and <- function(x, comma = ", ", and = " & ", oxford = FALSE) {
  if (length(x) == 1) {
    txt <- x
  } else {
    last <- x[length(x)]
    first <- paste(x[1:(length(x)-1)], collapse = comma)
    if (oxford) and <- gsub(" +", " ", paste0(comma, and))
    txt <- paste0(first, and, last)
  }

  return(txt)
}


#' Write yaml with fixed logical values
#'
#' @param x the object to be converted
#' @param file either a character string naming a file or a connection open for writing
write_yaml <- function(x, file) {
  yaml::write_yaml(x, file, handlers = list(
    logical = function(x) {
      result <- ifelse(x, "true", "false")
      class(result) <- "verbatim"
      return(result)
    }
  ))
}


