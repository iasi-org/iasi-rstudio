.edr_groups <- function(project_path) {
   chapters_path <- file.path(project_path, "chapters")
   directories <- list.dirs(chapters_path, recursive = FALSE, full.names = FALSE)
   matches <- regexec("^([0-9]{2})[-_ ](.+)$", directories)
   parts <- regmatches(directories, matches)
   valid <- lengths(parts) == 3L

   if (!any(valid)) {
      return(data.frame(code = character(), label = character(), directory = character()))
   }

   parts <- parts[valid]
   data.frame(
      code = vapply(parts, `[[`, character(1), 2L),
      label = vapply(parts, function(x) gsub("[-_]", " ", x[[3L]]), character(1)),
      directory = directories[valid],
      stringsAsFactors = FALSE
   )
}

.next_edr_number <- function(group_path, code) {
   files <- list.files(
      group_path,
      pattern = paste0("^EDR", code, "[0-9]{3}-.*\\.qmd$"),
      ignore.case = TRUE
   )

   if (!length(files)) {
      return("001")
   }

   sequence <- as.integer(substr(files, 6L, 8L))
   sprintf("%03d", max(sequence, na.rm = TRUE) + 1L)
}

.edr_slug <- function(title) {
   slug <- iconv(title, from = "", to = "ASCII//TRANSLIT")
   slug <- tolower(slug)
   slug <- gsub("[^a-z0-9]+", "-", slug)
   gsub("(^-+|-+$)", "", slug)
}

.edr_filename <- function(project_path, groups, group_index, title) {
   group <- groups[group_index, , drop = FALSE]
   group_path <- file.path(project_path, "chapters", group$directory)
   sequence <- .next_edr_number(group_path, group$code)
   slug <- .edr_slug(title)

   if (!nzchar(slug)) {
      return("")
   }

   paste0("EDR", group$code, sequence, "-", slug, ".qmd")
}

#' Create an EDR document
#'
#' Opens an RStudio gadget that creates the next numbered EDR in a group from
#' the template bundled with `iasi.rstudio`.
#'
#' @return The path of the new document, invisibly, or `NULL` if cancelled.
#' @export
create_edr <- function() {
   if (!rstudioapi::isAvailable()) {
      stop("'Crear EDR' debe ejecutarse desde RStudio.", call. = FALSE)
   }

   project_path <- rstudioapi::getActiveProject()
   if (is.null(project_path) || basename(normalizePath(project_path)) != "iasi-edr") {
      rstudioapi::showDialog(
         title = "Crear EDR",
         message = "Esta acción solo está disponible en el proyecto 'iasi-edr'."
      )
      return(invisible(NULL))
   }

   groups <- .edr_groups(project_path)
   if (!nrow(groups)) {
      rstudioapi::showDialog(
         title = "Crear EDR",
         message = "No hay grupos EDR en 'chapters'. Se espera una carpeta 'nn-nombre'."
      )
      return(invisible(NULL))
   }

   labels <- stats::setNames(seq_len(nrow(groups)), groups$label)
   ui <- miniUI::miniPage(
      miniUI::gadgetTitleBar("Crear EDR"),
      miniUI::miniContentPanel(
         shiny::selectInput("group", "Grupo", choices = labels),
         shiny::textInput("title", "Título"),
         shiny::tags$strong("Archivo"),
         shiny::textOutput("filename")
      )
   )

   server <- function(input, output, session) {
      filename <- shiny::reactive({
         shiny::req(input$group)
         .edr_filename(project_path, groups, as.integer(input$group), input$title)
      })

      output$filename <- shiny::renderText({
         value <- filename()
         if (nzchar(value)) value else "Escribe un título"
      })

      shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
      shiny::observeEvent(input$done, {
         if (!nzchar(filename())) {
            shiny::showNotification("Escribe un título para el EDR.", type = "error")
            return()
         }
         shiny::stopApp(list(group = as.integer(input$group), title = input$title))
      })
   }

   result <- shiny::runGadget(
      ui,
      server,
      viewer = shiny::dialogViewer("Crear EDR", width = 560, height = 360)
   )
   if (is.null(result)) {
      return(invisible(NULL))
   }

   group <- groups[result$group, , drop = FALSE]
   group_path <- file.path(project_path, "chapters", group$directory)
   filename <- .edr_filename(project_path, groups, result$group, result$title)
   destination <- file.path(group_path, filename)

   if (file.exists(destination)) {
      stop("El EDR calculado ya existe; vuelve a ejecutar la acción.", call. = FALSE)
   }

   template <- system.file("templates", "edr.qmd", package = "iasi.rstudio")
   if (!nzchar(template)) {
      stop("No se encontró la plantilla EDR instalada.", call. = FALSE)
   }

   content <- readLines(template, warn = FALSE, encoding = "UTF-8")
   yaml_title <- gsub('"', '\\"', result$title, fixed = TRUE)
   content <- sub("\\{\\{ title \\}\\}", yaml_title, content)
   writeLines(content, destination, useBytes = TRUE)

   rstudioapi::navigateToFile(normalizePath(destination))
   invisible(destination)
}
