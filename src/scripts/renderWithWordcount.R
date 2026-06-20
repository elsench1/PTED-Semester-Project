library(quarto)
library(xml2)
library(rvest)
library(stringr)

# -----------------------------
# Settings
# -----------------------------

# This profile is used for the official word/character count.
# It should match the structure of the submitted report.
# With multiple YAML files, this means that _quarto-pdf.yml is used.
count_profile <- "pdf"

# HTML is used as an intermediate format because it is easier and cleaner to count
# than extracting text from the final PDF.
count_output_format <- "zhaw-lsfm-html"

# These pages are counted as flowing text for the word count.
flow_text_page_names <- c(
  "abstract",
  "introduction",
  "methods",
  "results",
  "discussion"
)

# This page is counted for the character count, but not for the flowing-text word count.
reference_page_name <- "references"

# Appendix file where the wordcount block is written.
appendix_file <- "chapters/appendix.qmd"

# Final outputs to render after updating the appendix.
final_renders <- list(
  list(profile = "pdf",  output_format = "zhaw-lsfm-typst"),
  list(profile = "html", output_format = "zhaw-lsfm-html")
)

# -----------------------------
# Helper functions
# -----------------------------

render_project <- function(profile, output_format) {
  quarto::quarto_render(
    input = ".",
    profile = profile,
    output_format = output_format,
    as_job = FALSE
  )
}

find_rendered_page <- function(page_name, required = TRUE) {
  candidates <- c(
    file.path("docs", "chapters", paste0(page_name, ".html")),
    file.path("docs", paste0(page_name, ".html"))
  )
  
  existing <- candidates[file.exists(candidates)]
  
  if (length(existing) == 0) {
    if (required) {
      stop(
        "Rendered file not found for page '", page_name, "'. Tried:\n",
        paste(candidates, collapse = "\n")
      )
    } else {
      return(NA_character_)
    }
  }
  
  existing[[1]]
}

format_number <- function(x) {
  format(x, big.mark = "'", scientific = FALSE, trim = TRUE)
}

extract_rendered_text <- function(html_file) {
  if (!file.exists(html_file)) {
    stop("Rendered file not found: ", html_file)
  }
  
  doc <- read_html(html_file)
  
  main <- html_element(doc, "main#quarto-document-content")
  
  if (inherits(main, "xml_missing") || length(main) == 0) {
    main <- html_element(doc, "main")
  }
  
  if (inherits(main, "xml_missing") || length(main) == 0) {
    stop("No main content found in: ", html_file)
  }
  
  # Remove technical elements that should not be counted.
  xml_remove(xml_find_all(
    main,
    ".//script | .//style | .//pre | .//code | .//button |
     .//*[contains(concat(' ', normalize-space(@class), ' '), ' sourceCode ')] |
     .//*[contains(concat(' ', normalize-space(@class), ' '), ' code-copy-button ')]"
  ))
  
  text <- html_text2(main)
  
  text |>
    str_replace_all("\\s+", " ") |>
    str_squish()
}

count_words <- function(text) {
  words <- str_extract_all(
    text,
    "\\b[\\p{L}\\p{M}]+(?:[-’'][\\p{L}\\p{M}]+)*\\b"
  )[[1]]
  
  length(words)
}

count_characters_including_spaces <- function(text) {
  clean_text <- text |>
    str_replace_all("\\s+", " ") |>
    str_squish()
  
  nchar(clean_text, type = "chars")
}

detect_references_in_text <- function(text) {
  str_detect(
    text,
    regex("\\b(references|literaturverzeichnis|bibliography)\\b", ignore_case = TRUE)
  )
}

update_appendix_wordcount <- function(word_count,
                                      character_count,
                                      references_counted) {
  appendix <- paste(
    readLines(appendix_file, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  
  character_note <- if (references_counted) {
    "characters including spaces and references, excluding code listing"
  } else {
    "characters including spaces, excluding code listing. References were not found in the counted output"
  }
  
  wordcount_block <- paste0(
    "## Wordcount\n\n",
    "<!-- WORDCOUNT-START -->\n\n",
    format_number(word_count), " words\n\n",
    "and\n\n",
    format_number(character_count), " ", character_note, "\n\n",
    "<!-- WORDCOUNT-END -->\n"
  )
  
  if (str_detect(appendix, "(?s)<!-- WORDCOUNT-START -->.*<!-- WORDCOUNT-END -->")) {
    updated_appendix <- str_replace(
      appendix,
      "(?s)## Wordcount\\s*<!-- WORDCOUNT-START -->.*<!-- WORDCOUNT-END -->",
      wordcount_block
    )
  } else if (str_detect(appendix, "(?s)## Wordcount.*$")) {
    updated_appendix <- str_replace(
      appendix,
      "(?s)## Wordcount.*$",
      wordcount_block
    )
  } else {
    updated_appendix <- paste0(appendix, "\n\n", wordcount_block)
  }
  
  writeLines(updated_appendix, appendix_file, useBytes = TRUE)
}

copy_latest_pdf_to_docs <- function(render_started_at, target_dir = "docs") {
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  pdf_files <- list.files(
    ".",
    pattern = "\\.pdf$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(pdf_files) == 0) {
    warning("No PDF file found after rendering.")
    return(invisible(NULL))
  }
  
  pdf_info <- file.info(pdf_files)
  
  recent_pdf_files <- pdf_files[
    !is.na(pdf_info$mtime) &
      pdf_info$mtime >= render_started_at - 60
  ]
  
  if (length(recent_pdf_files) == 0) {
    # Fallback: use the newest PDF found anywhere in the project.
    recent_pdf_files <- pdf_files
  }
  
  recent_pdf_info <- file.info(recent_pdf_files)
  newest_pdf <- recent_pdf_files[which.max(recent_pdf_info$mtime)]
  
  target_pdf <- file.path(target_dir, basename(newest_pdf))
  
  if (normalizePath(newest_pdf, winslash = "/", mustWork = FALSE) ==
      normalizePath(target_pdf, winslash = "/", mustWork = FALSE)) {
    message("PDF already exists in docs: ", target_pdf)
    return(invisible(target_pdf))
  }
  
  file.copy(
    from = newest_pdf,
    to = target_pdf,
    overwrite = TRUE
  )
  
  message("PDF copied to: ", target_pdf)
  
  invisible(target_pdf)
}

find_latest_pdf <- function(render_started_at, search_dir = ".") {
  pdf_files <- list.files(
    search_dir,
    pattern = "\\.pdf$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(pdf_files) == 0) {
    stop("No PDF file found after PDF rendering.")
  }
  
  pdf_info <- file.info(pdf_files)
  
  recent_pdf_files <- pdf_files[
    !is.na(pdf_info$mtime) &
      pdf_info$mtime >= render_started_at - 60
  ]
  
  if (length(recent_pdf_files) == 0) {
    recent_pdf_files <- pdf_files
  }
  
  recent_pdf_info <- file.info(recent_pdf_files)
  recent_pdf_files[which.max(recent_pdf_info$mtime)]
}

backup_pdf <- function(pdf_file) {
  backup_file <- file.path(tempdir(), basename(pdf_file))
  
  file.copy(
    from = pdf_file,
    to = backup_file,
    overwrite = TRUE
  )
  
  message("PDF backed up to: ", backup_file)
  
  backup_file
}

restore_pdf_to_docs <- function(backup_file, target_dir = "docs") {
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  target_file <- file.path(target_dir, basename(backup_file))
  
  file.copy(
    from = backup_file,
    to = target_file,
    overwrite = TRUE
  )
  
  message("PDF restored to: ", target_file)
  
  invisible(target_file)
}

# -----------------------------
# 1. Render an HTML version using the count profile
# -----------------------------

render_project(
  profile = count_profile,
  output_format = count_output_format
)

# -----------------------------
# 2. Find rendered HTML files
# -----------------------------

flow_text_pages <- vapply(
  flow_text_page_names,
  find_rendered_page,
  character(1),
  required = TRUE
)

reference_page <- find_rendered_page(
  reference_page_name,
  required = FALSE
)

# -----------------------------
# 3. Count the rendered output
# -----------------------------

flow_text <- paste(
  vapply(flow_text_pages, extract_rendered_text, character(1)),
  collapse = " "
)

if (!is.na(reference_page)) {
  length_pages <- c(flow_text_pages, reference_page)
} else {
  length_pages <- flow_text_pages
}

length_text <- paste(
  vapply(length_pages, extract_rendered_text, character(1)),
  collapse = " "
)

word_count <- count_words(flow_text)
character_count <- count_characters_including_spaces(length_text)

references_counted <- !is.na(reference_page) || detect_references_in_text(length_text)

if (!references_counted) {
  warning(
    "No references section was found in the counted output. ",
    "The character count therefore does not include a reference list."
  )
}

# -----------------------------
# 4. Update the appendix
# -----------------------------

update_appendix_wordcount(
  word_count = word_count,
  character_count = character_count,
  references_counted = references_counted
)

cat("\nWordcount updated:\n")
cat(format_number(word_count), "words\n")

if (references_counted) {
  cat(
    format_number(character_count),
    "characters including spaces and references, excluding code listing\n\n"
  )
} else {
  cat(
    format_number(character_count),
    "characters including spaces, excluding code listing\n"
  )
  cat("WARNING: References were not found in the counted output.\n\n")
}

# -----------------------------
# 5. Render final outputs
# -----------------------------

# Render PDF first so that the HTML output can link to it.
pdf_render_started_at <- Sys.time()

render_project(
  profile = "pdf",
  output_format = "zhaw-lsfm-typst"
)

# Save a temporary backup of the generated PDF.
pdf_file <- find_latest_pdf(
  render_started_at = pdf_render_started_at
)

pdf_backup <- backup_pdf(pdf_file)

# Render HTML afterwards.
# This may rewrite the docs folder and remove the PDF.
render_project(
  profile = "html",
  output_format = "zhaw-lsfm-html"
)

# Restore the PDF into docs after the HTML render.
restore_pdf_to_docs(
  backup_file = pdf_backup,
  target_dir = "docs"
)