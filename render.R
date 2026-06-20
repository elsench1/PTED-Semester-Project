library(quarto)

# PDF mit ZHAW Typst-Format
quarto_render(
  input = ".",
  profile = "pdf",
  output_format = "zhaw-lsfm-typst"
)

# HTML mit ZHAW HTML-Format
quarto_render(
  input = ".",
  profile = "html",
  output_format = "zhaw-lsfm-html"
)