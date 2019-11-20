library(rmarkdown)

render(
  "ft/presentation_ft.Rmd", 
  output_file = "FT Survey Analysis.pdf", 
  output_dir = "presentations")
