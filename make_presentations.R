library(rmarkdown)

source("ft/load_data_ft.R")

render(
  "ft/presentation_slt.Rmd", 
  output_file = "FT Survey Analysis.pdf", 
  output_dir = "presentations",
  params = list(response_data = response_data, employee_data = employee_data)
)

for (d in unique(response_data$division)) {
  if (d != "All") {
    responses <- response_data %>%
      filter(division ==  "All" | division == d)
    
    employees <- employee_data %>%
      filter(division ==  "All" | division == d)
    
    render(
      "ft/presentation_divisional.Rmd",
      output_file = paste0("FT Survey Analysis - ", d, ".pdf"),
      output_dir = "presentations",
      params = list(response_data = responses, employee_data = employees)
    )
  }
}

source("pt/load_data_pt.R")

render(
  "pt/presentation_pt.Rmd",
  output_file = "Venue Survey Analysis - ECC.pdf",
  output_dir = "presentations",
  params = list(
    response_data = filter(response_data, division == "Edmonton Convention Centre"),
    employee_data = filter(employee_data, division == "Edmonton Convention Centre"),
    subtitle = "Edmonton Convention Centre"
  )
)

render(
  "pt/presentation_pt.Rmd",
  output_file = "Venue Survey Analysis - Expo.pdf",
  output_dir = "presentations",
  params = list(
    response_data = filter(response_data, division == "Edmonton Expo Centre"),
    employee_data = filter(employee_data, division == "Edmonton Expo Centre"),
    subtitle = "Edmonton Expo Centre"
  )
)
