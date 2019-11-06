# Set parameters
library(webshot)

output_file <- "D://Project/Field_test/Rmd/Poster-presentation.html"
width <- 1200
poster_width <- 33.1 # inches
res <- 300

# Render document
rmarkdown::render(
  input = "D://Project/Field_test/Rmd/Poster presentation.Rmd",
  output_file = paste0(output_file)
)

# Render previewa
webshot::webshot(
  url = paste0(output_file),
  file = "D://Project/Field_test/Rmd/Poster-presentation.jpg",
  vwidth = width,
  vheight = floor(width * sqrt(2)),          # Use A series aspect ratio
  delay = 1,                                 # Wait to allow all element to load
  zoom = poster_width / (width / res)        # Adjust elements relative size
)

# End of script