#' @export
listing_companies <- function(path = "https://raw.githubusercontent.com/thinh-vu/vnstock/beta/data/listing_companies_enhanced-2023.csv") {
  df <- read.csv(path)
  return(df)
}