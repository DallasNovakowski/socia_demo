
# Function to propagate the non-empty header from row1
propagate_headers <- function(headers) {
  for (i in 2:length(headers)) {
    if (is.na(headers[i]) || headers[i] == "") {
      headers[i] <- headers[i - 1]
    }
  }
  headers
}

