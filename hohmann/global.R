library(ggplot2)

data <- data.frame(
  provider = c("Mutual Life", "Danger and Danger", "Borisov, Inc.", "Lemon Fruit"),
  product = c("ISA", "ISA", "SIPP", "Unwrapped"),
  method = c("Cash", "Stock", "Stock", "Cash"),
  rereg = c("Postal", "Electronic", "Postal", "Electronic"),
  q_25 = c(1, 2, 3, 4),
  q_50 = c(5, 6, 7, 8),
  q_75 = c(12, 14, 16, 16),
  stringsAsFactors = F
)

get_provider_list <- function() {
  # TODO: This needs to query the DB and return a list of all providers for
  # whom we have enough data over the last six months
  return(as.list(unique(data$provider)))
}

create_message <- function(provider, product, method, success = TRUE) {
  if (success) {
    string <- paste(
      "Showing data for transfers of", product, "accounts from", provider,
      "as", tolower(method), "over the last six months"
    )
  } else {
    string <- paste("We haven't had enough", tolower(method), "transfers of",
                    product, "accounts from", provider, "to make a prediction.")
  }
  return(string)
}

create_duration_string <- function(duration) {
  duration <- round(duration)
  if (duration == 0) {
    return("Same-day")
  } else if (duration == 1) {
    return("1 day")
  } else {
    return(paste(duration, "days"))
  }
}

make_plot <- function(data) {
  # TODO: Use real data
  p <- ggplot(diamonds, aes(x = carat, y = price, colour = clarity)) +
    geom_point()
  return(p)
}

get_estimates <- function(data, success = TRUE, min_cases = 10) {
  result <- list(
    low_estimate_postal = create_duration_string(1),
    medium_estimate_postal = create_duration_string(2),
    high_estimate_postal = create_duration_string(3),
    low_estimate_electronic = create_duration_string(1),
    medium_estimate_electronic = create_duration_string(2),
    high_estimate_electronic = create_duration_string(3)
  )
  return(result)
}
