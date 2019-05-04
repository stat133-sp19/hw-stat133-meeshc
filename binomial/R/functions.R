# auxiliary function to test if input prob is valid probability value
check_prob <- function(prob) {
  if (prob >= 0 && prob <= 1 && length(prob) == 1) {
    return(TRUE)
  } else {
    stop("p must be a number between 0 and 1")
  }
}

# auxiliary function to check if input trials is a valid number of trials
check_trials <- function(trials) {
  if (length(trials) == 1 && trials > 0 && trials%%1 == 0) {
    return(TRUE)
  } else {
    stop("invalid trials value")
  }
}

# auxiliary function to check if input success is a valid value for number of successes
check_success <- function(success, trials) {
  if (is.vector(success) == TRUE && all(success >= 0 && success <= trials && success%%1 == 0 && check_trials(trials))) {
    return(TRUE)
  } else {
    stop('invalid successs value')
  }
}

# auxiliary function for mean
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# auxiliary function for variance
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# auxiliary function for mode
aux_mode <- function(trials, prob) {
  return(floor(prob * trials + prob))
}

# auxiliary function for skewness
aux_skewness <- function(trials, prob) {
  numerator <- 1 - 2 * prob
  denominator <- sqrt(trials * prob * (1 - prob))
  return(numerator / denominator)
}

# auxiliary function for kurtosis
aux_kurtosis <- function(trials, prob) {
  numerator <- 1 - 6 * prob * (1 - prob)
  denominator <- trials * prob * (1 - prob)
  return(numerator / denominator)
}

#' @title Bin Choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes, can be a number or a vector
#' @return number or vector of combinations, depending on k
#' @export
#' @examples
#' bin_choose(5, 0)
bin_choose <- function(n, k) {
  if (all(k > n)) {
    stop('k cannot be greater than n')
  }
  numerator <- factorial(n)
  denominator <- factorial(k) * factorial(n - k)
  return(numerator / denominator)
}

#' @title Bin Probability
#' @description calculates probability of getting k number of successes in n trials
#' @param success number of successes you want to calculate the probability of
#' @param trials number of trials
#' @param prob probability of success
#' @return probability of k successes in n trials
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability <- function(success, trials, prob) {
  if (check_trials(trials) && check_prob(prob) && check_success(success, trials)) {
    middle <- prob^success
    right <- (1 - prob)^(trials - success)
    left <- bin_choose(trials, success)
    return(left * middle * right)
  }
}

#' @title Bin Distribution
#' @description calculates binomial probability distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame with probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  success <- 0:trials
  probability <- bin_probability(success, trials, prob)
  result <- data.frame("success" = success, "probability" = probability)
  class(result) <- c("bindis", "data.frame")
  return(result)
}

#' @export
plot.bindis <- function(input) {
  barplot(input$probability, xlab = "successes", ylab = "probability", names.arg = input$success)
}

#' @title Bin Cumulative
#' @description Creates a data frame with probability distribution and cumulative probabilities
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame with bincum and data.frame classes
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  result <- bin_distribution(trials, prob)
  cumulative <- rep(0, length(result$probability))
  for (i in 1:length(result$probability)) {
    if (i == 1) {
      cumulative[i] <- result$probability[i]
    } else {
      cumulative[i] <- cumulative[i - 1] + result$probability[i]
    }
  }
  result$cumulative <- cumulative
  class(result) <- c("bincum", "data.frame")
  return(result)
}

#' @export
plot.bincum <- function(input) {
  plot(y = input$cumulative, x = input$success, type = "o", xlab = "successes", ylab = "probability")
}

#' @title Bin Variable
#' @description Creates a binomial random variable objects given trials and probability
#' @param trials number of trials
#' @param prob probability of success
#' @return list with named elements trials and prob and of class binvar
#' @export
bin_variable <- function(trials, prob) {
  result <- list(trials = trials, prob = prob)
  class(result) <- "binvar"
  return(result)
}

#' @export
print.binvar <- function(input) {
  cat('"Binomial Variable"\n\nParameters\n')
  cat(paste("- number of trials:", input$trials))
  cat(paste("\n- prob of success:", input$prob))
}

#' @export
summary.binvar <- function(input) {
  trials <- input$trials
  prob <- input$prob
  result <- list(trials = trials, prob = prob, mean = aux_mean(trials, prob), variance = aux_variance(trials, prob), mode = aux_mode(trials, prob), skewness = aux_skewness(trials, prob), kurtosis = aux_kurtosis(trials, prob))
  class(result) <- "summary.binvar"
  return(result)
}

#' @export
print.summary.binvar <- function(input) {
  cat('"Summary Binomial"\n\n')
  cat("Parameters\n")
  cat(paste("- number of trials:", input$trials))
  cat(paste("\n- prob of success:", input$prob))
  cat("\n\nMeasures\n")
  cat(paste("- mean:", input$mean))
  cat(paste("\n- variance:", input$variance))
  cat(paste("\n- mode:", input$mode))
  cat(paste("\n- skewness:", input$skewness))
  cat(paste("\n- kurtosis:", input$kurtosis))
}

#' @title Bin Mean
#' @description main function for mean of bin
#' @param trials number of trials
#' @param prob probability of success
#' @return number that is the mean of the bin
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  if (check_trials(trials) && check_prob(prob)) {
    return(aux_mean(trials, prob))
  }
}

#' @title Bin Variance
#' @description main function for variance of bin
#' @param trials number of trials
#' @param prob probability of success
#' @return number that is the variance of the bin
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  if (check_trials(trials) && check_prob(prob)) {
    return(aux_variance(trials, prob))
  }
}

#' @title Bin Mode
#' @description main function for mode of bin
#' @param trials number of trials
#' @param prob probability of success
#' @return number that is the mode of the bin
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  if (check_trials(trials) && check_prob(prob)) {
    return(aux_mode(trials, prob))
  }
}

#' @title Bin Skewness
#' @description main function for skewness of bin
#' @param trials number of trials
#' @param prob probability of success
#' @return number that is the skewness of the bin
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  if (check_trials(trials) && check_prob(prob)) {
    return(aux_skewness(trials, prob))
  }
}

#' @title Bin Kurtosis
#' @description main function for kurtosis of bin
#' @param trials number of trials
#' @param prob probability of success
#' @return number that is the kurtosis of the bin
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  if (check_trials(trials) && check_prob(prob)) {
    return(aux_kurtosis(trials, prob))
  }
}
