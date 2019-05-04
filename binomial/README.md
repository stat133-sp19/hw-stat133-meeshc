## Overview
`"binomial"` provides functionalities for visualizing and accessing summary measures of a binomial variable.
- `bin_variable` creates a binomial random variable object (of class`"binvar"`)
- `bin_distribution` creates a binomial distribution object (of class `"bindis"`)
- `bin_cumulative` creates a binomial cumulative object (of class `"bincum"`)
- `summary` creates a summary for a binvar object, resulting in a `"summary.binvar"` object
- `bin_probability` returns the probability of generating the specified number of successes in the specified number of trials given `prob` probability of success. It will raise an error if any of the parameters are invalid
- `bin_mean`, `bin_variance`, `bin_mode`, `bin_skewness`, and `bin_kurtosis` are all summary measure functions that can be used with `trials` and `prob` parameters

## Usage
```{r}
# create a bin variable
bin1 <- bin_variable(trials = 10, p = 0.3)

# create a summary for the bin variable
binsum1 <- summary(bin1)

# create objects for binomial distributions
bindis1 <- bin_distribution(10, 0.3)
bincum1 <- bin_cumulative(10, 0.3)

# graph the two binomial distribution objects
graph(bindis1)
graph(bincum1)

# see individual summary measures given a certain amount of trials and probability of success
bin_mean(10, 0.3)
bin_variance(10, 0.3)
bin_mode(10, 0.3)
bin_skewness(10, 0.3)
bin_kurtosis(10, 0.3)
```

