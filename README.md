# BlankmanF

**BlankmanF** is an R package for conducting and visualizing the Friedman test, 
a non-parametric statistical test used for analyzing differences across multiple
treatments in repeated measures or block designs.

## Installation

Install the development version from GitHub:

```r
# Replace 'yourusername' with your actual GitHub username
devtools::install_github("yourusername/BlankmanF")
```

## Features

- Perform the Friedman test with a single function
- Generate clean, publication-ready rank plots
- Summarize treatment ranks with mean and standard deviation
- Designed for tidy (long-format) data

## Example

```r
library(BlankmanF)

example_data <- data.frame(
  Subject = rep(1:6, each = 3),
  Treatment = rep(c("A", "B", "C"), times = 6),
  Score = c(85, 88, 90, 78, 80, 82, 91, 93, 92, 87, 89, 90, 70, 72, 74, 76, 77, 79)
)

run_friedman_test(example_data, "Subject", "Treatment", "Score")
plot_friedman_ranks(example_data, "Subject", "Treatment", "Score")
get_friedman_summary(example_data, "Subject", "Treatment", "Score")
```

## License

This package is released under the [CC0 license](https://creativecommons.org/publicdomain/zero/1.0/). Feel free to use, modify, or redistribute without restriction.

## Author

**Holden Blank**  
University of South Florida  
Email: blankh@usf.edu
