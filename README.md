# billboard

[billboard.js](https://naver.github.io/billboard.js) for R.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("JohnCoene/billboard")
```

## Example

```r
library(billboard)

mtcars %>% 
  b_board(mpg) %>% 
  b_area(qsec) %>% 
  b_spline(wt) %>% 
  b_bar(disp, axis = "y2") %>% 
  b_step(cyl) %>% 
  b_xlabel("Miles per galon") %>% 
  b_brewer("Set1") %>% 
  b_tooltip(grouped = TRUE)
```
