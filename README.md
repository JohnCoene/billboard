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

## Functions

![Customise all the things](FOO.png)

* `b_area`
* `b_area_spline`  
* `b_bar`
* `b_board`
* `b_brewer`
* `b_colors`
* `b_donut`
* `b_gauge`
* `b_grid`
* `b_grid_line`
* `billboardOutput`
* `b_inferno`
* `b_inter`
* `b_labels`
* `b_legend` 
* `b_line` 
* `b_magma`
* `b_pad`
* `b_pie`
* `b_plasma`
* `b_region`
* `b_resize` 
* `b_rotate`
* `b_scatter`
* `b_size`
* `b_spline` 
* `b_step`
* `b_step_area`  
* `b_subchart`
* `b_svg`
* `b_title`
* `b_tooltip`
* `b_trans`
* `b_viridis`
* `b_xaxis`
* `b_xgrid` 
* `b_xlabel`
* `b_xtick` 
* `b_yaxis`
* `b_ygrid`
* `b_ylabel`
* `b_ytick`
* `b_zoom`
* `renderBillboard`
