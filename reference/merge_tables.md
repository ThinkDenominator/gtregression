# Merge gtregression tables and preserve structure and notes

Merge gtregression tables and preserve structure and notes

## Usage

``` r
merge_tables(..., spanners = NULL, theme = "minimal")
```

## Arguments

- ...:

  Two or more `gtregression` objects containing `$table_display`.

- spanners:

  Character vector of spanner labels, one per table. If `NULL`, defaults
  to `"Table 1"`, `"Table 2"`, etc.

- theme:

  Merge theme preset or vector of primitives.

## Value

A merged table object of class `c("gtregression", "merged_table", ...)`.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

uni_tbl <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht"),
  approach = logit
)

multi_tbl <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht"),
  adjust_for = c("age", "lwt"),
  approach = logit
)

merge_tables(
  uni_tbl,
  multi_tbl,
  spanners = c("Univariable", "Adjusted")
)
#> <div id="wkkeulbfdp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#wkkeulbfdp table {
#>   font-family: system-ui;
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #wkkeulbfdp thead, #wkkeulbfdp tbody, #wkkeulbfdp tfoot, #wkkeulbfdp tr, #wkkeulbfdp td, #wkkeulbfdp th {
#>   border-style: none;
#> }
#> 
#> #wkkeulbfdp p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #wkkeulbfdp .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: auto;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #DADADA;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #DADADA;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #wkkeulbfdp .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #wkkeulbfdp .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 3px;
#>   padding-bottom: 5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #wkkeulbfdp .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 6px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #wkkeulbfdp .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 5px;
#>   padding-bottom: 5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #wkkeulbfdp .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_group_heading {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #wkkeulbfdp .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #wkkeulbfdp .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #wkkeulbfdp .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #wkkeulbfdp .gt_row {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #wkkeulbfdp .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #wkkeulbfdp .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #wkkeulbfdp .gt_last_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_first_grand_summary_row {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_last_grand_summary_row_top {
#>   padding-top: 8px;
#>   padding-bottom: 8px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #wkkeulbfdp .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #wkkeulbfdp .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 2px;
#>   padding-bottom: 2px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_left {
#>   text-align: left;
#> }
#> 
#> #wkkeulbfdp .gt_center {
#>   text-align: center;
#> }
#> 
#> #wkkeulbfdp .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #wkkeulbfdp .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #wkkeulbfdp .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #wkkeulbfdp .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #wkkeulbfdp .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #wkkeulbfdp .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #wkkeulbfdp .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #wkkeulbfdp .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #wkkeulbfdp .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #wkkeulbfdp .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #wkkeulbfdp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_col_headings gt_spanner_row">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="Characteristic">Characteristic</th>
#>       <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" style="font-weight: bold;" scope="colgroup" id="Univariable">
#>         <div class="gt_column_spanner">Univariable</div>
#>       </th>
#>       <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="Adjusted">
#>         <div class="gt_column_spanner">Adjusted</div>
#>       </th>
#>     </tr>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="N_p1">N</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="OR..95..CI._p1">OR (95% CI)</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="p.value_p1">p-value</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="Adjusted.OR..95..CI._p2">Adjusted OR (95% CI)</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="p.value_p2">p-value</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="font-weight: bold; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">age</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">189</td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.95 (0.89-1.01)</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.105</td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="font-weight: bold; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">ht</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">189</td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="text-indent: 12px; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">  No</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"><br /></td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">--</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">—</td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="text-indent: 12px; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">  Yes</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"><br /></td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">3.37 (1.02-11.09)</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.046</td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">6.14 (1.56–24.23)</td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.009</td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="font-weight: bold; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">lwt</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">189</td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.99 (0.97-1.00)</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.023</td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="font-weight: bold; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">smoke</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">189</td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="text-indent: 12px; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">  No</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"><br /></td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">--</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">—</td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"></td></tr>
#>     <tr><td headers="Characteristic" class="gt_row gt_left" style="text-indent: 12px; border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">  Yes</td>
#> <td headers="N_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;"><br /></td>
#> <td headers="OR..95..CI._p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">2.02 (1.08-3.78)</td>
#> <td headers="p.value_p1" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.028</td>
#> <td headers="Adjusted.OR..95..CI._p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">1.96 (1.03–3.70)</td>
#> <td headers="p.value_p2" class="gt_row gt_center" style="border-top-width: 1px; border-top-style: solid; border-top-color: #DADADA;">0.040</td></tr>
#>   </tbody>
#>   <tfoot>
#>     <tr class="gt_sourcenotes">
#>       <td class="gt_sourcenote" colspan="6">Abbreviations: OR = Odds Ratio; CI = Confidence Interval.</td>
#>     </tr>
#>     <tr class="gt_sourcenotes">
#>       <td class="gt_sourcenote" colspan="6">Adjusted for age and lwt</td>
#>     </tr>
#>   </tfoot>
#> </table>
#> </div>
```
