# dissect works correctly on Pima dataset

    Code
      print(result, n = nrow(result))
    Output
      # A tibble: 18 x 6
         Variable     Type    `Missing (%)` Unique Levels                Compatibility
         <chr>        <chr>   <chr>          <int> <chr>                 <chr>        
       1 pregnant     numeric 0%                17 -                     compatible   
       2 glucose      numeric 0.7%             135 -                     compatible   
       3 pressure     numeric 4.6%              46 -                     compatible   
       4 triceps      numeric 29.6%             50 -                     compatible   
       5 insulin      numeric 48.7%            185 -                     compatible   
       6 mass         numeric 1.4%             247 -                     compatible   
       7 pedigree     numeric 0%               517 -                     compatible   
       8 age          numeric 0%                52 -                     compatible   
       9 diabetes     numeric 0%                 2 -                     maybe        
      10 bmi          factor  1.4%               3 Normal, Overweight, ~ compatible   
      11 age_cat      factor  0%                 3 Young, Middle-aged, ~ compatible   
      12 npreg_cat    factor  0%                 2 Low parity, High par~ compatible   
      13 glucose_cat  factor  0.7%               2 Normal, High          compatible   
      14 bp_cat       factor  4.6%               2 Normal, High          compatible   
      15 triceps_cat  factor  29.6%              2 Normal, High          compatible   
      16 insulin_cat  factor  48.7%              3 Low, Normal, High     compatible   
      17 dpf_cat      factor  0%                 3 Low Genetic Risk, Mo~ compatible   
      18 diabetes_cat factor  0%                 2 Diabetes negative, D~ compatible   

