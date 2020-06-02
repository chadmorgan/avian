California eBird Exploration
================

I have the full eBrid data of observations from California from January
- March. In this notebook I will document my process of initial
exploration.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(auk)
```

    ## Warning in auk_get_ebd_path(): Directory specified by EBD_PATH does not
    ## exist.

    ## auk 0.4.1 is designed for EBD files downloaded after 2019-08-15. 
    ## EBD data directory:  /Users/chad/Documents/avian/ebd_US-SC_201912_201912_relMar-2020 
    ## eBird taxonomy version:  2019

``` r
library(ggmap)
```

    ## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.

    ## Please cite ggmap if you use it! See citation("ggmap") for details.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(glue)
```

    ## 
    ## Attaching package: 'glue'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

``` r
theme_set(theme_minimal())
```

## Data wrangling and import using auk package

``` r
print(bcr_codes)
```

    ## # A tibble: 66 x 2
    ##    bcr_code bcr_name                      
    ##       <int> <chr>                         
    ##  1        1 Aleutian/Bering Sea Islands   
    ##  2        2 Western Alaska                
    ##  3        3 Arctic Plains And Mountains   
    ##  4        4 Northwestern Interior Forest  
    ##  5        5 Northern Pacific Rainforest   
    ##  6        6 Boreal Taiga Plains           
    ##  7        7 Taiga Shield And Hudson Plains
    ##  8        8 Boreal Softwood Shield        
    ##  9        9 Great Basin                   
    ## 10       10 Northern Rockies              
    ## # … with 56 more rows

``` r
bcr_filtering <- function(
    BCR, 
    out_filename, 
    ebd_path='/Users/chad/Documents/avian/data/ebd_US-CA_202001_202003_relMar-2020'
  ){
  
    ca_ebird_file <- glue('{ebd_path}/ebd_US-CA_202001_202003_relMar-2020.txt')
    
    # apply BCR filter
    f_out0 <- glue('{ebd_path}/{out_filename}_intermediate.txt')
    bcr_filter <- ca_ebird_file %>%
      auk_ebd() %>% 
      auk_bcr(bcr = BCR) %>%
      auk_filter(file = f_out0, overwrite = TRUE) 
    
    # Select columns
    keep_columns <- c("group_identifier", "sampling_event_identifier", 
                  "taxonomic_order","category","common_name","scientific_name",
                  "iba_code","bcr_code","usfws_code","locality","locality_id","locality_type",
                  "county","latitude","longitude","observation_date","time_observations_started",
                  "observer_id","observation_count")
    
    f_out1 <- glue('{ebd_path}/{out_filename}.txt')
    selected_cols <- f_out0 %>% 
      auk_ebd() %>% 
      auk_select(select = keep_columns, file = f_out1, overwrite = TRUE)
}
```

### Filter to coastal California BCR

``` r
bcr_filtering(32, 'coastalCA_ebd')
```

    ## Warning in auk_get_ebd_path(): Directory specified by EBD_PATH does not
    ## exist.
    
    ## Warning in auk_get_ebd_path(): Directory specified by EBD_PATH does not
    ## exist.

### Sierra Nevada filtering

``` r
bcr_filtering(15, 'sierranevada_ebd')
```

    ## Warning in auk_get_ebd_path(): Directory specified by EBD_PATH does not
    ## exist.
    
    ## Warning in auk_get_ebd_path(): Directory specified by EBD_PATH does not
    ## exist.

# Read in Sierra Nevada file

``` r
ebd_path <- '/Users/chad/Documents/avian/data/ebd_US-CA_202001_202003_relMar-2020'
sierra_nevada_file <- glue('{ebd_path}/sierranevada_ebd.txt')
sierra_nevada <- sierra_nevada_file %>% read_ebd()

str(sierra_nevada)
```

    ## tibble [21,381 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ checklist_id             : chr [1:21381] "S63291476" "S63672250" "S62977532" "S62977500" ...
    ##  $ taxonomic_order          : num [1:21381] 11407 27462 27462 445 7885 ...
    ##  $ category                 : chr [1:21381] "species" "species" "species" "species" ...
    ##  $ common_name              : chr [1:21381] "American Kestrel" "American Robin" "American Robin" "American Wigeon" ...
    ##  $ scientific_name          : chr [1:21381] "Falco sparverius" "Turdus migratorius" "Turdus migratorius" "Mareca americana" ...
    ##  $ observation_count        : chr [1:21381] "1" "2" "1" "24" ...
    ##  $ county                   : chr [1:21381] "Alpine" "Alpine" "Alpine" "Alpine" ...
    ##  $ iba_code                 : chr [1:21381] NA NA NA NA ...
    ##  $ bcr_code                 : int [1:21381] 15 15 15 15 15 15 15 15 15 15 ...
    ##  $ usfws_code               : chr [1:21381] NA NA NA NA ...
    ##  $ locality                 : chr [1:21381] "Tamarack Junction" "Carson River Rd." "Markleeville (please use for Yellow-browed Warbler)" "Indian Creek Reservoir" ...
    ##  $ locality_id              : chr [1:21381] "L10474085" "L756766" "L390444" "L272916" ...
    ##  $ locality_type            : chr [1:21381] "P" "H" "H" "H" ...
    ##  $ latitude                 : num [1:21381] 38.6 38.8 38.7 38.7 38.7 ...
    ##  $ longitude                : num [1:21381] -120 -120 -120 -120 -120 ...
    ##  $ observation_date         : Date[1:21381], format: "2020-01-11" "2020-01-22" ...
    ##  $ time_observations_started: chr [1:21381] "14:07:00" "09:53:00" "12:24:00" "13:35:00" ...
    ##  $ observer_id              : chr [1:21381] "obsr28253" "obsr103197" "obsr382535" "obsr382535" ...
    ##  $ sampling_event_identifier: chr [1:21381] "S63291476" "S63672250" "S62977532" "S62977500" ...
    ##  $ group_identifier         : chr [1:21381] NA NA NA NA ...
    ##  - attr(*, "rollup")= logi TRUE

``` r
species_summary <- sierra_nevada %>% 
  group_by(common_name,scientific_name) %>%
  summarise(obs = n()) %>%
  arrange(desc(obs))

head(species_summary,10)
```

    ## # A tibble: 10 x 3
    ## # Groups:   common_name [10]
    ##    common_name           scientific_name       obs
    ##    <chr>                 <chr>               <int>
    ##  1 Common Raven          Corvus corax          955
    ##  2 Dark-eyed Junco       Junco hyemalis        870
    ##  3 Steller's Jay         Cyanocitta stelleri   835
    ##  4 American Robin        Turdus migratorius    630
    ##  5 Mountain Chickadee    Poecile gambeli       606
    ##  6 Northern Flicker      Colaptes auratus      599
    ##  7 Ruby-crowned Kinglet  Regulus calendula     574
    ##  8 Canada Goose          Branta canadensis     569
    ##  9 Red-breasted Nuthatch Sitta canadensis      494
    ## 10 Spotted Towhee        Pipilo maculatus      485
