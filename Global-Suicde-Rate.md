Global Suicide Rate Overview
================
Syarifah Rizki
2/7/2022

# Background

In 2021, the World Health Organization (WHO) had published a fact-sheet
article regarding the global suicide overview on its official website.
According to its report, an estimate of 703,000 people across the globe
had taken their lives every year and even more had attempted to do so.
Suicide has become a major public health problem and although it is
commonly found in in the low and middle income countries, it is a
phenomenon that also occurred in higher income countries. Eastern Europe
for example, becomes the region with the highest number of suicide rate
both for men and women alike and through this report, I aim to analyse
the global suicide rates in 100 countries using the “Suicide Rates
Overview 1985 to 2016” dataset that I obtained from Kaggle. I also tried
to limit the analysis to 10 years only, starting from the year
1995-2015. Several variables such as GDP per capita will be excluded
from the analysis since I will only focused on the trends of suicide
rates in each country.

# Import packages & read data

``` r
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(plotly)
library(glue)
```

``` r
suicide <-  read.csv('Suicide Rates Overview 1985 to 2016.csv')
```

Columns description:

-   country : Name of countries
-   year : Year
-   sex : Male & Female
-   age : Age group is divided into six category
-   suicides_no : Number of suicides committed
-   population : Total population
-   suicides/100k pop : Number of suicides committed for a population of
    100,000
-   country-year : Name of countries and year
-   HDI for year : Human Development Index for every country
-   gdp_for_year : Total of GDP per year for every country
-   gdp_per_capita : GDP measures a country’s economic output per person
    and is calculated by dividing the GDP of a country by its
    population.
-   generation: There are six generations that exist between
    1985-2016. 1) G.I. Generation/The Greatest Generation (born
    1901-1924), 2) Silent (born 1925-1945), 3) Boomers (born
    1946-1964), 4) Generation X (born 1965-1980) 5)
    Millenials/Generation Y (born 1981-1996), 6) Generation Z (born
    1997-2012)

# Data Wrangling

Check dimensions.

``` r
dim(suicide)
```

    ## [1] 27820    12

We have 27820 observations and 12 columns.

Check data types for all columns.

``` r
str(suicide)
```

    ## 'data.frame':    27820 obs. of  12 variables:
    ##  $ ï..country        : chr  "Albania" "Albania" "Albania" "Albania" ...
    ##  $ year              : int  1987 1987 1987 1987 1987 1987 1987 1987 1987 1987 ...
    ##  $ sex               : chr  "male" "male" "female" "male" ...
    ##  $ age               : chr  "15-24 years" "35-54 years" "15-24 years" "75+ years" ...
    ##  $ suicides_no       : int  21 16 14 1 9 1 6 4 1 0 ...
    ##  $ population        : int  312900 308000 289700 21800 274300 35600 278800 257200 137500 311000 ...
    ##  $ suicides.100k.pop : num  6.71 5.19 4.83 4.59 3.28 2.81 2.15 1.56 0.73 0 ...
    ##  $ country.year      : chr  "Albania1987" "Albania1987" "Albania1987" "Albania1987" ...
    ##  $ HDI.for.year      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ gdp_for_year....  : chr  "2,156,624,900" "2,156,624,900" "2,156,624,900" "2,156,624,900" ...
    ##  $ gdp_per_capita....: int  796 796 796 796 796 796 796 796 796 796 ...
    ##  $ generation        : chr  "Generation X" "Silent" "Generation X" "G.I. Generation" ...

-   Remove all commas from ‘gdp_for_year….’ column.
-   Use str_replace() function from stringr packages.

``` r
suicide$gdp_for_year.... <- str_replace_all(suicide$gdp_for_year...., ",",'')
```

-   Change data types to factor: country, age, sex, generation
-   Change data types to datetime: year
-   change data types to integer: gdp.for.year

``` r
 suicide <- suicide %>% 
  rename(country = ï..country,
         gdp.for.year = gdp_for_year....,
         gdp.per.capita = gdp_per_capita....) %>%
  mutate_at(.vars = c("country","sex", "age", "generation"), .funs=as.factor) %>% 
  mutate(gdp.for.year = as.numeric(gdp.for.year),
         year = as.character(year),
         year = as.Date(year, "%Y"),
         year = year(year))

str(suicide)
```

    ## 'data.frame':    27820 obs. of  12 variables:
    ##  $ country          : Factor w/ 101 levels "Albania","Antigua and Barbuda",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ year             : num  1987 1987 1987 1987 1987 ...
    ##  $ sex              : Factor w/ 2 levels "female","male": 2 2 1 2 2 1 1 1 2 1 ...
    ##  $ age              : Factor w/ 6 levels "15-24 years",..: 1 3 1 6 2 6 3 2 5 4 ...
    ##  $ suicides_no      : int  21 16 14 1 9 1 6 4 1 0 ...
    ##  $ population       : int  312900 308000 289700 21800 274300 35600 278800 257200 137500 311000 ...
    ##  $ suicides.100k.pop: num  6.71 5.19 4.83 4.59 3.28 2.81 2.15 1.56 0.73 0 ...
    ##  $ country.year     : chr  "Albania1987" "Albania1987" "Albania1987" "Albania1987" ...
    ##  $ HDI.for.year     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ gdp.for.year     : num  2.16e+09 2.16e+09 2.16e+09 2.16e+09 2.16e+09 ...
    ##  $ gdp.per.capita   : int  796 796 796 796 796 796 796 796 796 796 ...
    ##  $ generation       : Factor w/ 6 levels "Boomers","G.I. Generation",..: 3 6 3 2 1 2 6 1 2 3 ...

Now, every columns are stored with the correct data types.

``` r
head(suicide)
```

    ##   country year    sex         age suicides_no population suicides.100k.pop
    ## 1 Albania 1987   male 15-24 years          21     312900              6.71
    ## 2 Albania 1987   male 35-54 years          16     308000              5.19
    ## 3 Albania 1987 female 15-24 years          14     289700              4.83
    ## 4 Albania 1987   male   75+ years           1      21800              4.59
    ## 5 Albania 1987   male 25-34 years           9     274300              3.28
    ## 6 Albania 1987 female   75+ years           1      35600              2.81
    ##   country.year HDI.for.year gdp.for.year gdp.per.capita      generation
    ## 1  Albania1987           NA   2156624900            796    Generation X
    ## 2  Albania1987           NA   2156624900            796          Silent
    ## 3  Albania1987           NA   2156624900            796    Generation X
    ## 4  Albania1987           NA   2156624900            796 G.I. Generation
    ## 5  Albania1987           NA   2156624900            796         Boomers
    ## 6  Albania1987           NA   2156624900            796 G.I. Generation

Check for missing values.

``` r
colSums(is.na(suicide))
```

    ##           country              year               sex               age 
    ##                 0                 0                 0                 0 
    ##       suicides_no        population suicides.100k.pop      country.year 
    ##                 0                 0                 0                 0 
    ##      HDI.for.year      gdp.for.year    gdp.per.capita        generation 
    ##             19456                 0                 0                 0

``` r
suicide <- subset(suicide, select = -c(HDI.for.year))
head(suicide)
```

    ##   country year    sex         age suicides_no population suicides.100k.pop
    ## 1 Albania 1987   male 15-24 years          21     312900              6.71
    ## 2 Albania 1987   male 35-54 years          16     308000              5.19
    ## 3 Albania 1987 female 15-24 years          14     289700              4.83
    ## 4 Albania 1987   male   75+ years           1      21800              4.59
    ## 5 Albania 1987   male 25-34 years           9     274300              3.28
    ## 6 Albania 1987 female   75+ years           1      35600              2.81
    ##   country.year gdp.for.year gdp.per.capita      generation
    ## 1  Albania1987   2156624900            796    Generation X
    ## 2  Albania1987   2156624900            796          Silent
    ## 3  Albania1987   2156624900            796    Generation X
    ## 4  Albania1987   2156624900            796 G.I. Generation
    ## 5  Albania1987   2156624900            796         Boomers
    ## 6  Albania1987   2156624900            796 G.I. Generation

Our data contains information that is dated back to 1987. However, our
main interest here is the information from 1995-2015 only. Therefore, we
will need to subset suicide cases from the last ten years starting from
1995.

``` r
decade <- suicide %>% 
  filter(year >= 1995,
         suicide$year <= 2015)

head(decade)
```

    ##   country year    sex         age suicides_no population suicides.100k.pop
    ## 1 Albania 1995   male 25-34 years          13     232900              5.58
    ## 2 Albania 1995   male 55-74 years           9     178000              5.06
    ## 3 Albania 1995 female   75+ years           2      40800              4.90
    ## 4 Albania 1995 female 15-24 years          13     283500              4.59
    ## 5 Albania 1995   male 15-24 years          11     241200              4.56
    ## 6 Albania 1995   male   75+ years           1      25100              3.98
    ##   country.year gdp.for.year gdp.per.capita      generation
    ## 1  Albania1995   2424499009            835    Generation X
    ## 2  Albania1995   2424499009            835          Silent
    ## 3  Albania1995   2424499009            835 G.I. Generation
    ## 4  Albania1995   2424499009            835    Generation X
    ## 5  Albania1995   2424499009            835    Generation X
    ## 6  Albania1995   2424499009            835 G.I. Generation

# Exploratory Data Analysis (EDA)

Several research questions that we want to answer:

1.  “Which country has the highest number of suicide cases in the last
    ten years?”
2.  “Does a certain sex has a relatively higher number of suicide
    rates?”
3.  “Inspect the global trends of suicide rates within the last ten
    years. Which countries have a trend of declining or increasing?”

Numbers of suicide cases in the last ten years based on sex

``` r
sex <- decade %>% 
  group_by(year, sex) %>% 
  summarise(suicides_no = sum(suicides_no))

head(sex)
```

    ## # A tibble: 6 x 3
    ## # Groups:   year [3]
    ##    year sex    suicides_no
    ##   <dbl> <fct>        <int>
    ## 1  1995 female       54504
    ## 2  1995 male        189040
    ## 3  1996 female       54583
    ## 4  1996 male        192142
    ## 5  1997 female       54126
    ## 6  1997 male        186619

``` r
line = c("grey", "firebrick")

sex_plot <- ggplot(sex, aes(
  x = year,
  y = suicides_no,
  color= sex,
  group = sex,
  text = glue("Sex: {sex}
              Year: {year}
              Total of suicides: {comma(suicides_no)}"))) +
  geom_line(size=1) + 
  scale_color_manual(values = line) +
  labs(x = NULL,
       y = NULL,
       title = 'Total of suicides based on sex') +
  scale_y_continuous(label = comma_format()) +
  theme_minimal()

sex_plot
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Regardless of which year we’re looking at, the number of suicides
committed by male remains about three times higher compared to female.
The range number of female that died from suicides were around 40,000 -
58,000 while there were roughly 150,000 - 200,000 male that committed
suicide each year.

``` r
# numbers of suicide cases in the last ten years based on generations
gen <- decade %>% 
  group_by(generation) %>% 
  summarise(sum(suicides_no)) %>%
  rename('suicides_total' = 'sum(suicides_no)') %>% 
  arrange(desc(suicides_total))

gen
```

    ## # A tibble: 6 x 2
    ##   generation      suicides_total
    ##   <fct>                    <int>
    ## 1 Boomers                1666539
    ## 2 Generation X           1310523
    ## 3 Silent                 1297704
    ## 4 Millenials              613425
    ## 5 G.I. Generation         127393
    ## 6 Generation Z             15906

``` r
gen_plot <- gen %>%
  ggplot(aes(x = reorder(generation, -suicides_total), y = suicides_total)) +
  geom_col(fill='firebrick') +
  labs(x = NULL,
       y = NULL,
       title = 'Total of suicides based on generations',
       subtitle = 'from 1995-2015') +
  scale_y_continuous(label = comma_format()) +
  theme_minimal()

gen_plot
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Top 5 countries with the highest numbers of suicide cases in the last
ten years (not based on the population).

``` r
country_sn <- decade %>% 
  group_by(country) %>% 
  summarise(sum(suicides_no)) %>%
  rename('suicides_total' = 'sum(suicides_no)') %>% 
  arrange(desc(suicides_total)) %>% 
  top_n(5)
```

    ## Selecting by suicides_total

``` r
country_sn
```

    ## # A tibble: 5 x 2
    ##   country            suicides_total
    ##   <fct>                       <int>
    ## 1 Russian Federation         930323
    ## 2 United States              727923
    ## 3 Japan                      589174
    ## 4 Ukraine                    229956
    ## 5 Republic of Korea          227213

``` r
country_sn_plot <- country_sn %>%
  ggplot(aes(x = reorder(country, -suicides_total),
             y = suicides_total,
             text = glue("Country:{country}
                         Total suicides: {comma(suicides_total)}"))) +
  geom_col(fill='firebrick') +
  labs(x = NULL,
       y = NULL,
       title = 'Countries with the highest number of suicides',
       subtitle = 'from 1995-2015') +
  scale_y_continuous(label = comma_format()) +
  theme_minimal()

country_sn_plot
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Russia, US, Japan, South Korea and Ukraine are the top 5 countries that
has the highest number of suicide cases within the last ten years. Now
we want to see if those same countries also exist in the top 20
countries with the highest suicide rates, taking into consideration the
ratio between suicides rates and its total of population.

The plot above only measures the global total suicides between 1995-2015
and it doesn’t take into account the difference of population size in
each country. This can be problematic as countries with denser
population will likely to have higher total of suicide compared with
those that have smaller population size. That is why, in the next
section we will divided the total of suicides that occurred in a country
with its own population size.

``` r
# aggregate countries name with their suicides rates and country's population
country_rat <- decade %>% 
  group_by(country) %>% 
  summarise(total_suicides = sum(suicides_no),
            pop = sum(population)) %>% 
  mutate(ratio = (total_suicides / pop)*100) %>% 
  arrange(desc(ratio)) %>% 
  top_n(20)
```

    ## Selecting by ratio

``` r
country_rat
```

    ## # A tibble: 20 x 4
    ##    country            total_suicides        pop  ratio
    ##    <fct>                       <int>      <dbl>  <dbl>
    ##  1 Lithuania                   27217   65639914 0.0415
    ##  2 Russian Federation         930323 2867389720 0.0324
    ##  3 Belarus                     47176  149621392 0.0315
    ##  4 Sri Lanka                   44628  154567226 0.0289
    ##  5 Latvia                      12770   44852640 0.0285
    ##  6 Hungary                     56818  201252445 0.0282
    ##  7 Kazakhstan                  83893  302076069 0.0278
    ##  8 Slovenia                    10615   40268619 0.0264
    ##  9 Estonia                      7034   27090810 0.0260
    ## 10 Ukraine                    229956  901824384 0.0255
    ## 11 Guyana                       2846   12001419 0.0237
    ## 12 Republic of Korea          227213  962846436 0.0236
    ## 13 Japan                      589174 2523090244 0.0234
    ## 14 Finland                     22259  104426558 0.0213
    ## 15 Belgium                     42273  209631421 0.0202
    ## 16 Croatia                     17746   88163308 0.0201
    ## 17 Suriname                     1759    8888955 0.0198
    ## 18 Serbia                      24179  126140138 0.0192
    ## 19 France                     209781 1143209675 0.0184
    ## 20 Austria                     29991  163884549 0.0183

``` r
country_rat %>% 
  ggplot(aes(x = ratio,
             y = reorder(country, ratio))) +
  geom_col(aes(fill = ratio)) +
  scale_fill_gradient(low = 'darkgrey', high = 'firebrick') +
  labs(x = NULL,
       y = NULL,
       title = 'Suicides ratio per population') +
  theme_minimal()
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

According to the barplot above, among the top five countries that has
the highest number of suicide cases, four of them; Russia, South Korea,
Japan, and Ukraine still ranked quite high when we included the ratio
between suicides number and the country’s population. They all exist in
the top 20 countries with the most suicide cases/population. United
States is the only country that didn’t make to the top 20 countries.
Next, we’ll inspect more the data from those five countries.

``` r
# subset data from the top 5 countries

top_five <- decade %>%
  filter(country %in% c('Russian Federation', 'Japan', 'Ukraine', 'United States', 'Republic of Korea'))

head(top_five)
```

    ##   country year    sex         age suicides_no population suicides.100k.pop
    ## 1   Japan 1995   male   75+ years        1358    2553900             53.17
    ## 2   Japan 1995   male 55-74 years        4429   12402300             35.71
    ## 3   Japan 1995 female   75+ years        1582    4592800             34.45
    ## 4   Japan 1995   male 35-54 years        5507   18054500             30.50
    ## 5   Japan 1995   male 25-34 years        1692    8404400             20.13
    ## 6   Japan 1995 female 55-74 years        2426   14003800             17.32
    ##   country.year gdp.for.year gdp.per.capita      generation
    ## 1    Japan1995 5.449116e+12          46043 G.I. Generation
    ## 2    Japan1995 5.449116e+12          46043          Silent
    ## 3    Japan1995 5.449116e+12          46043 G.I. Generation
    ## 4    Japan1995 5.449116e+12          46043         Boomers
    ## 5    Japan1995 5.449116e+12          46043    Generation X
    ## 6    Japan1995 5.449116e+12          46043          Silent

``` r
levels(top_five$country)
```

    ##   [1] "Albania"                      "Antigua and Barbuda"         
    ##   [3] "Argentina"                    "Armenia"                     
    ##   [5] "Aruba"                        "Australia"                   
    ##   [7] "Austria"                      "Azerbaijan"                  
    ##   [9] "Bahamas"                      "Bahrain"                     
    ##  [11] "Barbados"                     "Belarus"                     
    ##  [13] "Belgium"                      "Belize"                      
    ##  [15] "Bosnia and Herzegovina"       "Brazil"                      
    ##  [17] "Bulgaria"                     "Cabo Verde"                  
    ##  [19] "Canada"                       "Chile"                       
    ##  [21] "Colombia"                     "Costa Rica"                  
    ##  [23] "Croatia"                      "Cuba"                        
    ##  [25] "Cyprus"                       "Czech Republic"              
    ##  [27] "Denmark"                      "Dominica"                    
    ##  [29] "Ecuador"                      "El Salvador"                 
    ##  [31] "Estonia"                      "Fiji"                        
    ##  [33] "Finland"                      "France"                      
    ##  [35] "Georgia"                      "Germany"                     
    ##  [37] "Greece"                       "Grenada"                     
    ##  [39] "Guatemala"                    "Guyana"                      
    ##  [41] "Hungary"                      "Iceland"                     
    ##  [43] "Ireland"                      "Israel"                      
    ##  [45] "Italy"                        "Jamaica"                     
    ##  [47] "Japan"                        "Kazakhstan"                  
    ##  [49] "Kiribati"                     "Kuwait"                      
    ##  [51] "Kyrgyzstan"                   "Latvia"                      
    ##  [53] "Lithuania"                    "Luxembourg"                  
    ##  [55] "Macau"                        "Maldives"                    
    ##  [57] "Malta"                        "Mauritius"                   
    ##  [59] "Mexico"                       "Mongolia"                    
    ##  [61] "Montenegro"                   "Netherlands"                 
    ##  [63] "New Zealand"                  "Nicaragua"                   
    ##  [65] "Norway"                       "Oman"                        
    ##  [67] "Panama"                       "Paraguay"                    
    ##  [69] "Philippines"                  "Poland"                      
    ##  [71] "Portugal"                     "Puerto Rico"                 
    ##  [73] "Qatar"                        "Republic of Korea"           
    ##  [75] "Romania"                      "Russian Federation"          
    ##  [77] "Saint Kitts and Nevis"        "Saint Lucia"                 
    ##  [79] "Saint Vincent and Grenadines" "San Marino"                  
    ##  [81] "Serbia"                       "Seychelles"                  
    ##  [83] "Singapore"                    "Slovakia"                    
    ##  [85] "Slovenia"                     "South Africa"                
    ##  [87] "Spain"                        "Sri Lanka"                   
    ##  [89] "Suriname"                     "Sweden"                      
    ##  [91] "Switzerland"                  "Thailand"                    
    ##  [93] "Trinidad and Tobago"          "Turkey"                      
    ##  [95] "Turkmenistan"                 "Ukraine"                     
    ##  [97] "United Arab Emirates"         "United Kingdom"              
    ##  [99] "United States"                "Uruguay"                     
    ## [101] "Uzbekistan"

Okay, so i just subset my data frame (decade) to a new dataframe
(top_five) and it was supposed to consisted of five countries only. But
turns out, all of the countries from our previous list are still exist.
Now i’ll removed the unused levels from our top_five using
*droplevels()*.

``` r
top_five$country <- droplevels(top_five$country)

levels(top_five$country)
```

    ## [1] "Japan"              "Republic of Korea"  "Russian Federation"
    ## [4] "Ukraine"            "United States"

Good. Now there’s only five levels of countries in our top_five
dataframe. Next, let’s find out the ratio between number of suicide
cases found and the population for each countries from 1995-2015.

``` r
top_five_trend <- top_five %>%
  group_by(country, year) %>% 
  summarise(suicide_num = sum(suicides_no),
            population = sum(population),
            ratio = (suicide_num / population)*100)

head(top_five_trend)
```

    ## # A tibble: 6 x 5
    ## # Groups:   country [1]
    ##   country  year suicide_num population  ratio
    ##   <fct>   <dbl>       <int>      <int>  <dbl>
    ## 1 Japan    1995       21249  118349500 0.0180
    ## 2 Japan    1996       21967  118785000 0.0185
    ## 3 Japan    1997       23280  119060000 0.0196
    ## 4 Japan    1998       31458  119338000 0.0264
    ## 5 Japan    1999       31115  119540000 0.0260
    ## 6 Japan    2000       29989  119752660 0.0250

``` r
# highlight any country that has an increasing number of suicide rates within the last ten years

cols <- c("gray", "firebrick", "grey", "grey", "grey")

five_trend_plot <- top_five_trend %>% 
  ggplot(aes(x = year,
             y = ratio,
             color = country,
             group = country)) +
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = cols) +
  labs(title = "Suicide ratio trends in five countries",
       x = NULL,
       y = NULL) +
  theme_minimal()
  
five_trend_plot
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

From the line chart above we can see the trends of suicide rates in each
of the five countries from 1995-2015.The country that I want to
highlight here is South Korea. There has been a rise of suicides rates
in the Republic of Korea or South Korea and it is slightly more
fluctuate compared to the other top 5 countries. Suicides number in the
other four countries witness a general decline or going on stagnant over
the last ten years. On the contrary, the number of suicides rates in
South Korea (Republic of Korea) has increased.The number of suicides
rates per population in 2009 is three times higher than when in 1995.

In general, the ratio of suicide rates per population in the top 5
countries are declining, except in South Korea.

By highlighting a single line in our chart, it is much easier for our
audience to focused their attention on which country that is in contrast
with the general trends. In this case, South Korea is the only country
that has an increasing ratio of suicide rates while the other four
countries have an improvement in pressing down the ratio of suicides
rates.

It’s also interesting to know that South Korea begins as the country
with the lowest ratio of suicide rates in 1995 but it ranked first as
the country with the highest ratio of suicide cases per population
within a span of ten years.

``` r
single_country <- decade %>% 
  filter(country == c("Russian Federation","Republic of Korea")) %>% 
  group_by(country, year) %>% 
  summarise(total_suicides = sum(suicides_no),
            pop = sum(population),
            ratio = (total_suicides / pop)*100)
```

    ## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.

``` r
single_country
```

    ## # A tibble: 42 x 5
    ## # Groups:   country [2]
    ##    country            year total_suicides      pop  ratio
    ##    <fct>             <dbl>          <int>    <int>  <dbl>
    ##  1 Republic of Korea  1995           1955 19302700 0.0101
    ##  2 Republic of Korea  1996           2540 21125900 0.0120
    ##  3 Republic of Korea  1997           3862 24956900 0.0155
    ##  4 Republic of Korea  1998           3192 19234500 0.0166
    ##  5 Republic of Korea  1999           2898 20494700 0.0141
    ##  6 Republic of Korea  2000           4395 26689565 0.0165
    ##  7 Republic of Korea  2001           4437 24594672 0.0180
    ##  8 Republic of Korea  2002           4241 22735787 0.0187
    ##  9 Republic of Korea  2003           6925 25107082 0.0276
    ## 10 Republic of Korea  2004           7507 26363344 0.0285
    ## # ... with 32 more rows

``` r
cols <- c("firebrick","grey")

single_country %>% 
  ggplot(aes(x = year,
             y = ratio,
             color = country,
             group = country)) +
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = cols) +
  labs(title = "Comparison of suicide ratio trends",
       subtitle = "In South Korea & Russia between 1995-2015",
       x = NULL,
       y = NULL) +
  theme_minimal()
```

![](Global-Suicde-Rate_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

The trend seems to be reversed in the two countries.

# Conclusion

1.  Total of suicides committed by male are always three times higher
    compared to female in any year.
2.  ‘Boomers’ are the generation that has the highest suicide rates with
    1,666,539 number of cases.
3.  Russia, US, Japan, South Korea and Ukraine are the top 5 countries
    that has the highest number of suicide cases within the last ten
    years.
4.  The ratio of suicide rates per population in the top five countries
    are declining, except in South Korea.
5.  United States is the only country from the top five countries that
    wasn’t on the list of “20 countries with the highest suicide ratio
    per population”.
