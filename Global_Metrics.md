Global Metrics of Economic Prosperity and Health
================
Quinn Keck
5/15/2019

Distribution of Credit Scores and Infant Mortality Rate
=======================================================

``` r
countries <- read_csv("country_rows_df.csv")
countries <- drop_na(countries)

ggplot(countries, aes(x = countries$`Country credit rating, 0–100 (best)*`)) + 
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white", 
        bins = 100) + geom_density(alpha = 0.2, fill = "green4") + 
    labs(title = "Distribution of Country Credit Scores in 2018", 
        x = "Country credit rating, 0–100", y = "", caption = "Source: World Econmic Forum, 2018")
```

<img src="Global_Metrics_files/figure-markdown_github/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
ggplot(countries, aes(x = countries$`Infant mortality, deaths/1,000 live births*`)) + 
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white", 
        bins = 100) + geom_density(alpha = 0.2, fill = "blue") + 
    labs(title = "Distribution of Infant Mortality Rates in 2018", 
        x = "Infant Mortality, deaths per 1000 live births", 
        y = "", caption = "Source: World Econmic Forum, 2018")
```

<img src="Global_Metrics_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
healthcare <- read_csv("health_unviersal_rename_cols.csv")

ggplot(healthcare, aes(x = Infant_Mortality, fill = Heathcare)) + 
    geom_density(alpha = 0.3) + labs(title = "Distribution of Infant Mortality Rates", 
    x = "Infant Mortality, deaths per 1000 live births", y = "", 
    caption = "Source: World Econmic Forum, 2018") + theme_gray()
```

<img src="Global_Metrics_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Credit Score and Infant Mortality
=================================

``` r
healthcare <- read_csv("health_unviersal_rename_cols.csv")
healthcare %>% ggplot() + # stat_smooth(aes(x=Infant_Mortality, y=Credit_Rating),method
# = 'loess', col = 'gray', alpha =0.1) + # se=FALSE
geom_point(aes(x = Infant_Mortality, y = Credit_Rating, color = Type, 
    size = GDP), alpha = 0.6) + labs(title = "Universal Healthcare Type and Infant Mortality", 
    x = "Infant Mortality, deaths per 1000 live births", y = "County Credit Rating", 
    caption = "Source: World Econmic Forum, 2018 Data", color = "Universal Healthcare Type") + 
    theme(plot.caption = element_text(hjust = 0))
```

<img src="Global_Metrics_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
# Labels

bottom_credits <- healthcare %>% filter(Heathcare == "Universal Coverage" & 
    Credit_Rating <= 50)

bottom_credits$Credit_Rating_Minus_3 <- bottom_credits$Credit_Rating - 
    3

USA <- healthcare %>% filter(Name == "United States")

USA$Position <- USA$Infant_Mortality + 6

India <- healthcare %>% filter(Name == "India")

India$Position <- India$Infant_Mortality + 4

China <- healthcare %>% filter(Name == "China")

China$Position <- China$Infant_Mortality + 4

Nigeria <- healthcare %>% filter(Name == "Nigeria")

Nigeria$Position <- Nigeria$Infant_Mortality + 4

Nigeria$Credit_Rating_Plus_One <- Nigeria$Credit_Rating + 0.5

# Plot

healthcare %>% ggplot() + stat_smooth(aes(x = Infant_Mortality, 
    y = Credit_Rating), method = "loess", col = "gray", alpha = 0.1) + 
    scale_color_manual(values = c("navyblue", "chartreuse3")) + 
    # geom_label(data = bottom_credits, aes(x=Infant_Mortality,
 y=Credit_Rating_Minus_3, label = Name), colour =
 'springgreen4', size = 2, fontface = 'bold', alpha = 0.9)
 geom_label(data = USA, aes(x=Position, y=Credit_Rating,
 label = Name), colour = 'darkblue', size = 2, fontface =
 'bold', alpha = 0.9) + geom_label(data = China,
 aes(x=Position, y=Credit_Rating, label = Name), colour =
 'darkblue', size = 2, fontface = 'bold', alpha = 0.9) +
 geom_label(data = India, aes(x=Position, y=Credit_Rating,
 label = Name), colour = 'darkblue', size = 2, fontface =
 'bold', alpha = 0.9) + geom_label(data = Nigeria,
 aes(x=Position, y=Credit_Rating_Plus_One, label = Name),
 colour = 'darkblue', size = 2, fontface = 'bold', alpha =
 0.9) +
geom_point(aes(x = Infant_Mortality, y = Credit_Rating, colour = Heathcare, 
    size = GDP), alpha = 0.5) + labs(title = "Capitalism Will Not Save Infants", 
    subtitle = "Low or High Credit Scores, Countries with Universal Healthcare Have the Lowest Infant Mortality Rates", 
    x = "Infant Mortality, deaths per 1000 live births", y = "County Credit Rating", 
    caption = "Source: World Econmic Forum, 2018") + theme(plot.caption = element_text(hjust = 0)) + 
    theme_minimal()
```

<img src="Global_Metrics_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

There are a myriad of factors that go into any one country’s outcomes in economics, healthcare, and other markers of global competitiveness. A high credit score or GDP does not always mean better outcomes in healthcare, as there are many other factors at play. One important indicator is the infant mortality rate. It is important to examine why countries might be outliers, either having a high infant mortality rate despite good economic indicators or having a surprisingly low infant mortality rate despite economic indicators.

Data Sources
============

Global Competitive Index Dataset –

<http://reports.weforum.org/global-competitiveness-report-2018/downloads/> (includes GDP, Infant Mortality, and Credit Rating)

Country’s Universal Health Care (Joined with Global Competitive Index Data) –

<https://truecostblog.com/2009/08/09/countries-with-universal-healthcare-by-date/> \*\*
