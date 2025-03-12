---
title: "Exploring the correlation between life expectation and economic growth"
categories: ["EDA", "ggplot2", "R", "visualization"]
description: "Visualizing Hans Rosling's TED talk on the relationship between life expectation and economic growth with ggplot2 and gapminder"  
date: 2025-03-11
title-block-banner: true
page-layout: article
execute: 
  message: false
  warning: false
format:
  html:
    code-fold: false
    code-tools: true
freeze: true
image: image.png
---

On his lecture "[The Best Stats You Have Ever Seen](https://www.ted.com/talks/hans_rosling_the_best_stats_you_ve_ever_seen/transcript)", statistician Hans Rosling showed data on the evolution of life expectation and GDP per capita around the world. In this post, we will use the `gapminder` package to access the data and `ggplot2` and `tidyverse` to explore it.

### Setting Up

#### Load packages

```{r}
library(pacman)


pacman :: p_load("gapminder",
                 "dplyr",
                 "tidyverse",
                 "ggrepel",
                 "showtext",
                 "DT",
                 "Hmisc",
                 "plotly")
```

#### Load data

```{r}
data("gapminder")
data("continent_colors")
```

#### Explore data

```{r}
gapminder %>%
  summarise(across(everything(), ~sum(is.na(.x)))) # check for NA values
```

```{r}
str(gapminder) # check internal structure 
```

```{r}
gapminder <- mutate(gapminder, lgdppc = log10(gdpPercap)) # create a log column for gdp per capita 
```

```{r}
describe(gapminder) # a more detailed summary()
```

### Analysing the Data

Now that we have a better understanding of the data, we can better analyse it. To begin, let's build two tables to see which continents had the highest absolute and relative growth.

```{r}
gapminder %>%
  group_by(continent) %>%
  summarise(growth = last(gdpPercap) - first(gdpPercap)) %>%
  arrange(desc(growth)) %>%
  datatable(caption = "Absolute GDP Growth by Continent (1952-2007)")
```

```{r}
gapminder %>%
  group_by(continent) %>%
  summarise(growth = (last(gdpPercap) / first(gdpPercap) - 1) * 100) %>%
  arrange(desc(growth)) %>%
  datatable(caption = "Relative GDP Growth by Continent (1952-2007)")
```

The absolute growth of Oceania surprised me, but, when we look into the data, we see that only Australia and New Zealand are featured in the dataset.

```{r}
gapminder %>% 
  filter(continent == "Oceania") %>% 
  count(country) %>% 
  datatable
```

We can do the same to find out the highest growth in GDP per capita per country.

```{r}
gapminder %>% 
  group_by(country) %>%
  summarise(growth = last(gdpPercap) - first(gdpPercap)) %>%
  arrange(desc(growth)) %>%
  datatable(caption = "Absolute GDP Growth by Country (1952-2007)")
```

```{r}
gapminder %>% 
  group_by(country) %>% 
  summarise(growth = (last(gdpPercap) / first(gdpPercap) - 1) * 100) %>%
  arrange(desc(growth)) %>%
  datatable(caption = "Relative GDP Growth by Country (1952-2007)")
```

Now, let's take a look into life expectancy by country

```{r}
gapminder %>%
  group_by(country) %>%
  summarise(life_growth = last(lifeExp) - first(lifeExp)) %>%
  arrange(desc(life_growth)) %>%
  datatable(caption = "Absolute Life Expectancy Growth by Country (1952-2007)") %>% 
  formatRound(columns = "life_growth", digits = 2)
```

```{r}
gapminder %>%
  group_by(country) %>%
  summarise(life_growth = (last(lifeExp) / first(lifeExp) - 1) * 100) %>%
  arrange(desc(life_growth)) %>%
  datatable(caption = "Relative Life Expectancy Growth by Country (1952-2007)") %>% 
  formatRound(columns = "life_growth", digits = 2)
```

We can also figure out which was the largest variations in the sample.

```{r}
gapminder %>%
  group_by(country) %>%
  mutate(
    life_diff = lifeExp - lag(lifeExp),
    life_abs  = abs(lifeExp - lag(lifeExp))) %>%
  ungroup() %>%
  arrange(desc(life_abs)) %>%
  select(country, year, life_diff) %>%
  slice(1:10) %>%
  datatable(caption = "") %>% 
  formatRound(columns = "life_diff", digits = 2)
```

### Visualization

#### Aesthetics

Before we begin plotting our visualizations, we can set a theme and a few personalized aesthetic values for our plots.

```{r}
font_add_google("Montserrat", "Montserrat") # defining a font 
showtext_auto()

mytheme <- theme_minimal() +
  theme(
    text = element_text(family = "Montserrat", size = 14, colour = "gray20"),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
    ) # create a custom theme
```

#### Relationship between GDP per capita and life expectancy (2007)

```{r}
gap2007 <- filter(gapminder, year == 2007)

ggplot(gap2007, aes(lgdppc, lifeExp)) +
         geom_point() +
         geom_smooth(method = "lm", se = FALSE) + 
         labs(
           title = "Life Expectancy X GDP Per Capita, 2007",
           subtitle = "Relationship between life expactancy and GDP per capita",
           caption = "Source: Gapminder and World Bank Open Data",
           x = "GDP Per Capita (Log, US$2010)",
           y = "Life expectancy at birth") + 
  mytheme
```

#### Life expectancy world average

```{r}
gap_life <- gapminder %>%
  group_by(year) %>%
  summarise(mean = mean(lifeExp))

ggplot(gap_life, aes(year, mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Life Expectancy X GDP Per Capita (World Average)",
    caption = "Source: Gapminder and World Bank Open Data",
    x = "GDP Per Capita (Log, US$2010)",
    y = "Life expectancy at birth") +
    mytheme
    
```

#### Life expectancy continent average

```{r}
gap_life_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarise(mean = mean(lifeExp))

ggplot(gap_life_continent, aes(year, mean, colour = continent)) + 
  geom_line() +
  geom_point() +
  labs(
    title = "Average life expectancy (1952-2007)",
    caption = "Source: Gapminder and World Bank Open Data",
    x = "",
    y = "Life expectancy at birth") +
    mytheme
```

#### GDP per capita continent average

```{r}
gap_gdp <- gapminder %>%
  group_by(year, continent) %>%
  summarise(avg_gdp = mean(gdpPercap))

ggplot(gap_gdp, aes(year, avg_gdp, colour = continent)) + 
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(
    title = "Average GDP per capita (1952-2007)",
    caption =  "Source: Gapminder and World Bank Open Data",
    x = "",
    y = "GDP Per Capita (Log, US$2010)"
  ) +
  mytheme
```

#### Selected countries

Now, let's select the five countries with largest absolute GDP per capita growth and see how their life expectancy behaved.

```{r}
c <- c("Oman", "Vietnam", "Indonesia", "Saudi Arabia", "Libya")
c_gapminder <- filter(gapminder, country %in% c)

ggplot(gapminder, aes(lgdppc, lifeExp)) +
    geom_line(
        data = gapminder,
        aes(x = lifeExp, y = gdpPercap, group = country),
        colour = "gray60",
        alpha = 0.5
    ) + 
    geom_point(
        data = c_gapminder,
        aes(x = lifeExp, y = gdpPercap, colour = country)
    ) +
    geom_line(
        data = c_gapminder,
        aes(x = lifeExp, y = gdpPercap, color = country),
        linewidth = 1
    ) +
    scale_y_log10(labels = scales::comma) +  # Use comma format for labels
    labs(
        title = "Life Expectancy X GDP Per Capita (Selected Countries",
        caption = "Source: Gapminder and World Bank Open Data",
        x = "Life Expectancy",
        y = "GDP Per Capita (Log, US$2010)"
    ) + 
    mytheme

```

#### Interactive plot

```{r}
info <- list(
  symbol = "circle",
  sizemode = "diameter",
    line = list(width = 2, color = '#FFFFFF')
  )

plot_ly(
  data = gapminder,
  x = ~lgdppc,
  y = ~lifeExp,
  color = ~continent,
  colors = continent_colors,
  size = ~pop,
  frame = ~year,
  type = "scatter",
  mode = "markers",
  text = ~paste(
    "Country:", country,
    "<br>Life Expectancy:", round(lifeExp, 2),
    "<br>GDP per capita:", round(gdpPercap, 2),
    "<br>Pop.:", format(pop, big.mark = ","))
) %>%
  layout(
    title = "Life Expectation X GDP Per Capita",
    xaxis = list(title = "GDP per capita (US$, log)"),
    yaxis = list(title = "Life Expectancy (years)")
  )
```
