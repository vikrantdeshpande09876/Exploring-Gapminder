---
title: "Homework 2"
author: "Vikrant Deshpande, Tanvi Kolhatkar, Saishree Godbole"
date: "02/13/2022"
output: html_document
---


# INTRODUCTION
  In this project, we want to understand and assess the relationship of life expectancy to GDP per capita. For this, we have used the gapminder R package which has information about the life expectancy in 142 countries for a selection of years from 1952 to 2007. To understand the type of relationship between life expectancy and GDP, we planned as follows:
1. Check if the relationship can be fitted by a linear model and explaining any differences
2. Analyzing the trend of life expectancy over time for individual continents and investigating changes caused by respective countries
3. Checking if there are any other factors affecting the life expectancy apart from the GDP



  
install.packages(c("gapminder", "directlabels"))
require(gapminder)
require(tidyverse)
require(broom)
require(directlabels)


**Can the increase in life expectancy since World War 2 be largely explained by increases in GDP per capita?**


Q1. GDP and life expectancy in 2007: How does life expectancy vary with GDP per capita in 2007? Can the trends be well-described by a simple model such as a linear model, or is a more complicated model required? Is the pattern the same or different for every continent? If some continents are different, which ones? Can differences between continents be simply described by an additive or multiplicative shift, or is it more complicated than that?

Ans.

We have skipped Oceania from this analysis: there are just 2 countries with great GDP-per-capita values and correspondingly good Life-expectancies.

gapminder.2007 <- gapminder %>% filter((year==2007) & (continent!="Oceania"))
ggplot(gapminder.2007) + geom_point(aes(x=gdpPercap,y=lifeExp))

In the scatterplot, we see a hollow-down shape. A linear model might not be the best idea for these raw feature-values. Applying a Box-Cox transformation with higher value of Tau like T=2, should give us better linear relationships.

Residual plots to show effectiveness of a model, are in the `Appendix` section at the end of this report.


box_cox_transformation <- function(data, n){
  return (((data^n)-1)/n)
}


# Simple Linear model
gapminder_2007_linear <- gapminder.2007 %>% 
  lm(formula=lifeExp~gdpPercap)

augment(gapminder_2007_linear) %>%
  ggplot() +
  geom_point(aes(x=gdpPercap,y=lifeExp), color="darkgreen", size=1.5, alpha=0.5) + 
  geom_line(aes(x=gdpPercap,y=.fitted), color="blue", size=1.3, alpha=0.4) +
  labs(title="Simple Linear model", subtitle="Trends unexplained, not very useful", x="GDP per capita", y="Life-Expectancy")



# Polynomial Regression with degree = 2; Quadratic regression
gapminder_2007_quadratic <- gapminder.2007 %>%
  lm(formula=lifeExp~gdpPercap+I(gdpPercap^2))

augment(gapminder_2007_quadratic) %>% 
  ggplot() + 
  geom_point(aes(x=gdpPercap,y=lifeExp), color="darkgreen", size=1.5, alpha=0.5) +
  geom_line(aes(x=gdpPercap,y=.fitted), color="blue", size=1.3, alpha=0.4) +
  labs(title="Linear model of degree 2", subtitle="Trends partially explained now, but still considerably large residuals", x="GDP per capita", y="Life-Expectancy")


# Polynomial Regression with degree = 3
gapminder_2007_cubic <- gapminder.2007 %>%
  lm(formula=lifeExp~gdpPercap+I(gdpPercap^2)+I(gdpPercap^3))

augment(gapminder_2007_cubic) %>%
  ggplot() + 
  geom_point(aes(x=gdpPercap,y=lifeExp), color="darkgreen", size=1.5, alpha=0.5) +
  geom_line(aes(x=gdpPercap,y=.fitted), color="blue", size=1.3, alpha=0.4) +
  labs(title="Linear model of degree 3", subtitle="Trends better explained now, but still considerable residuals due to noisy data", x="GDP per capita", y="Life-Expectancy")





# Localized Estimated Regression : LOESS
gapminder_2007_loess <- gapminder.2007 %>%
  loess(formula=lifeExp~gdpPercap, span=0.45, degree=2)

augment(gapminder_2007_loess) %>%
  ggplot() + 
  geom_point(aes(x=gdpPercap,y=lifeExp), color="darkgreen", size=1.5, alpha=0.5) +
  geom_line(aes(x=gdpPercap,y=.fitted), color="blue", size=1.3, alpha=0.4) +
  labs(title=expression(paste("LOESS Regression model ",alpha,"= 0.45 and ",lambda,"= 2")), subtitle="Even here we can't capture all the noise in our data. We might need a very high-degree polynomial regression model", x="GDP per capita", y="Life-Expectancy")







gapminder.2007 %>%
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, color=continent)) +
  geom_point(aes(size=pop)) +
  geom_smooth(method="lm", se=F, color="blue", size=0.3) +
  facet_wrap(~continent, scales="free_x") +
  theme(axis.text.x=element_text(angle=45, hjust=1),strip.text=element_text(size = rel(1.1))) +
  labs(title="2007 Life-Expectancy vs GDP-per-capita for each continent", 
       subtitle="Oceania is excluded since it has just 2 countries",
       x="GDP per capita", y="Life Expectancy (yrs)") +
  scale_color_discrete("Continent (colors)") +
  scale_size_continuous("Population (sizes)")





Africa : We see an unfortunate scatterplot for the countries here. Most have very low GDP values, and very life-expectancies ~40-50 years. There is no clear observable linear relationship between GDP and Life-Expectancy. Hence, it doesnt make sense to use a simple linear-model here.

Americas, Asia: We see a fairly linear relationship between GDP per capita, and the Life-Expectancy. There are a few observable outliers for Americas and Asia. If we fit a simple linear-regression line, the predicted Life-Expectancy increases as GDP-per-capita increases.

Europe: Pretty uninteresting linear relationship between GDP and Life-Expectancy. There isnt a drastic change in Life-Expectancy with higher GDP of European nations. Might be because healthcare was always affordably better in the EU.

tau_param <- -2
gapminder.2007$transformed.lifeExp <- box_cox_transformation(gapminder.2007$lifeExp, tau_param)

gapminder.2007 %>%
  ggplot(mapping=aes(x=gdpPercap, y=transformed.lifeExp, color=continent)) +
  geom_point(aes(size=pop)) +
  geom_smooth(method="lm", se=F, color="blue", size=0.3, weight=aes(pop)) +
  facet_wrap(~continent, scales="free_x") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title="2007 Life-Expectancy vs GDP-per-capita for each continent",
       subtitle=expression(paste("Life-Expectancy is transformed using Box-Cox transformation: ",tau,"=-2 . Linear regressioin fits better now.")),
       x="GDP per capita", y="Life-Expectancy transformed") +
  theme(strip.text=element_text(size = rel(1.1))) + 
  scale_color_discrete("Continent (colors)") +
  scale_size_continuous("Population (sizes)")


Africa: There is still no observable linear relationship between the features. We shouldnt model the distribution for Africa using a Linear-model.

Americas, Asia, and Europe: We see a better linear relationship between the transformed Life-Expectancy, and the GDP-per-capita. Now we see fewer observable outliers for Americas and Asia, if we fit a linear-regression model.





get_quantile_plot <- function(continent1, continent2){
  c1 <- gapminder %>% subset(continent==continent1)
  c2 <- gapminder %>% subset(continent==continent2)
  return (
    qqplot(c1$lifeExp, c2$lifeExp, plot.it=FALSE) %>%
      as.data.frame() %>%
      ggplot(aes(x=x, y=y)) +
      geom_point(color="dark green", alpha=0.5) +
      theme_bw() +
      labs(title="Life Expectancies",subtitle=paste(continent1," vs ",continent2), x=continent1, y=continent2)
    )
}

grid.arrange(
  get_quantile_plot("Europe", "Americas"),
  get_quantile_plot("Europe", "Africa"),
  get_quantile_plot("Asia", "Europe"),
  get_quantile_plot("Africa", "Asia"),
  get_quantile_plot("Asia", "Americas"),
  get_quantile_plot("Americas", "Africa"),
  ncol = 3)


########### PEER REVIEW NEEDED ###########

The QQ-Plots of Life-Expectancies for Asia vs Africa, and Americas vs Asia, are **arguably** straight lines barring one/two outlier points. Essentially- distribution of life expectancy in Asia, Africa, and Americas, can be explained with some additive or multiplicative shift.

There appears to be true for QQ plot distribution of life expectancy in Asia and Europe. The remaining QQ plots show quite complex relationships (the distributions are quite different) which cannot be merely described by simple additive or multiplicative shifts.






################################################################################################












Q2. Life expectancy over time by continent: How has average life expectancy changed over time in each continent? Have some continents caught up (at least partially) to others? If so, is this just because of some countries in the continent, or is it more general? Have the changes been linear, or has it been faster/slower in some periods for some continents? What might explain periods of faster/slower change?

Ans.



To answer this question we have plotted the average life expectancy for all continents over the years 1950-2010. On looking at the plot, we can see the life expectancies in most continents have been linearly increasing with time. 
Continents of Europe and Americas started at high average life expectancies and have had a steady increase over the years. Asia and Africa were the two continents with very low life expectancy in 1950. Over time, notwithstanding a dip around 1962, Asia has caught up with the other continents at a fast rate. 

Average life expectancy in Africa has had a steady but relatively small increase from 30-50 to 40-60.

#average life exp change over time for each continent
gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp=weighted.mean(x=lifeExp, w=pop, na.rm=T)) %>%
  ggplot(aes(x =year, y=lifeExp, colour=continent)) +
  geom_line(size=1.2) +
  labs(title="Yearly Average Life-Expectancy for each continent",
       subtitle="Y-axis shows weighted-average of Life-Expectancy",
       x="Year", y="Life Expectancy") +
  scale_color_discrete("Continent")


gapminder %>% 
  filter(continent!="Oceania") %>%
  ggplot(aes(x=year, y=lifeExp, group=country, color=country)) +
  geom_line(lwd=1, show.legend=F)+
  facet_wrap(~continent) +
  scale_color_manual(values=country_colors) +
  theme(strip.text=element_text(size = rel(1.1)))




#Have some continents caught up (at least partially) to others? 
#If so, is this just because of some countries in the continent, or is it more general?
#deep dive into Asia as it has caught up to other continents
library(directlabels)
gapminder %>%
  filter(continent=="Asia") %>%
  ggplot(aes(x=year, y=lifeExp, group=country, color=country)) +
  geom_line(size=1.5, show.legend=F) +
  geom_dl(aes(label=country), method=list("last.points"))

In Asia, the dip in average life expectancy (around 1962) can be attributed to a dip in China. China has the largest population in Asia and contributes heavily to the weighted average life expectancy of the entire continent. The causes of these changes could be attributed socio-economic changes and natural disasters such as famines occurring around 1962. Apart from this, the majority of the countries have had a steady growth in their life expectancy which has contributed to the overall growth of Asia.























################################################################################################
















Q3. Changes in the relationship between GDP and life expectancy over time: How has the relationship between GDP and life expectancy changed in each continent? Can changes in life expectancy be entirely explained by changes in GDP per capita? Does it look like there's a time effect on life expectancy in addition to a GDP effect? Has there been "convergence" in the sense that perhaps GDP and/or continent don't matter as much as it used to? Are there exceptions to the general patterns?

Ans. 


**Changes in the relationship between GDP and life expectancy over time**

Africa: From Ans.1, we already know that there is not much of a linear-relationship between GDP and Life-Expectancy, and in Ans.2 we saw that there 
Americas, Asia, Europe, Oceania: GDP and life expectancy have a more or less linear relationship. Looking at the example of Africa, we cannot entirely attribute the changes to be due to the GDP. We can see that time also contributes to the increase in life expectancy.

#Pending:
  # Has there been "convergence" in the sense that perhaps GDP and/or continent don't matter as much as it used to?
  # Are there exceptions to the general patterns?


gapminder %>% 
  mutate(
    cat.lifeExp=if_else(lifeExp<50, "Very low", if_else(lifeExp<60, "Average", "Great")),
    color.lifeExp=if_else(lifeExp<50, "gray", if_else(lifeExp<60, "red", "green")),
    ) %>%
  ggplot(aes(x=year, y=gdpPercap, color=color.lifeExp)) +
  geom_jitter(shape='o', width=.7, size=2.5) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~continent, ncol=2) +
  labs(title="Yearly GDP-per-capita for each continent",
       x="Year", y="GDP-per-capita", ) +
  scale_colour_discrete(name="Life-Expectancy categories",
                      breaks=c("gray", "red", "green"),
                      labels=c("Very Low: x<50", "Average: 50<x<60", "Great: x>60"))


# CONCLUSION

# APPENDIX


plot(gapminder_2007_linear, 1, main="Residuals plot for Q1. Simple Linear Regression")

plot(gapminder_2007_quadratic, 1, main="Residuals plot for Q1. Polynomial Linear Regression with degree 2")

plot(gapminder_2007_cubic, 1, main="Residuals plot for Q1. Polynomial Linear Regression with degree 3")
