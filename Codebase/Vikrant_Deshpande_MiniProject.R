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
require(gridExtra)


**Can the increase in life expectancy since World War 2 be largely explained by increases in GDP per capita?**
  
  
Q1. GDP and life expectancy in 2007: How does life expectancy vary with GDP per capita in 2007? Can the trends be well-described by a simple model such as a linear model, or is a more complicated model required? Is the pattern the same or different for every continent? If some continents are different, which ones? Can differences between continents be simply described by an additive or multiplicative shift, or is it more complicated than that?
  
  Ans.

We have skipped Oceania from this analysis: there are just 2 countries with great GDP-per-capita values and correspondingly good Life-expectancies.


# Basic Box-Cox function
box_cox_transformation <- function(data, n){
  if (n==0) {return (log(data))}
  return ((data^n-1)/n)
}


tau_param <- 0
gapminder.2007 <- gapminder %>% filter((year==2007) & (continent!="Oceania"))

gapminder$transformed.gdpPercap <- box_cox_transformation(gapminder$gdpPercap, tau_param)
ggplot(gapminder.2007) + 
  geom_point(aes(x=transformed.gdpPercap,y=lifeExp), color="darkgreen", size=1.5, alpha=0.5) + 
  labs(title="Life-Expectancy vs GDP-per-capita", subtitle="Note the parabolic shape- would a Quadratic Linear Regression model work?", x="GDP per capita", y="Life-Expectancy")

In the scatterplot, we dont see a straightforward relationship between Life-Expectancy and GDP per capita of nations. A linear model might not be the best idea for these raw feature-values.

Residual plots to show effectiveness of a model, are in the `Appendix` section at the end of this report.

# Simple Linear model
gapminder_2007_linear <- gapminder.2007 %>% 
  lm(formula=lifeExp~transformed.gdpPercap)


# # Polynomial Regression with degree = 2; Quadratic regression
# gapminder_2007_quadratic <- gapminder.2007 %>%
#   lm(formula=lifeExp~transformed.gdpPercap+I(transformed.gdpPercap^2))
# 
# 
# # Polynomial Regression with degree = 3
# gapminder_2007_cubic <- gapminder.2007 %>%
#   lm(formula=lifeExp~transformed.gdpPercap+I(transformed.gdpPercap^2)+I(transformed.gdpPercap^3))
# 
# 
# # Localized Estimated Regression : LOESS
# gapminder_2007_loess <- gapminder.2007 %>%
#   loess(formula=lifeExp~transformed.gdpPercap, span=1, degree=2)


# Function to carve model-outputs in required format
get_model_features <- function(model, model_type) {
  data <- augment(model) %>% 
    select(transformed.gdpPercap, .fitted, lifeExp) %>%
    mutate(model=model_type)
  return (data)
}

# get_model_features(gapminder_2007_linear, "Simple Linear") %>%
#   union(get_model_features(gapminder_2007_quadratic,"Quadratic: Degree 2")) %>%
#   union(get_model_features(gapminder_2007_cubic,"Cubic: Degree 3")) %>%
#   union(get_model_features(gapminder_2007_loess, "LOESS: span= 1 and degree= 2")) %>%
#   ggplot() +
#   geom_point(aes(x=transformed.gdpPercap,y=lifeExp), color="darkgreen", size=1.8, alpha=0.8) +
#   geom_line(aes(x=transformed.gdpPercap, y=.fitted, color=model), size=1.5, alpha=0.6) +
#   scale_color_discrete(name="Model-Type") +
#   labs(
#     title=paste("Variations of Linear Regression models fit to the noisy data"), 
#     subtitle="We can't accurately capture all the trends in our data due to noise. \nWe might need a very high-degree polynomial regression model",
#     x="GDP-per-capita", y="Life-Expectancy") +
#   theme(axis.text.x=element_text(angle=45, hjust=1),strip.text=element_text(size = rel(1.1))) +
#   theme_bw()
# 


get_model_features(gapminder_2007_linear, "Simple Linear") %>%
  ggplot() +
  geom_point(aes(x=transformed.gdpPercap,y=lifeExp), color="darkgreen", size=1.8, alpha=0.8) +
  geom_line(aes(x=transformed.gdpPercap, y=.fitted, color=model), size=1.5, alpha=0.6) +
  scale_color_discrete(name="Model-Type") +
  labs(
    title=paste("Linear Regression fit to the noisy data"), 
    subtitle="Transformed GDP to a log-scale: still can't accurately capture all trends in data due to noise.",
    x="Transformed GDP-per-capita", y="Life-Expectancy (yrs)") +
  theme(axis.text.x=element_text(hjust=1),strip.text=element_text(size = rel(1.1))) +
  theme_bw()





--- HOW ARE SOME CONTINENTS DIFFERENT THAN OTHER IN TERMS OF LIFE-EXPECTANCY VS GDP PATTERNS?
  
gapminder.2007 %>%
  ggplot(mapping=aes(x=transformed.gdpPercap, y=lifeExp, color=continent)) +
  geom_point(aes(size=pop)) +
  geom_smooth(method="lm", se=F, color="blue", size=0.3) +
  facet_wrap(~continent, scales="free_x") +
  theme(axis.text.x=element_text(hjust=1),strip.text=element_text(size = rel(1.1))) +
  labs(title="2007 Life-Expectancy vs Log-GDP-per-capita for each continent", 
       subtitle="Linear regression doesn't fit well for Africa",
       x="Transformed GDP-per-capita", y="Life Expectancy (yrs)") +
  scale_color_discrete("Continent (colors)") +
  scale_size_continuous("Population (sizes)")





Africa : We see an unfortunate scatterplot for the countries here. Most have very low GDP values, and very life-expectancies ~40-50 years. There is no clear observable linear relationship between GDP and Life-Expectancy. Hence, it doesnt make sense to use a simple linear-model here.

Americas, Asia: We see a fairly linear relationship between GDP per capita, and the Life-Expectancy. There are a few observable outliers for Americas and Asia. If we fit a simple linear-regression line, the predicted Life-Expectancy increases as GDP-per-capita increases.

Europe: Pretty uninteresting linear relationship between GDP and Life-Expectancy. There isnt a drastic change in Life-Expectancy with higher GDP of European nations. Might be because healthcare was always affordably better in the EU.






--- HOW ARE DIFFERENCES OF LIFE-EXPECTANCY VS GDP PATTERNS EXPLAINED FOR EACH CONTINENT? ADDITIVE/MULTIPLICATIVE SHIFT?
# 
# get_quantile_plot <- function(continent1, continent2){
#   c1 <- gapminder %>% subset(continent==continent1)
#   c2 <- gapminder %>% subset(continent==continent2)
#   return (
#     qqplot(c1$lifeExp, c2$lifeExp, plot.it=FALSE) %>%
#       as.data.frame() %>%
#       ggplot(aes(x=x, y=y)) +
#       geom_point(color="dark green", alpha=0.5) +
#       theme_bw() +
#       labs(title=paste0(continent2," vs ",continent1), x=continent1, y=continent2)
#   )
# }
# 
# 
# 
# #   get_quantile_plot("Americas", "Africa"),
#   ncol = 3)


########### PEER REVIEW NEEDED ###########

The QQ-Plot of Life-Expectancies for Americas vs Asia is **arguably** a straight line, barring one/two outlier points. Essentially, the distribution of life expectancy in Americas and Asia can be explained with some additive shift since this straight line lies above the usual 45 degrees reference line.

The remaining QQ plots show quite complex relationships (the distributions are quite different) which cannot be merely described by simple additive or multiplicative shifts.






################################################################################################












Q2. Life expectancy over time by continent: How has average life expectancy changed over time in each continent? Have some continents caught up (at least partially) to others? If so, is this just because of some countries in the continent, or is it more general? Have the changes been linear, or has it been faster/slower in some periods for some continents? What might explain periods of faster/slower change?
  
  Ans.



To answer this question we have plotted the average life expectancy for all continents over the years 1950-2010. On looking at the plot, we can see the life expectancies in most continents have been linearly increasing with time. 
Continents of Europe and Americas started at high average life expectancies and have had a steady increase over the years. Asia and Africa were the two continents with very low life expectancy in 1950. Over time, notwithstanding a dip around 1962, Asia has caught up with the other continents at a fast rate. 

Average life expectancy in Africa has had a steady but relatively small increase from 30-50 to 40-60.

#average life exp change over time for each continent
gapminder.avg_trends <- gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp=weighted.mean(x=lifeExp, w=pop, na.rm=T)) 

gapminder.avg_trends %>%
  ggplot(aes(x =year, y=lifeExp, colour=continent)) +
  geom_line(size=1.2, show.legend=F) +
  labs(title="Yearly Average Life-Expectancy for each continent",
       subtitle="Y-axis shows weighted-average of Life-Expectancy based on Population",
       x="Year", y="Life Expectancy") +
  geom_dl(aes(label=continent), method=list("last.points"))


gapminder %>% 
  filter(continent!="Oceania") %>%
  ggplot(aes(x=year, y=lifeExp, group=country, color=country)) +
  geom_line(lwd=1, show.legend=F)+
  facet_wrap(~continent) +
  scale_color_manual(values=country_colors) +
  labs(title="Yearly Life-Expectancy for countries in each continent",
       x="Year", y="Life Expectancy") +
  theme(strip.text=element_text(size = rel(1.1)))




#Have some continents caught up (at least partially) to others? 
#If so, is this just because of some countries in the continent, or is it more general?
#deep dive into Asia as it has caught up to other continents
gapminder.mod <- gapminder %>%
  filter(continent=="Asia") %>%
  mutate(
    color.country=if_else(country=='China','red',if_else(country=='Cambodia','navyblue','brown')),
    visibility.country=if_else(country=='China',1,if_else(country=='Cambodia',1,0.6)),
    label.country=if_else(country=='China','China',if_else(country=='Cambodia','Cambodia','')),
    )

colors <- as.character(gapminder.mod$color.country)
names(colors) <- as.character(gapminder.mod$country)



gapminder.avg_trends_asia <- gapminder.avg_trends %>%
  filter(continent=='Asia') %>%
  mutate(country='Overall Asia')


gapminder.mod %>%
  ggplot(aes(x=year, y=lifeExp, group=country, color=country)) +
  geom_line(aes(alpha=visibility.country), size=1.6, show.legend=F) +
  scale_color_manual(values=colors) +
  labs(title="Yearly Life-Expectancies for Asia focusing on significant countries",
       subtitle="Asia's overall trend impacted more by China, than Cambodia",
       x="Year", y="Life Expectancy") +
  geom_dl(aes(label=label.country), method=list("first.points", "last.points")) #+
  #geom_line(data=gapminder.avg_trends_asia, mapping=aes(x=year,y=lifeExp), color='darkgreen', size=3.0, alpha=0.5)

In Asia, the dip in average life expectancy (around 1962) can be attributed to a dip in China. China has the largest population in Asia and contributes heavily to the weighted average life expectancy of the entire continent. The causes of these changes could be attributed socio-economic changes and natural disasters such as famines occurring around 1962. Apart from this, the majority of the countries have had a steady growth in their life expectancy which has contributed to the overall growth of Asia.























################################################################################################
















Q3. Changes in the relationship between GDP and life expectancy over time: How has the relationship between GDP and life expectancy changed in each continent? Can changes in life expectancy be entirely explained by changes in GDP per capita? Does it look like theres a time effect on life expectancy in addition to a GDP effect? Has there been "convergence" in the sense that perhaps GDP and/or continent dont matter as much as it used to? Are there exceptions to the general patterns?
  
Ans. 


----------- CHANGES IN THE RELATIONSHIP BETWEEN GDP AND LIFE EXPECTANCY OVER TIME FOR EACH CONTINENT
  

# Trivariate co-plot with 'given'=year
# gapminder %>% 
#   ggplot(aes(x=transformed.gdpPercap, y=lifeExp, color=continent)) +
#   geom_jitter(width=.7, size=2.5, alpha=0.5) + 
#   geom_smooth(method="loess", se=F, size=1, alpha=0.9, span=1, method.args=list(degree=2)) +
#   theme(axis.text.x=element_text(hjust=1), strip.text=element_text(size=rel(1.1))) +
#   facet_wrap(~year, ncol=3) +
#   labs(
#     title="Life-expectancy vs GDP-per-capita for all continents from 1952-2007", 
#     subtitle="LOESS regression lines created using span=1, degree=2",
#     x="Log GDP-per-capita", 
#     y="Life-expectancy") +
#   scale_color_discrete("Continents")


gapminder %>% 
  ggplot(aes(x=transformed.gdpPercap, y=lifeExp, color=continent)) +
  geom_jitter(width=.7, size=2.5, alpha=0.5) + 
  geom_smooth(method="lm", se=F, size=1, alpha=0.9) +
  theme(axis.text.x=element_text(hjust=1), strip.text=element_text(size=rel(1.1))) +
  facet_wrap(~year, ncol=3) +
  labs(
    title="Life-expectancy vs Log-GDP-per-capita for all continents from 1952-2007", 
    subtitle="Linear regression lines for developed continents, seem to converge to a good life-expectancy\nas we reach 2007",
    x="Log-GDP-per-capita", 
    y="Life-expectancy") +
  scale_color_discrete("Continents")


  Each continent has a regression line with a positive slope indicating that for higher GDP-per-capita, Life-Expectancy is higher on average. For each facet of year, note that some points lie below the linear-regression line, and this can be attributed to "regression-to-the-means".
Important Observations:
  Africa in 1952 had a regression line almost parallel to the X-axis: life-expectancy was just in general low there, irrespective of GDP. As we move through time till 2007, we see the slope change to a more positive outlook.
  Europe and Asia have somewhat of parallel regression-lines in 1952, with positive slopes. Over time, we see these lines converge towards a fantastic life-expectancy of approximately 80 years.
  Americas and Europe seems to have an ideal regression-line with a small positive slope that points to a better Life-Expectancy for countries with higher GDP-per-capita. As we move from 1952 to 2007, the regression-lines for Europe, Asia, and Americas seemingly get merged into the same line (almost parallel to X-axis) indicative of this idealistic hypothesis.
  This might be proof that after 2010, such continents with developed nations, will have a regression-line with small slope converging to a life-expectancy of 80.



-------------- CHANGES IN LIFE EXPECTANCY CANT BE ENTIRELY EXPLAINED BY CHANGES IN GDP
# Trivariate co-plot with 'given'=continent
gapminder %>% 
  filter(continent!="Oceania") %>%
  ggplot(aes(x=year, y=transformed.gdpPercap, color=lifeExp)) +
  geom_jitter(width=.7, size=3, alpha=0.7) + 
  theme(axis.text.x=element_text(angle=45, hjust=1), strip.text=element_text(size=rel(1.1))) +
  theme_bw() +
  facet_wrap(~continent, ncol=2) +
  labs(title="Log-GDP-per-capita for each continent vs Time",
       subtitle="Time does play a factor in increase of life-expectancy",
       x="Year", 
       y="Log-GDP-per-capita") +
  scale_color_gradient("Life\nExpectancy", guide="colorbar", low="red", high="#00ab0e")


Changes in life-expectancy cant be entirely explained by just changes in GDP. There does seem to be a time-factor involved in this.
For example, within Africa and Americas, even though the GDP of countries stays similar (0 to 15000), the Life-Expectancy just seems to get better. In other words, the number of green dots increases from left to right even without a big jump in GDP.

# CONCLUSION

# APPENDIX


plot(gapminder_2007_linear, 1, main="Residuals plot for Q1. Simple Linear Regression\nNo heteroskedasticity: Just noisy data\n")
# 
# plot(gapminder_2007_quadratic, 1, main="Residuals plot for Q1. Polynomial Linear Regression with degree 2")
# 
# plot(gapminder_2007_cubic, 1, main="Residuals plot for Q1. Polynomial Linear Regression with degree 3")