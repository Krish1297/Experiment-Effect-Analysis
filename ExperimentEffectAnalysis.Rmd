---
title: "ExperimentalEffectsLinearModels"
author: "Krishnamoorthy Juttoo Chandrasekaran"
date: "04/11/2023"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# PART 1: Analyzing Effects Accross Groups with Linear Models

So far, we already encountered the basic ideas for experiments: We know that the most basic analysis involves a comparison, and that comparison is often done between groups. For example, you already analyzed if female or male product testers had better impression of a product or used language with a different sentiment when making a statement about the tested product (see exercise 2). You also compared the differences in brand recognition and remembered social media contacts between two groups of survey respondents, one before marketing campaign and one after a marketing campaign  (see exercise 3). Now, in the latter case, you found that you needed to be very careful about the groups' structures. This is because a group comparison is only valid, if the groups are very similar. If they are not, then the differences between the groups might be due to the group structure, and not due to the treatment (the marketing campaign). In the latter case, we would say that the group structure is a confounding variable.

For these reasons we often use experiments to compare groups. In experiments, we can control the group structure (because a random generator assigns respondents/users to the groups), and we can also control the treatment. This means that we can make sure that the groups are very similar, and that the only difference between the groups is the treatment. This is why experiments are often called the "gold standard" of research.

## Analyzing Experiments with Linear Models

In this exercise, we will learn how to analyze experiments with linear models. We will also learn how to analyze the data with linear models, and how to interpret the results. We will also learn how to test for so-called moderation/interaction/context effects, and how to interpret the results.

In the lectures, you may have already seen results from a field experiment where an online flower/plant retailer tested the effect of product images (single v. ensemble) and the placement (start page v. control) on the purchase likelihood. In the lecture, we may have used the data for the classic analysis of experiments with ANOVA or ANCOVA, with subsequent post-hoc tests (multiple t-tests) of group differences. These tests and the associated interaction plots showed an interaction effect (ensemble pictures appear better, at least on the start page). We therefore assume that the placement as moderator changes (moderates) the average effect of the image type.
If you cannot remember the data, here is the explanation of the data set and the experiment:
The online retailer for plants and flowers can show different pictures of the products. The pictures can either show a single plant, or an ensemble of plants. Furthermore, the start page of the online shop is the most important (first) touch point with a user. Is it better on the start page to show a user single products only so they can immediatly find what they look for? Or is it better to show flower and plant ensembles on the start page to inspire the user and show the variety of possible products and how thy may be used together? 
The retailer tested if the (randomly shown) type of the picture and the (random) placement of the picture (on the start page or on the category page) has an effect on the purchase likelihood. The purchase likelihood is the dependent variable. The independent variables are the picture type (single v. ensemble) and the placement (start page v. category page). The data set is called "PlantsEcomm_forR.xlsx" and is available on the course website.

Here is the explanation of the data set:
  * Variable "photo": a customer always happened to see different plants, but once:
   * individually, or
   * as an ensemble/flower arrangement
  * Variable "placement": The image of the plants also appeared either: 
   * featured on the landing page
   * only in the usual category subpage (control group)
  * Variable "price_eur": the price of the product, likely an important control variable for purchase probability
  * Variable "purchase_likelihood": The dependent variable is the probability of purchase.




### Reading in the data - from an Excel file


```{r}
if(!require(readxl)){install.packages("readxl")};library(readxl) #Package to read xlsx files

# this is the experimental data:
#Plants/Flowers Ecommerce store experiments with single plant vs. plant ensemble photos, and featured vs. normal webshop position.
experiment.data <- as.data.frame(read_xlsx("PlantsEcomm_forR.xlsx", sheet= 1, col_names = TRUE))

```

### visualize for first inspection

Let's take a first look at how our possible 4 groups differ in their purchase likelihood. There are 4 groups as a result of two independent, random experimental factors (picture type and placement), each with two options. Therefore the groups are alone_&_start, ensemble_&_start, alone_&_category, ensemble_&_category. We first plot the group means in an interaction plot:



```{r}

if(!require(ggplot2)){install.packages("ggplot2")};library(ggplot2) #Package for fancy graphs

  ggplot() + 
    theme_classic() +
    aes(x = experiment.data$placement, color = experiment.data$photo, group = experiment.data$photo, y = experiment.data$purchase_likelihood) +
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.y = mean, geom = "line")+
      labs(title="Interaction plot", 
       subtitle="the four groups differ in their purchase likelihood", 
       y="purchase likelihood", 
       x="Placement", 
       caption="source: Experiment dataset")

```
The plot shows that the groups differ in their purchase likelihood. However, it looks like the placement plays an important role, too. When looking at control page placements, it does not seem to matter what kind of picture is used. But on the start page, the ensemble pictures seem to be better than the single pictures (and both better than on control pages).

But is this difference statistically significant? And how can we test this? What is the distribution in the groups?

We will use a boxplot for a first look. The boxplot shows the distribution of the purchase likelihood for each group. The boxplot also shows the median (the line in the box), the 25% and 75% quantiles (the box), and the minimum and maximum values (the whiskers). The boxplot also shows outliers (the dots). 

```{r}


if(!require(ggplot2)){install.packages("ggplot2")};library(ggplot2) #Package for fancy graphs
#visual inspection of distributions by experimental groups --> no strong visual difference
ggplot(experiment.data, aes(x = photo, y = purchase_likelihood, fill = placement)) +
  geom_boxplot()

```
Apparently, the distribution of the purchase likelihood is not very different between the groups. The median is very similar, and the 25% and 75% quantiles are also very similar. The whiskers are also very similar. Maybe the variance is too large to find a clear significant effect here. Lets keep this observation in mind and try to do our first test using linear models

### Linear models

```{r}
#linear models:

#M1: direct effects of placement and photo type only
m1 <- lm( purchase_likelihood ~ photo + placement, data=experiment.data)

summary(m1)
```
We do not have a very convincing model here. The R-squared is very low, and the p-values are not significant. The model does not explain much of the variance in the purchase likelihood. 

But wait, we forgot that there may be an interaction between the variables here: ensemble pictures seemed better on the start page. in a model, this should be an interaction effect between the two variables. Lets add this to the model:

```{r}
#M2: direct and interaction effects of placement and photo type (two-way ANOVA no controls)
m2 <- lm( purchase_likelihood ~ photo * placement, data=experiment.data)
summary(m2)
```
Again, nothing significant. The interaction effect is not significant, and the R-squared is still very low.

Maybe we should add some control variables to the model. The price of the product is likely an important control variable. Lets add it to the model:

```{r}
#M3: direct and interaction effects of placement and photo type with controls
m3 <- lm( purchase_likelihood ~ photo * placement + price_eur, data=experiment.data)
summary(m3)
```
Ah, finally! The price explains a lot of variance in the purchase likelihood. The R-squared is much higher now. 
With more of the variance explained by price, the interaction effect is now significant. The ensemble pictures are indeed significantly better on the start page. And also the general start-page effect is significant (but smaller than the interaction).

For a better side-by-side comparison, let us create a table of the models:

### Model comparison tables

This is important: always compare models side by side in tables, both in your thesis (you see it done in academic papers for a reason) and in the backup slides for a practical presentation. The reason is quite simple: putting all your models in one table allows you to compare them easily. If you have to flip through slides, you will not be able to compare them easily. A good model table also tells a story of discovery (here from too simple to find anything to a more complex model that explains a lot of variance). Or it could tell a story of robustness ("look, whatever we try to model diffwerently, the main results do not change much")



```{r}

# Now: output in model table

if(!require(texreg)){install.packages("texreg")};library(texreg) #Package for model overview tables; the useful function here is screenreg()


screenreg(list(m1, 
               m2,
               m3),
          caption="Linear models of the experimental effects", 
          dcolumn=FALSE,
          digits=2,                            # digits for rounding
          leading.zero = TRUE,                # do you want to show a 0 before a 0.23, or only .23?
          custom.model.names=c("Direct effects", "with interaction", "with interaction and controls"))

```

In this comparison, we see, for example, that including the price variable merely explained additional variance, allowing the other effects to become significant, but did not change the estimates of the interaction effect. The estimate is still at 7.76, meaning that on the start page the ensemble pictures are 7.76 percentage points in purchase probability better than the single pictures on the start page. on all other pages, there is no difference between the two picture types. All picture types are better on the start page, increasing the purchase probability by 1.82 percentage points.



# PART 2: Analyzing observational data with linear models

Quite often, companies did not design an actual experiment with randomized treatments. However, companies do change what they do to their customers on a regular basis, and that often includes some more or less well informed trial and error. This is a great opportunity to use observational data to learn about the effects of different treatments. (Be careful, however, to label them causal effects. Causality is a tricky issue, as we will see later in this course).

If you want to gain a first impression of what you (or others) did in the pased had any effect on an outcome of interest, you can again use linear models. The basic idea is the same as in the experimental case: you want to explain the outcome of interest with the treatment variable, and you want to control for other variables that may influence the outcome.

Lets look again at the example from R exercise 2, the customer satisfaction with an e commerce store. Back then, we used it for a cluster analysis, no we want to explore what factors are associated with customer satisfaction.

## The customer satisfaction dataset

```{r}
## Load Data ##
SatisfactionSurvey.data <- read.csv("EcommSatisfaction_forCourse.csv", header=TRUE, sep=";",  dec=".")

```
This dataset consists of customer survey and customer history data from an online store.
Each row is a customer.
We are particularly interested in:
 * Satisfaction: customer satisfaction
 * wom: Word-of-mouth -> Recommendation intention
 
For this purpose we want to see if some assessments of our store or service have an impact.
We asked whether the following aspects were perceived positively (high/very) or negatively (low/not at all), using various slider scales:
 * Delivery_speed : was everything delivery fast enough? (especially important now before Christmas)
 * Qualityperception : Does the store give a high quality impression?
 * OrderingProcess : How easy is the ordering process?
 * CompetitivePricing : How competitively priced are our offers perceived?
 * Warranty : How convincing and confidence-building are our dealer warranties?
 * TechnicalSupport : How good is the impression of our tech support?
 * ComplaintResolution : How well do we resolve complaints?
 * AdvertisingPerception : How positively is our advertising perceived?
 * EcommerceExperience : How much previous experience do customers have with the store?
 The only group variable available was location
 * Location : Customer's place of residence -> 1 = suburb/rural area; 0 = city centre.

## Data Exploration
maybe now you use your knowledge and get to know the data a bit


```{r}
# Here is space for your code


```

The online store managers tell you that they believe that the delivery speed is a very important factor for customer satisfaction. Lets see if that is true.

one possibly interesting plot:
```{r}
library(ggplot2)
ggplot(SatisfactionSurvey.data, aes(x=Delivery.Speed, y=Satisfaction)) + 
  geom_point() +
  geom_smooth(method=lm, se=TRUE, ) +
  theme_light() +
  ggtitle("Satisfaction and Delivery Speed") +
  theme(plot.title = element_text(hjust = 0.5))

```

Apparently there is a positive association between delivery speed and satisfaction. But is that relationship significant? Lets use a linear model to find out.


## Linear models with observational data

Now we want to see if we can explain customer satisfaction with the delivery speed.
We will use a linear model for this.
We will also control for the other variables, because we want to see if delivery speed has an effect on satisfaction, even if we control for other factors.




```{r}
CRM.m1 <- lm(CompetitivePricing ~ Delivery.Speed, SatisfactionSurvey.data)
summary(CRM.m1)

```
It looks like there is indeed a significant increase in satisfaction with delivery speed.

Now your turn. Try a different variable. Is there a significant effect for competitive pricing?


```{r}
#your code
CRM.m2 <- lm(Competitive.pricing ~ ., SatisfactionSurvey.data)
summary(CRM.m2)
```

What is with the other explanatory variables? Are they significant?
Build a model an (ALWAYS) compare to the first in a comparison table


```{r}

CRM.m2 <- lm(Satisfaction ~ ., SatisfactionSurvey.data[2:12]) #the "." in a formula says " + all variables(or columns) in dataset, not yet used"


screenreg(list(CRM.m1, 
               CRM.m2),
          caption="Drivers of Satisfaction",
          dcolumn=FALSE,
          digits=2,
          leading.zero = FALSE,
          custom.model.names=c("DeliverySpeed", "All Variables"), 
          custom.header = list("Linear Models" = 1:2), 
          reorder.coef = c(2:11, 1))


```

The model with all variables explains significantly more variance. But not all variables have a significant effect. The delivery-speed-effect is even no longer significant. Maybe we put too many variables into the model. A good rule of thumb is to have as few variables as possible and only to include variables that explain some variance, or that are of mandatory interest (e.g., because a theory says so, or because managers want to see how the variable does, even if it is insignificant). Also remember that significance tests are susceptible to sample sizes. Another rule of thumb would be to have at least 10 observations per variable. We have 100 observations, so we could have 10 variables. But we have 11 variables, so we are at the limit. Lets see what happens if we leave out the insignificant variables.


```{r}

CRM.m3 <- lm(Satisfaction ~ ., SatisfactionSurvey.data[c(2,3,4,5,7,12)]) 


screenreg(list(CRM.m1, 
               CRM.m2,
               CRM.m3),
          caption="Drivers of Satisfaction",
          dcolumn=FALSE,
          digits=2,
          leading.zero = FALSE,
          custom.model.names=c("DeliverySpeed", "All Variables", "Significant Variables"), 
          custom.header = list("Linear Models" = 1:3), 
          reorder.coef = c(2:11, 1))


```
We have hardly lost any explained variance. The third model seems to make more sense than the second.

Now the managers tell us that they also believe deliver speed is less important if you live in a rural area. Lets see if that is true. We will use an interaction term for that.

## Interaction with location

Actually, we wanted to see if there is a moderation of the delivery time effect via the place of residence...


##First: check visually
```{r}
#check visually
ggplot(SatisfactionSurvey.data, aes(x=Delivery.Speed, y=Satisfaction, color=as.factor(Location))) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  theme_light() +
  ggtitle("Satisfaction and Delivery Speed") +
  theme(plot.title = element_text(hjust = 0.5))

```
MAybe the managers are correct: the slope of the regression lines seems to be different and for customers in rural areas (location =1) delivery speed matters less. But is that difference significant? Lets use a linear model to find out.

## Interaction model

```{r}

CRM.m4 <- lm(Satisfaction ~ Delivery.Speed*Location + Quality.percption + E.Commerce.experience+Complaint.resolution , SatisfactionSurvey.data) 


screenreg(list(CRM.m1, 
               CRM.m2,
               CRM.m3,
               CRM.m4),
          caption="Drivers of Satisfaction",
          dcolumn=FALSE,
          digits=2,
          leading.zero = FALSE,
          custom.model.names=c("DeliverySpeed", "All Variables", "Significant Variables","Location Interaction"), 
          custom.header = list("Linear Models" = 1:4), 
          reorder.coef = c(2:12, 1))


```

Aha, it looks like the managers were right about this interaction.
Now it is your turn again. Are there comparable influences and interactions when you want to explain the customers recommendation behavior (variable wom)?


```{r}
#your code
CRM.m3 <- lm(wom~ ., SatisfactionSurvey.data[2:1]) 
summary(CRM.m3)
```

# PART3: BONUS Task --> Non-Linear models with experimental data 

For the final part in this exercise, I want to introduce a new case and dataset again. This data is from a large field experiment at a prominent web service platform. The problem is, that many customers use an ad blocker in their browsers. So the retailer decided to test an appeal to users, to turn off their Ad Blocker. Two banners were designed, one arguing rationally and another one humorously. These banner appeals could be implemented natively into the website such that an ad blocker would not block them. For a few weeks, users with an active ad blocker randomly received one of the two banners. A third group with ad blockers received no banner appeal (control group). Some users visited the site often and hence saw the banner more often (or not, in the control group). The dependent variable, is ad blocker deactivation at any time during the trial.
Almost 300k users participated. The data is in the file "AdBlockReductionBannerExperiment_forCourse.xlsx". 

```{r}
## Load Data ##

if(!require(readxl)){install.packages("readxl")};library(readxl) #Package to read xlsx files (Microsoft Excel)

AdBlocker.data <- as.data.frame(read_xlsx("AdBlockReductionBannerExperiment_forCourse.xlsx", sheet= 1, col_names = TRUE))

```
This dataset lists the following variables:
 * ID: Each row is a respondent
 * VisitViews : How often the user came during the trial and potentially how often the banner was shown
 * Group: the experimental group: (control, humor, rational)
 * adblock_off: if, at some point during the trial, the user deactivated the ad blocker (1) or not (0)
 
 
As usual, you should get to know the data first and explore it a bit.


```{r}
# Here is space for your code
# Assuming your dataset is named 'your_data'
# You may need to replace 'adblock_off' and 'group' with the actual variable names in your data

# Load the required libraries
library(ggplot2)

# Fit a logistic regression model
model <- glm(adblock_off ~ Group, data = AdBlocker.data, family = "binomial")

# Create a data frame for predictions
plot_data <- expand.grid(Group = c("control", "humor", "rational"))

# Make predictions using the model
plot_data$predicted_probability <- predict(model, newdata = plot_data, type = "response")

# Create a box plot
ggplot(plot_data, aes(x = Group, y = predicted_probability, fill = Group)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Experimental Group",
    y = "Predicted Probability of Adblock Deactivation",
    title = "Bar Plot of Predicted Probabilities by Experimental Group",
     subtitle = "Krishnamoorthy Juttoo Chandrasekaran",  # Add your name as a subtitle
       caption = Sys.Date()
  ) +
  theme_minimal()
predictionplot <- last_plot()
ggsave("JuttooChandrasekaran_Krishnamoorthy_bonus4.png", plot = predictionplot, device = "png", dpi = 300, width = 10, height = 10, units = "cm", limitsize = FALSE)
```


Here are a few things, that I want to highlight:


```{r}
hist(AdBlocker.data$VisitsViews, main="Histogram of Visits", xlab="Visits (or views)",ylab="Frequency")
```

* The variable VisitViews is a count variable. It is not normally distributed, but skewed to the right. This is typical for count variables. We will have to take this into account when we model the data. For example, we could use a Poisson regression model. But we will not do that here. Instead, we will use a linear model, but we will transform the variable first. We could use the log() (natural logarithm) of the variable. This is a common transformation for count variables. It is called a log-linear model.

```{r}
hist(log(AdBlocker.data$VisitsViews), main="Histogram of Visits", xlab="log(Visits)",ylab="Frequency")
```
Not perfect, but better...




## Bonus task 4: reducing ad blocker usage

This week your task is again quite short:
 * Take the AdBlocker data and create a linear model to predict ad blocker deactivation (adblock_off).
 * You may want to consider different transformations of the VisitViews variable.
 * YOu may want to consider different effects of the two tested banner types
 * you should show your results (or an avrage forst overview of the them) in a figure: a plot that shows the predicted probabilities of ad blocker deactivation for the different experimental groups (control, humor, rational)
  * Save the figure as a pdf or image
 * Upload the file to moodle




```{r}
# create your final plot here
model <- lm(adblock_off ~ Group, data = AdBlocker.data, family = "binomial")

# Create a data frame for predictions
plot_data <- expand.grid(Group = c("control", "humor", "rational"))

# Make predictions using the model
plot_data$predicted_probability <- predict(model, newdata = plot_data, type = "response")

# Create a plot
ggplot(plot_data, aes(x = Group, y = predicted_probability)) +
  geom_col(fill = "skyblue", color = "black") +
   geom_smooth(method=lm, se=FALSE)
  labs(
    x = "Experimental Group",
    y = "Predicted Probability of Adblock Deactivation",
    title = "Predicted Probabilities of Adblock Deactivation by Experimental Group",
    subtitle = "Krishnamoorthy Juttoo Chandrasekaran",  # Add your name as a subtitle
       caption = Sys.Date()
  )
predictionplot<-last_plot()
# save as png
ggsave("JuttooChandrasekaran_Krishnamoorthy_bonus4.png", plot = predictionplot, device = "png", dpi = 300, width = 10, height = 10, units = "cm", limitsize = FALSE)


```

