# Regression Discontinuity Design & Instrumental Variables



```{=html}
<style>body {text-align: justify}</style>
```



## Causal Inference References

I have mentioned some references on Causal Inference in class that I find extremely useful and that have been an inspiration to my scripts.

The big ones out there that have helped me a lot are:

-   The Effect by @huntington-klein_effect_nodate
-   The Book of Why by @pearl_book_2018
-   The Causal Mixtape @cunningham_causal_2021

## Regression Discontinuity Design

This session is all about *Regression Discontinuity Design* (RDD). RDDs are useful when we have a treatment that is assigned discontinuously creating a *cutoff* or a *jump*. It creates a divide within a sample at that said cutoff and introduces a treatment. On one side, we have people (observations) which have been treated and than after/before the cutoff, people that have not been treated. Most treatments are binary, i.e. can either be 0 or 1.

The variable that creates this discontinuity is called the *running variable* <!--- insert Andrew Gelman reference to 2007-->. It is sometimes also refered to as the *forcing variable* or *forcing function*. All these terms refer to the same thing however. The *cutoff* is the moment where the *running variable* introduces the treatment; for example:

-   If age is your running variable, turning 21 introduces the treatment of being allowed to drink
-   Barely managing to win slightly more than 50% makes a candidate get into office

A third term that we will have to keep in mind is the *bandwidth*. In plain English, it simply means how many observations to the left and to the right of the cutoff we are going to compare. We assume that most individuals/observations that are very close to the cutoff are almost the same or at least very similar and thus comparable. The further away we get from the cutoff point the more noise is introduced and the more other and different variables might explain our outcome variable. As we will see later, the choice of bandwidth is highly important for RDDs and must be theoretically and empirically thoroughly discussed. But when is that not the case for any statistical decision that we make?

So to recap: In RDDs, a running variable introduces a treatment on a continuous variable and thus creates a cutoff. The difference between the people just left and right of the cutoff is therefore that one side has been treated whereas the other has not. It is important to note that this running variable is somewhat inevitable. It is hard to not turn 21 in your life assuming you make it until then. We all have turned 21 at some point.

### Once again, visualizing your data

I have been saying over and over again that it is very important that you first inspect your data and always visualize your data. RDD is actually a great use case in which visualizing the data at hand helps you identify potential discontinuities in your data.

Below I simulate both the `running_variable` as well as our `outcome` to graphically highlight the idea of RDD. Further, the tibble also includes a binary variable that highlights whether the cutoff has taken place or not.


::: {.cell}

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.1     ✔ tibble    3.2.1
✔ lubridate 1.9.3     ✔ tidyr     1.3.1
✔ purrr     1.0.2     
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
simulated_rdd_data_before_treatment <- 
  tibble(
    outcome = rnorm(200, mean = 1, sd = 1),
    running_variable = c(1:200),
    treatment = "before treatment"
  ) 

simulated_rdd_data_after_treatment <- 
  tibble(outcome = rnorm(200, mean = 3, sd = 1),
        running_variable = c(201:400),
        treatment = "after treatment"
  )

rbind(simulated_rdd_data_before_treatment, 
      simulated_rdd_data_after_treatment) |> 
  ggplot(aes(x = running_variable, y = outcome, color = treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "lm") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")
```

::: {.cell-output-display}
![](session2_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::
:::


Above you can easily see that there is a cutoff where running variable takes on value 201. This is only simulated data and we do not measure any specific variables yet. But visually speaking, this is something you should observe in your data. I do not want to recommend data mining in any way; it is bad practice and we should work on theoretically driven models. But if you observe such a pattern in your data, maybe there is something happening at the cutoff point that deserves our causal inference and thus RDD attention...?


### Sharp vs. fuzzy RDD

We distinguish between two types of RDDs. Below you will find examples for *sharp* and *fuzzy* designs. A *sharp* RD design occurs for variables like age for example. You can hardly escape turning 21 if you are 20 years and 364 days old. However, spending in a constituency is not a sharp RDD; it is fuzzy. It is fuzzy because the spending is not directly dependent on the vote share but rather on the vote share being above 50% but never directly at 50%. It only raises the probability of the treatment. In other words: It is fuzzy because the running variable increases the probability of the treatment but does not guarantee it. This is in contrast to a *sharp* RDD where the running variable directly introduces the treatment, a sort of jump from 0 to 1.

Put in formal terms, a sharp RDD is happens when the treatment is deterministic and the rate jumps from 0% to 100% at the cutoff. We are working with a fuzzy RDD when the treatment at the cutoff is not from 0 to 1 (or 100%) but rather from 0 to some value between 0 and 1.


##### Sharp RDD

Below, I am using an example by [Rohan Alexander](https://rohanalexander.com/) from his book [Telling Stories with Data](https://tellingstorieswithdata.com/) again. He reproduces a study by @carpenter_minimum_2015. The authors are interested in estimating the causal effect of access to alcohol on crime. They use the minimum legal drinking age (MLDA) as an instrument for access to alcohol. The idea is that the MLDA is a *sharp* running variable. You cannot escape turning 21 if you are 20 years and 364 days old. Their data is available [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/27070).


::: {.cell}

```{.r .cell-code}
library(haven)

carpenter_dobkin <-
  read_dta(
    "data/carpenter_dobkin_replication.dta"
  )

carpenter_dobkin
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2,922 × 143
   days_to_21   all felony misdemeanor fbi_offense_miss violent murder
        <dbl> <dbl>  <dbl>       <dbl>            <dbl>   <dbl>  <dbl>
 1      -1461  5289   1556        3064              206     686      7
 2      -1460  4800   1460        2756              196     581      8
 3      -1459  4464   1356        2589              214     561      8
 4      -1458  4425   1382        2511              180     587     15
 5      -1457  4451   1423        2535              169     596      5
 6      -1456  4501   1375        2593              176     587      9
 7      -1455  4331   1341        2459              159     575     14
 8      -1454  4432   1372        2568              179     599      6
 9      -1453  4576   1401        2650              201     604      3
10      -1452  4608   1486        2577              172     609     14
# ℹ 2,912 more rows
# ℹ 136 more variables: manslaughter <dbl>, rape <dbl>, robbery <dbl>,
#   assault <dbl>, aggravated_assault <dbl>, ot_assault <dbl>, property <dbl>,
#   burglary <dbl>, larceny <dbl>, mv_theft <dbl>,
#   stolen_prop_buy_rec_poss <dbl>, vandalism <dbl>, ill_drugs <dbl>,
#   cocaine_opio_sale_manuf <dbl>, mj_sale_manuf <dbl>,
#   dang_non_narc_sale_manuf <dbl>, cocaine_opio_posses <dbl>, …
```


:::
:::

::: {.cell}

```{.r .cell-code}
carpenter_dobkin_prepared <-
  carpenter_dobkin |>
  mutate(age = 21 + days_to_21 / 365) |>
  select(age, assault, aggravated_assault, dui, traffic_violations) |>
  pivot_longer(
    cols = c(assault, aggravated_assault, dui, traffic_violations),
    names_to = "arrested_for",
    values_to = "number"
  )

carpenter_dobkin_prepared |>
  mutate(
    arrested_for =
      case_when(
        arrested_for == "assault" ~ "Assault",
        arrested_for == "aggravated_assault" ~ "Aggravated assault",
        arrested_for == "dui" ~ "DUI",
        arrested_for == "traffic_violations" ~ "Traffic violations"
      )
  ) |>
  ggplot(aes(x = age, y = number)) +
  geom_point(alpha = 0.05) +
  facet_wrap(facets = vars(arrested_for), scales = "free_y") +
  theme_minimal()
```

::: {.cell-output-display}
![](session2_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
carpenter_dobkin_dui_only <-
  carpenter_dobkin_prepared |>
  filter(
    arrested_for == "dui",
    abs(age - 21) < 2
  ) |>
  mutate(is_21_or_more = if_else(age < 21, 0, 1))
```
:::


Below I am using the `rdrobust()` function from the `rdrobust` package to estimate the causal effect of the minimum legal drinking age on DUI arrests. The `rdrobust()` function requires the dependent variable `y`, the running variable `x`, the cutoff `c`, and the bandwidth `h`. The `all` argument is set to `TRUE` to include all robustness checks that you might want to do. Note that the choice of bandwidth is extremely important for the result and has to be empirically and theoretically grounded.


::: {.cell}

```{.r .cell-code}
library(rdrobust)
rdd_dui <- rdrobust(
  # specifies the DV
  y = carpenter_dobkin_dui_only$number,
  # specifies the running variable of the RDD
  x = carpenter_dobkin_dui_only$age,
  # specifies the cutoff
  c = 21,
  # specifies the bandwidth, here the unit of analysis is years
  h = 2,
  all = TRUE
) 

rdd_dui |> summary()
```

::: {.cell-output .cell-output-stdout}

```
Sharp RD estimates using local polynomial regression.

Number of Obs.                 1459
BW type                      Manual
Kernel                   Triangular
VCE method                       NN

Number of Obs.                  729          730
Eff. Number of Obs.             729          730
Order est. (p)                    1            1
Order bias  (q)                   2            2
BW est. (h)                   2.000        2.000
BW bias (b)                   2.000        2.000
rho (h/b)                     1.000        1.000
Unique Obs.                     729          730

=============================================================================
        Method     Coef. Std. Err.         z     P>|z|      [ 95% C.I. ]       
=============================================================================
  Conventional   192.790     6.221    30.989     0.000   [180.597 , 204.984]   
Bias-Corrected   207.423     6.221    33.341     0.000   [195.230 , 219.617]   
        Robust   207.423    11.524    17.999     0.000   [184.837 , 230.010]   
=============================================================================
```


:::
:::


The conventional estimate of the coefficient suggests that immediately upon crossing the age threshold of 21, there is an estimated increase of approximately 192.79 DUI arrests in the specified age bandwidth. This is a substantial effect, indicating a significant increase in DUI arrests that coincide with reaching the legal drinking age. The bias-corrected method provides a slightly higher estimate of the increase in DUI arrests, at about 207.42. This method adjusts for potential biases in the conventional estimate, potentially offering a more accurate depiction of the effect. The higher figure further emphasizes the impact of reaching the age threshold on DUI arrests. The robust estimate matches the bias-corrected estimate but accounts for potential heteroskedasticity or autocorrelation in the data, reflected in its larger standard error. This method aims to provide a more reliable estimate of the effect size, affirming the significant increase in DUI arrests but acknowledging greater uncertainty in the estimate. The robustness checks thus suggest that the effect is not sensitive to the choice of bandwidth, which is a good sign.


#### Fuzzy RDD example

Below I simulate a *fuzzy* regression discontinuity design. I am simulating election results in 1000 constituencies. The vote share is randomly distributed around the 50% threshold. It is drawn from a uniform distribution between 49% and 51% to ensure that all vote shares are near the 50% majority threshold. I make sure that in this simulation, the government spending is increased if the vote share is above 50%. The spending is increased by a random noise to add variability. The idea behind this is that the government wants to reward constituencies where they have a majority but especially those where they have a very narrow majority. 


::: {.cell}

```{.r .cell-code}
num_constituencies <- 1000

# Simulating election results
election_data <- tibble(
  constituency = c(1:1000),
  vote_share = runif(num_constituencies, min = 49, max = 51), # Vote share around the threshold
  spending = rnorm(num_constituencies, mean = 100, sd = 20) # Base government spending
)

# Increase spending if vote share >= 50 (majority threshold)
election_data <- 
  election_data |> 
  mutate(
    noise = rnorm(n = num_constituencies, mean = 15, sd = 5), # Noise to add variability
    spending = if_else(vote_share >= 50, spending + noise, spending)
  )

# Visualizing the discontinuity
election_data |> 
  ggplot(aes(x = vote_share, y = spending)) +
  geom_point(alpha = 0.2) +
  geom_smooth(
    data = filter(election_data, vote_share < 50),
    method = "lm",
    color = "blue",
    formula = y ~ poly(x, 2) # Polynomial regression for smoother line
  ) +
  geom_smooth(
    data = filter(election_data, vote_share >= 50),
    method = "lm",
    color = "red",
    formula = y ~ poly(x, 2) # Polynomial regression for smoother line
  ) +
  theme_minimal() +
  labs(x = "Vote Share (%)", y = "Government Spending")
```

::: {.cell-output-display}
![](session2_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::

```{.r .cell-code}
# Estimating the effect of crossing the threshold
election_data <- 
  election_data |> 
  mutate(majority = if_else(vote_share < 50, 0, 1))
```
:::



Here is our RD analysis using the `rdrobust` package again. WE specify the cutoff point `c = 50` and the bandwidth of `h = 0.2`.


::: {.cell messages='wrong'}

```{.r .cell-code}
library(rdrobust)

rd_result <-
  rdrobust(y = election_data$spending,
           x = election_data$vote_share,
           c = 50, 
           h = 0.2)

rd_result |> summary()
```

::: {.cell-output .cell-output-stdout}

```
Sharp RD estimates using local polynomial regression.

Number of Obs.                 1000
BW type                      Manual
Kernel                   Triangular
VCE method                       NN

Number of Obs.                  534          466
Eff. Number of Obs.             129          101
Order est. (p)                    1            1
Order bias  (q)                   2            2
BW est. (h)                   0.200        0.200
BW bias (b)                   0.200        0.200
rho (h/b)                     1.000        1.000
Unique Obs.                     534          466

=============================================================================
        Method     Coef. Std. Err.         z     P>|z|      [ 95% C.I. ]       
=============================================================================
  Conventional    20.780     5.502     3.777     0.000     [9.997 , 31.562]    
        Robust         -         -     2.890     0.004     [7.075 , 36.892]    
=============================================================================
```


:::
:::

The output indicates that our estimated treatment effect is statistically significant. Both the conventional as well as the robust p-values are smaller than 0.05. Quick reminder that `P>|z|` is the column with the p-values. The results provide enough strong evidence that crossing the 50% vote share threshold significantly increases spending. If we were interpreting this in a real-world setting, we would reject the null hypothesis that government spending does change discontinuously at the majority threshold. 

You might be wondering what the `rdrobust` package actually does. It makes our life easier because estimating the RD by hand using a linear model is not very straightforward. Below I show you a very simplistic way of doing it. You will see that it does not generate the same results, and I will discuss the reasons for that a bit further below. As a rule of thumb, I recommend doing all these things using the `rdrobust` package while being aware of what it does under the hood and what things to look out for. Hence, the explanation by hand at first. 

To replicate the rdrobust results using lm(), we need to manually estimate a *local linear regression* with a sharp RD design. The key components to match are:

- the *bandwidth* (I take the same bandwidth as above): `rdrobust` limits the data to a ±0.2 window around the threshold (vote_share = 50%)
- the *Linear regression* (which the `rdrobust` does automatically): `rdrobust` estimates a separate local linear regression on both sides of the cutoff
- the *treatment effect*: the coefficient of interest is the discrete jump in spending at `vote_share` = 50%

Below, I filter `vote_share` to keep only observations within the ±0.2 bandwidth, `centered_vote` is cereated to emasure the distance from the threshold. And finally `majority` is the treatment dummy that takes on 1 if `vote_share` is equal or bigger than 50, otherwise it is 0.


::: {.cell}

```{.r .cell-code}
# Define the bandwidth
h <- 0.2  

# Subset data within ±0.2 of the threshold
rd_data <- election_data |> 
  filter(abs(vote_share - 50) <= h)

# Create running variable centered at the threshold
rd_data <- rd_data |> 
  mutate(
    centered_vote = vote_share - 50,  # Centering at the threshold
    majority = if_else(vote_share >= 50, 1, 0) # Treatment indicator
  )

# Estimate local linear regression with interaction
rd_model <- lm(spending ~ majority + centered_vote, data = rd_data)

# Output results
summary(rd_model)
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = spending ~ majority + centered_vote, data = rd_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-54.231 -13.543   0.363  13.612  47.525 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)     97.122      2.973  32.667  < 2e-16 ***
majority        20.427      5.188   3.938  0.00011 ***
centered_vote  -10.568     22.513  -0.469  0.63924    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 18.89 on 227 degrees of freedom
Multiple R-squared:  0.1903,	Adjusted R-squared:  0.1832 
F-statistic: 26.68 on 2 and 227 DF,  p-value: 3.917e-11
```


:::
:::

Our results differ slightly from our previous ones calculated by the package. Now why don't the results of our `lm()` match those of the `rdrobust()`? There are two potential problems that the latter accounts for and that we either have not modeled or simply omitted from our linear model. First, we did not use triangular kernel weighting. This is a statistical method to weight up observations that are closer to the cutoff, and to weight down those that are further away. Second, `lm()` does not come with robust standard errors, meaning that they are not robust to any issues introduced by potential *heteroskedasticity*.


##### Other robustness checks to consider

To assess if the estimated effect of crossing a specific threshold (e.g., the legal drinking age on DUI arrests) is robust and not sensitive to the choice of bandwidth in a Regression Discontinuity (RD) design, we can employ several robustness checks. These checks help ensure that the findings are not artifacts of particular specifications but reflect a genuine causal relationship. Here are some key strategies:

-   Varying Polynomial Orders: The choice of polynomial order for fitting the regression on either side of the cutoff can influence the results. Testing the model with different polynomial orders and checking for consistency in the estimated effect size can help ensure that the results are not sensitive to this choice.

-   Varying Bandwidths: Conduct the analysis using a range of bandwidths around the cutoff point. This involves re-estimating the model with different bandwidth sizes to observe if the estimated effect significantly changes. `rdrobust()` chooses the optimal bandwidth automatically. However, this does not mean that we should blindly trust it! If the estimated effect remains stable across a variety of bandwidths, this suggests that the findings are robust to the choice of bandwidth. It is crucial to demonstrate that the effect is stable across different choices of bandwidths. A common method is to visualize how the effect changes as the bandwidth varies, ranging from half to twice the optimal bandwidth size. The best case scenario would be that the Local Average Treatment Effect that you are estimating does not change much.

-   Placebo Tests or Fake Cutoffs: Applying the same RD design to points away from the actual cutoff can serve as a placebo test. If significant effects are detected at these placebo cutoffs, it might suggest that the observed effect at the real cutoff could be due to other factors rather than the policy or intervention being studied. Another way to approach placebo tests would be to show that there is no effect on a number of "fake" outcomes such as a spatial or temporal variables.

You also want to check for *sorting at the cutoff*. Regression discontinuity designs live and die with the fact that observations left and right of the cutoff are nearly identical and that the only thing that jumps at the cutoff is the probability of the treatment. If however our observations are somehow able to trick themselves either left or right of the cutoff, this would violate this basic assumption of RDDs. You could run a so called `McCrary` test. Or simply use @cattaneo2018manipulation estimator to check and account for these things. The package in R would be the `rddensity` package.





### Named Entity Recognition (OPTIONAL!!!)

**This is still work in Progress!**

This is the first time we are really entering the realm of text-as-data methods. The web scraping script in the appendix is actually the first preliminary stage, but this is the first time we will really work with textual statistics. I must have mentioned a few times in class that this part of quantitative methods fascinates me the most and I use them on a daily basis. Unfortunately, I won't have the time to give you a great introduction to it. With a bit of luck, I will write a script in the appendix as an introduction to text-as-data. If that doesn't work out, you are more than welcome to come to the course Malo and I are organizing on Computational Social Sciences between the 3rd and 4th semester in January 2025!

In this tutorial I will introduce you to *Named Entity Recognition* (NER). NER is a task from the field of *Natural Language Processing* (NLP). It is about detecting entities in texts and classifying them according to certain categories. These entities can be places, organizations, persons/names, data or miscellaneous. Strictly speaking, there are two NLP tasks that NER can perform for us: firstly, detecting entities in texts and then, in a second step, classifying them according to predefined categories.

As is so often the case with NLP tasks, this method also works with pre-trained models. There are many NER models, but this script will deal with [spaCy](https://spacy.io/); simply because there is an R package for it. NER models are pretrained in the sense that they have seen a human annotated corpus of text before, where the entities have all been detected and classified by hand. In short, the model learns what to look for and what to classify and how once it has been found. These models can be refined later, but for this tutorial we will work with off-the-shelf models as provided by the developers.

#### Installation of spaCy

First, we need to install the `spacyr` package. This package is an R wrapper for the spaCy NLP library which is usually running in Python. And as a general disclaimer, this might be tricky step -- after this, everything will be easy. To be able to do NER in R, we will have to do three things:

1.  Install Python on your computer
2.  Install the `reticulate` package in R
3.  Install the `spacyr` package in R

As we need to have a Python environment running, we will have to install Python on your computer first. You can download Python [here](https://www.python.org/downloads/). Download the latest version of Python and install it. Next, you will have to install the `reticulate` library in R; here is an [in-depth tutorial](https://github.com/rstudio/reticulate) on how to do this. Here is the next thing, you might want to take a look at which is the `spacyr` library. You can find the documentation [here](https://spacyr.quanteda.io/). For a tutorial on how to use it (that does not necessarily go beyond the documentation), you can check out [this blog post](https://cran.r-project.org/web/packages/spacyr/vignettes/using_spacyr.html).

If you managed to install all three things, these lines of code should run smoothly:


::: {.cell}

```{.r .cell-code}
library(reticulate)
library(spacyr)
```
:::


#### Installation of spaCy models

As I mentioned above, Named Entity Recognition relies on pre-trained models that have been trained on a human annotated corpus. These models are not part of the `spacyr` package, but you can download them from the spaCy website. The `spacyr` package provides a function to download these models. You can download the English model with the following code:

#### What is a corpus?

Before we start, a few words about the corpus we will be working with. We will use the *State of the Union corpus*, which comes with the `quanteda` package. `quanteda` is one of the leading and most important packages in R when it comes to textual statistics. I won't spend much time explaining the idea of corpora, but here are a few key points:

To use any TaDa methods, we must construct a corpus. You could compare it to the sampling we do from a population of interest. Generally speaking your corpus can be anything as long as it makes sense (very broad statement, I know) but it should at least be useful depending on your dependent variable or guiding research question. A corpus is never value free. This is a key-point. The written text of a society is a certain reflection of society itself. This means that marginalized groups of any given society are also very likely to be marginalized in text. Textual data captures societal structure (structuralism and so on...). We must care about ethics! A corpus must be validated (and validated, and validated again). Ideally we should have the *entirety* of a corpus. In our example, we are looking at the State of the Union addresses. This is a very specific corpus. We could also look at all speeches by a certain president, all speeches by a certain party, all speeches of a certain type (inaugural addresses, farewell addresses, etc.). But you should definitely justify why we are looking at this specific corpus, this specific set of speeches. Exhaustiveness might be a good rule of thumb. Why stop at only looking at the SOTU from the 1970s on?

#### How to construct a corpus?

We can always do things by hand…

… or we automate it ;) For this I recommend that you go check out the appendix chapter on webscraping but let me tell you that the beautiful Internet is full of corpora which are waiting to be retrieved (scraped), cleaned, and used for text as data! Some might already be available. Party manifestos can be retroengineered from the [Comparative Manifesto Project](https://manifesto-project.wzb.eu/). The [State of the Union corpus](https://rdrr.io/github/quanteda/quanteda.corpora/man/data_corpus_sotu.html) is available within the `quanteda` package. Plenty of parliamentary speeches can be found [here](https://www.clarin.eu/resource-families/parliamentary-corpora).

But you can get as creative as you want! You can scrape entire newspaper archives or other digital trace data from the web. Or let's say you are interested in lyrics of songs. Why not scrape <https://genius.com/>? Any text that is available on the web can be scraped and used as a corpus!

#### United Nations corpus

Load both the `reticulate` and the `spacyr` library to your environment.


::: {.cell}

```{.r .cell-code}
library(spacyr)
library(reticulate)
```
:::


Next, we are going to initialize the spaCy session. R will go look for a python environment installed on your computer that it can use to do our NER. You need to specify which spaCy model you would like to use in your session.


::: {.cell}

```{.r .cell-code}
spacy_initialize(model = "en_core_web_lg")
```
:::


It reads as follows: `en_` indicates that we are working with a model trained on English text, and `core_` means that it could technically detect more than just entities, `web_` means that it was trained on web text, and `lg` means that it is a large model. Other available models in English are `en_core_web_sm` (small model) and `en_core_web_md` (medium model). The bigger the model, the better the accuracy of the NER. [^session5-1] If you want to use a different model, you can check out this [list](https://spacy.io/models) here of the official spaCy website and see whether your language of interest is available. [^session5-2]

[^session5-1]: There is technically also a `en_core_web_trf` model which is a so-called transformer model. This model is not available in the `spacyr` package and we would have to use it in Python. In my experience so far, the large model suffices for most purposes!

[^session5-2]: Otherwise, you can always try to find a model on [huggingface.co](huggingface.co). In that case, you will have to code in Python though.

You always have to finalize your spacy session at the end!


::: {.cell}

```{.r .cell-code}
spacy_finalize()
```
:::


## References

::: {#refs}
:::
