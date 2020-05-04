# Palaute
> Plot, Analyze, Learn And Understand Topic Emotions

**Palaute** is an R Shiny application for analyzing text data. Text analysis is performed using
structural topic model (STM) algorithm in combination with emotion and sentiment analysis.

Palaute is deployed [here](http://86.50.253.215/)

Palaute was created as a part of my master's thesis. The thesis can be found [here (not yet published)](#!)

## Installation
Palaute requires some packages to run and the packages require all of their dependencies. The used packages are listed below:
```
shiny
dplyr
stm
syuzhet
ggplot2
proxy
wesanderson
Rtsne
corpus
scales
readr
```

After installing above packages, you should be able to run the application from the R-studio. Open `app.R` in R-studio and select `Run App`.