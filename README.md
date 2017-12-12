# One Acre Fund predictive model 

Production-ready model generating probabilities that *groups* of farmers will default on their loans by the repayment deadline. 

## A note about SQL-R

This productionised model was built using [Microsoft's new SQL-R stored procedure](https://docs.microsoft.com/en-us/sql/advanced-analytics/tutorials/rtsql-using-r-code-in-transact-sql-quickstart) generation process. 

The stored procedure takes the following form:

<img src="https://user-images.githubusercontent.com/26271235/33419703-b541b046-d5bc-11e7-8f96-32b3144ffb59.png" width="800" height="400">

This approach has a few "features" associated with it:

* Joins within the SP result in a fail - therefore a view is made, and the SP merely calls part of that view. 
* Microsoft's SP is *much* slower than using a connection string within R (e.g. via RODBC package).
* The pre-SP version is [here](https://github.com/Michael-Bar/OAF-predicting-defaults-prototype).


## Overview

A group-level default event is defined as any single client within a lending circle defaulting on their loan at the repayment deadline (OAF provides loans without repayment plans and with just a single repayment due date, so "days-past-due" etc. are not applicable here). If a single member of a group defaults, then her entire group (6-12 members) is black-listed for subsequent seasons. This has significant implications for client growth. The aim of this model is to identify groups at risk of default, and flag them for action from OAF loan officers and/or call centres.

The prototype model is currently in action across Kenya, Rwanda and Tanzania, representing >85% of our clients. Model scoring yields a cross-validation AUC score of 0.9 - 0.95 (depending on the country). 

## Breakdown of model

The model can be broken down into 3 (automated) stages, data cleaning, model building and finally predictions. 

### Data cleaning

Data are first cleaned and then summarized at the group level, this is also where new features are constructed (e.g. looking at group cohesion through spreads on repayment dates and amounts). At this point we check for multi-collinearity amongst our variables and drop any variables with high correlation: 


<img src="https://user-images.githubusercontent.com/26271235/30958765-c4f5d406-a446-11e7-81b2-8920fd09c655.png" width="400" height="400">


### Model building

With clean data we can start model building and testing. First there are options for recursive-feature extraction and model stability checks (which increase runtime). Once these checks are complete a small RF is grown and nodesize and mtry optimized for minimal OOB error (a mixed gradient descent and grid-search approach):

![Model tuning](https://user-images.githubusercontent.com/26271235/30958758-c4ba3dc4-a446-11e7-9f43-abdd30a22384.png)

Once parameters are optimised on this smaller RF, a larger RF is grown with the aim of establishing an optimal cutoff point between classes:

![cutoff](https://user-images.githubusercontent.com/26271235/30958764-c4c2ba9e-a446-11e7-947c-38a7faebb613.png)

The aim of this cutoff is to minimise false positives and maximise true positives:

![cutoff](https://user-images.githubusercontent.com/26271235/30958760-c4bb8b2a-a446-11e7-81ff-34e8023d87ff.png)

However, given the importance of preventing default and subsequent black-listing, false negatives are penalized more heavily than false positives (the exact ratio of penalties depends on the historical default rates). 

Finally, the training process generates informative plots such as the variable importance plot:


<img src="https://user-images.githubusercontent.com/26271235/30958761-c4be8726-a446-11e7-8d53-d28c0fe3eef8.png" width="400" height="500">


And some partial-dependency plots:


<img src="https://user-images.githubusercontent.com/26271235/30958759-c4bab754-a446-11e7-9ce1-4f90c628cd2e.png" width="500" height="300">


The last part of the process involves applying the model to cleaned data from the current year. These predictions are then shared with loan officers and call centre staff for follow up

## Planned updates

* Loan officer fraud detection
* Individual level predictions
* Hybrid approach combining predictions of final % repaid with default probability
* Default hotspot mapping for decision making
