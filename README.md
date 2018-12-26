# useicd10cm
A R package for the use of the injury ICD-10-CM matrix and ICD-10-CM codes to define

**useicd10cm**, a package with functions and data for the analysis of icd-10-cm codes related primarily to injury. For injuries, it adds intent and mechanism, and ICD-10-CM code description to the inputed data.

To install and load the **useicd10cm** package into your working environment:

1. Install the devtools package: *`install.packages("devtools")`*
2. Install the useicd10cm package: *`devtools::install_github("epinotes/useicd10cm")`*
3. Load the package: `library(useicd10cm)`

Try these lines of R codes to get intent, mechanism or their combination. In this example the function looks into two diagnosis fields.  

```{r}
library(tidyverse)   
library(fuzzyjoin)   
library(useicd10cm)

dat <- data.frame(d1 = c("T63023", "X92821", "X99100", "T360x"),
d2 = c("T65823", "Y030x0", "T17200", "V0100x" ))

dat %>% icd_intent_mech(inj_col = c(1,2), reference = "both")
dat %>% icd_intent_mech(inj_col = c(1,2), reference = "intent")
dat %>% icd_intent_mech(inj_col = c(1,2), reference = "mechanism")
```
</br>

  For more details `?icd_intent_mech`
