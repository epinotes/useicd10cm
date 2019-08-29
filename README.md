# useicd10cm

## Installation

A R package for the use of the injury ICD-10-CM matrix and ICD-10-CM codes to define

**useicd10cm**, a package with functions and data for the analysis of icd-10-cm codes related primarily to injury. For injuries, it adds intent and mechanism, and ICD-10-CM code description to the inputed data.

To install and load the **useicd10cm** package into your working environment:

1. Install the devtools package: *`install.packages("devtools")`*
2. Install the useicd10cm package: *`devtools::install_github("epinotes/useicd10cm")`*
3. Load the package: *`library(useicd10cm)`*

</br>

## Example-1 

Using the function `icd_drug_opioid()` to add non-fatal drug overdose indicators to a file with ICD-10-CM codes.


```r
library(tidyverse)
library(useicd10cm)

# check the data content

set.seed(11)

icd10cm_data150 %>% sample_n(10)

# get the columns with the codes of interest

diag_cols <- grep("diag|ecode", names(icd10cm_data150), ignore.case = T)

diag_cols

# check the data again with the additional created overdose indicators

od_data <- icd10cm_data150 %>% 
  icd_drug_opioid(diag_ecode_col = diag_cols)

set.seed(11)

od_data %>% sample_n(10)

```

For information on the function `icd_drug_opioid` run this line:


```r
?icd_drug_opioid

```

For more details on the coding of drug overdose visit the **[Injury Toolkit](https://resources.cste.org/Injury-Surveillance-Methods-Toolkit/)** 

</br>

## Example-2

Try these lines of R codes to get intent, mechanism or their combination. In this example the function looks into two diagnosis fields. The function `icd_intent_mech()`uses the **[ICD-10-CM External Cause Matrix](ICD-10-CM External Cause Matrix)** to determine injury intent and mechanism.

```r
library(tidyverse)   
library(fuzzyjoin)   
library(useicd10cm)

# create a dataset and run the funtion icd_intent_mech()

dat <- data.frame(d1 = c("T63023", "X92821", "X99100", "T360x"),
d2 = c("T65823", "Y030x0", "T17200", "V0100x" ))

dat %>% icd_intent_mech(inj_col = c(1,2), reference = "both")
dat %>% icd_intent_mech(inj_col = c(1,2), reference = "intent")
dat %>% icd_intent_mech(inj_col = c(1,2), reference = "mechanism")

```
</br>

  For more details on the function `icd_intent_mech()`, run:
  
```r
  ?icd_intent_mech
  
```
  
