## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, message=FALSE, warning=FALSE, include=FALSE-----------
library(dplyr)
library(reshape2)
library(nlme)
library(emmeans)
library(car)

## ---- include=TRUE-------------------------------------------------------
library(ClinReport)
library(officer)
library(flextable)

data(datafake)

# Default quantitative stat desc

tab1=report.quanti(data=datafake,y="y_numeric",
		x1="GROUP",y.label="Quantitative response (units)",
		subjid="SUBJID",total=T)
		
# Default qualitative stat desc

tab2=report.quali(data=datafake,y="y_logistic",
		x1="GROUP",y.label="Qualitative response",
		subjid="SUBJID",total=T)

# Binding them into a single table

tab=regroup(tab1,tab2)

# In the R console, it looks like

tab

# Create formatted output for Microsoft Word or R markdown documents (like this one)

doc=report.doc(tab,title="Table of classic descriptive statistics (qualitative and quantitative parameter)",
		colspan.value="Treatment group", init.numbering =T )			

doc

