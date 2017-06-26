# Critical Care Data Anonymisation

This code is used in conjunction with the [Standard Operating Procedure](https://github.com/UCL-HIC/ccanonym/blob/master/inst/SOP%20data%20release.pdf) for anonymising data for release for use by clinical researchers according to the terms of the end user license.

We have made this repository public in the interests of transparency.

## Dependencies:
* sdcMicro
* ccdata

## How to run:
1. Prepare a YAML configuration file. You can use the following function to
create a template for you.
```
template.conf("template.yaml")
```

2. Using `sdc.trial` to find the most suitable parameters such as K-anonymity
or L-diversity. If it is specified in the SOP, then this step can be skipped.

3. Create the new ccdata using `anonymisation` function.


## Template code

```
# Load libraries
library(cleanEHR)
library(ccanonym)
library(yaml)

# Load data
load("data/current_db")

# Run anonymiser
ccd <- anonymisation(
  alldata,
  conf="release-internal.yaml",
  verbose=T,
  remove.alive=FALSE,
  k.anon=5)

anon_conf <- yaml.load_file("release-internal.yaml")

# Now save both data and configuration into a single R data file
save(file="anon_internal.RData", list=c("ccd", "anon_conf"))
```
