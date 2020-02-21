# Mental Health

Mental Health reports the prevalence of the mental illness in the past year by age range for US regions and Connecticut.

### Data Source

[National Survey on Drug Use and Health](https://www.samhsa.gov/data/nsduh/state-reports-NSDUH-2018)

* https://www.samhsa.gov/data/report/2016-2017-nsduh-state-specific-tables
* 

### Update dataset

#### Up to 2016-2017

1. Data needed for processing is an HTML that should be retrieved from an iframe URL, like that: https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHsaeSpecificStates2017B/NSDUHsaeSpecificStates2017.htm
1. Rename the HTML as `NSDUH_YYYY-YYYY.html` and save into `raw/` folder
1. Run the R script for processing
