# Mental Health

Mental Health reports the prevalence of the mental illness in the past year by age range for US regions and Connecticut.

### Data Source

[National Survey on Drug Use and Health](https://www.samhsa.gov/data/nsduh/state-reports-NSDUH-2018)

* https://www.samhsa.gov/data/report/2016-2017-nsduh-state-specific-tables (2016-2017 HTML)
* https://www.samhsa.gov/data/report/2017-2018-nsduh-state-prevalence-estimates (2017-2018 Excel)

### Update dataset

#### Up to 2016-2017

1. Data needed for processing is an HTML that should be retrieved from an iframe URL, like that: https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHsaeSpecificStates2017B/NSDUHsaeSpecificStates2017.htm
1. Rename the HTML as `NSDUH_YYYY-YYYY.html` and save into `raw/` folder
1. Run the R script for processing

#### 2017-2018

1. Data is processed by Jupyter notebook, because it is published in .xls
