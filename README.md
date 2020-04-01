# wfp-foodprices
A set of R tools for analyzing the "Global Food Prices Database" maintained by the World Food Programme.

# Description of Dataset:
  This dataset contains Global Food Prices data from the World Food Programme covering foods such as maize, rice, beans, fish, and sugar for 76 countries and some 1,500 markets. It is updated weekly but contains to a large extent monthly data. The data goes back as far as 1992 for a few countries, although many countries started reporting from 2003 or thereafter. (humdata.org)
<br/><br/>
<b>Link:</b>
  https://data.humdata.org/dataset/wfp-food-prices
  <br/>
  (The csv will be called 'wfpvam_foodprices.csv' when downloaded.)
<br/><br/>

# Description of Repository:

### Overview ###
The primary goal of the analysis is to identify pairs of countries for which price returns for the same commodity are highly-correlated. If the same crop follows similar price patterns in two coutnries, we could infer that the markets of these two countries are fairly integrated.<br/><br/>
One caveat is that food prices in this dataset are listed in local currencies. Due to this, we are also implicitly measuring whether the currencies of two countries are moving in tangent, which is actually helpful for measuring the extent to which country-crop markets are integrated, particularly if a producer or consumer is searching for substitutable markets.

### Data ###
1. World Food Programme - Global Food Prices Database

### Code ###
1. Import and Clean Dataset
2. Correlation of Prices for Country-Crop Pairs
3. Charting Price Returns for Country-Crop Pairs (return distributions, indexed returns, and scatterplot)
4. Indian state prices
