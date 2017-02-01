TED data analysis

A couple of years ago, we did an analysis of the data collected in Market Dojo.
this provide to be quite interesting, and a good opportunity to learn more about oru customres

Recently I also did some research into OJEU and TED and came across the TED dataset.
https://data.europa.eu/euodp/en/data/dataset/ted-csv

This seemed like a great opportunity to try some different tools for data analysis, and to see what I could learn.

The 2015 dataset contains 542, 376 rows and took a while to download. Excel opened it without complaint, and I was able to see

If this has piqued your interest, there is a fantastic website http://yleborgne.net/opented/ where you can investigate for yourself. You can also see how I generated the graphs here (github).

—
# Useful labs
# Group by for our dataset
library(plyr)
# Nice graphs
library(ggplot2)
# Format our scales nicely
library(scales)
# We want cutting  countrycode 0.19
# So we can convert ISO to EU
# https://github.com/vincentarelbundock/countrycode#custom-country-match-vector
library(devtools)
install_github("vincentarelbundock/countrycode")

#
library(countrycode)
# Lookups for country codes

custom_match <- c('EL' = 'Greece', 'UK' = 'United Kingdom', 'MK' = 'Macedonia')

# load data into a dataframe
x = read.csv('/Users/nic/git/TED/TED_CAN_2015.csv', header=TRUE)

# summarize data by country
country_data = ddply(x, .(ISO_COUNTRY_CODE), summarize, freq=length(ISO_COUNTRY_CODE), total_value=sum(AWARD_VALUE_EURO, na.rm=TRUE))

# summarize data by country and spend country
# Country spend by country
country_by_country_data = ddply(x, c('ISO_COUNTRY_CODE', 'WIN_COUNTRY_CODE'), summarize, freq=length(ISO_COUNTRY_CODE), total_value=sum(AWARD_VALUE_EURO, na.rm=TRUE))

# Add a column that indicates if the spend was with same country
# or another
 country_by_country_data$ISO_COUNTRY_CODE <- factor(country_by_country_data$ISO_COUNTRY_CODE, levels=levels(country_by_country_data$WIN_COUNTRY_CODE))
country_by_country_data$OWN_COUNTRY <- ifelse(country_by_country_data$ISO_COUNTRY_CODE == country_by_country_data$WIN_COUNTRY_CODE, 'Yes', 'No')

# summarize data by number of offers

offers_data = ddply(x, c('NUMBER_OFFERS'), summarize, freq=length(NUMBER_OFFERS))

# Create a bar plot (number of contracts)
bp = ggplot(country_data, aes(x=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match),y=freq, fill=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match)))+
 geom_bar(width = 1, stat = "identity") +
geom_text(size=2, data=country_data,aes(x=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match),y=freq, label=comma(freq)), angle=90) +
 scale_fill_discrete(name="Country") +
 scale_x_discrete(name = 'Country') +
 scale_y_continuous(name='Number of contracts', labels = comma) +
theme(axis.text.x = element_text(angle=45, vjust=0.5, size=5),
      axis.text.y = element_text(size=5) )
bp

# Create a bar plot (value of contracts)
# Plot
# Reorder by value
# Translate country code to name
# divide to get bn euros
# Flip x and y so we can read it more easily
bp = ggplot(country_data, aes(x=reorder(countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match),total_value),y=total_value/1000000000, fill=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match)))+
 geom_bar(width = 1, stat = "identity") +
#geom_text( data=country_data,aes(x=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match),y=total_value, label=paste("€", comma(total_value)))) +
 scale_fill_discrete(name="Country") +
 scale_x_discrete(name = 'Country') +
 scale_y_continuous(name="Value of contracts (billion Euros)", labels = dollar_format(prefix="€", suffix="bn")) +
theme(axis.text.x = element_text(vjust=0.5, size=5),
      axis.text.y = element_text(size=5) ) +
coord_flip()
bp


# Plot country by country
bp = ggplot(country_by_country_data, aes(x=ISO_COUNTRY_CODE,y=total_value, fill=WIN_COUNTRY_CODE))+
 geom_bar(width = 1, stat = 'identity')+

#geom_text(data=country_by_country_data,aes(x=ISO_COUNTRY_CODE,y=total_value, label=comma(total_value)), angle=90) +
 scale_y_continuous(name='Value of contracts', labels = dollar_format(prefix='€'))
bp


# Seeing all the countries by country is overwhelming
# Check what each country spent with itself
bp = ggplot(country_by_country_data, aes(x=countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match) ,y=total_value, fill=OWN_COUNTRY))+
 geom_bar(width = 1, stat = 'identity')+
 scale_fill_discrete(name="Spent\nIn-Country") +
 scale_x_discrete(name = 'Country') +
 scale_y_continuous(name='Value of contracts', labels = dollar_format(prefix='€'))+
theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=5))
bp

### Playtime


## Plot frequency of number of offers
bp = ggplot(na.omit(offers_data), aes(x=NUMBER_OFFERS, y=freq, colour=freq))+
 geom_point() + #geom_bar(width = 1, stat = "identity")+
#scale_fill_discrete(name="Number of contracts") +
scale_x_log10(name = 'Number of offers') +
scale_y_continuous(name="Number of contracts)") +
theme(axis.text.x = element_text(vjust=0.5, size=5),
      axis.text.y = element_text(size=5) ) +
scale_colour_gradientn(colours=rainbow(4))
bp
## The graph has a very long tail due to some large framework agreements.
## Kudos to the Greek team who were able to get 940 offers
# for 903 different childcare contracts
# http://ted.europa.eu/udl?uri=TED:NOTICE:267981-2015:TEXT:EN:HTML&tabId=1#id12497736-II.
# and also to UK Crown Commercial Services
# who obtained 206 offers in 206 cases for temporary stagg
# http://ted.europa.eu/udl?uri=TED:NOTICE:267981-2015:TEXT:EN:HTML&tabId=1#id12497736-II.


## Plot frequency of number of offers 0-50 offers only
my_breaks = c(100000,50000,1000,10)
bp = ggplot(offers_data[1:50,], aes(x=NUMBER_OFFERS, y=freq, colour=freq))+
 geom_point() + #geom_bar(width = 1, stat = "identity")+
scale_fill_continuous(name="Number of contracts", breaks=my_breaks, labels=my_breaks) +
scale_x_continuous(name = 'Number of offers') +
scale_y_continuous(name="Number of contracts") +
theme(axis.text.x = element_text(vjust=0.5, size=5),
      axis.text.y = element_text(size=5) ) +
scale_colour_gradient(name="Number of contracts",low='#000000', high='#FF0000', trans="log", labels=comma)
bp

## Main activity
activity_data = ddply(x, .(MAIN_ACTIVITY), summarize, freq=length(MAIN_ACTIVITY), total_value=sum(AWARD_VALUE_EURO, na.rm=TRUE))
activity_data$MAIN_ACTIVITY_TOP_LEVEL <- gsub("\\,.*","",activity_data$MAIN_ACTIVITY)
activity_data2 = ddply(activity_data, .(MAIN_ACTIVITY_TOP_LEVEL), summarize, freq=sum(freq), total_value=sum(total_value, na.rm=TRUE))
activity_data2$total_value_bn <- (total_value/100000000)
bp = ggplot(activity_data2, aes(x=reorder(MAIN_ACTIVITY_TOP_LEVEL,total_value),y=total_value_bn, fill=total_value_bn,label=paste("€",round(total_value_bn,1), "bn")))+
 geom_bar(width = 1, stat = "identity", position="dodge") +
 geom_text(size=2, position = position_dodge(0.9), colour="#990000") +
 #geom_text(size=2,data=activity_data2, aes(x=MAIN_ACTIVITY_TOP_LEVEL,y=total_value_bn, label=dollar_format(prefix="€", suffix='bn'))) +
#geom_text( data=activity_data2,aes(x=reorder(MAIN_ACTIVITY_TOP_LEVEL,total_value),y=total_value_bn), label=dollar_format(prefix="€", suffix='bn')) +
 scale_fill_continuous(name="Total Awarded Value",low="#00FF00", high='#FF00FF') +
 scale_x_discrete(name = 'Main Activity') +
 scale_y_continuous(name="Value of contracts (billion Euros)", labels = dollar_format(prefix="€", suffix="bn")) +
theme(axis.text.y = element_text(vjust=0.5, size=5),
      axis.text.x = element_text(size=5, angle=45) ) +
#scale_colour_gradientn(colours=rainbow(4)) +
coord_flip()
bp

## 5 highest , 5 lowest contracts
## Highest, lowest contract by country


bp = ggplot(country_by_country_data, aes(x=OWN_COUNTRY,y=total_value, fill=reorder(countrycode(WIN_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match),total_value),label=comma(total_value)))+
 geom_bar(width = 1, stat = 'identity')+
scale_y_continuous(name='Value of contracts', labels = dollar_format(prefix='€'))+
facet_wrap(~countrycode(ISO_COUNTRY_CODE, "iso2c", "country.name", custom_match=custom_match), ncol=4, scales="free") +
theme(axis.text.y = element_text(size=2, vjust=0.5),
      axis.text.x = element_text(size=2),
      legend.position="none",
      strip.background = element_blank()) +
scale_colour_gradientn(colours=rainbow(4)) +
coord_flip()
bp

