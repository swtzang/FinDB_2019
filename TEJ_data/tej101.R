library("Tejapi")
library(tidyquant)

Tejapi.api_key("1WpypSQZfNGk13As2EZ7kZlZgxud06")
# yourdata<- Tejapi('TRAIL/TAPRCD', mdate.gt='2018-01-01',mdate.lt='2018-02-01', paginate=TRUE)

yourdata <- Tejapi('TRAIL/TAPRCD', mdate.gt='2018-01-01',mdate.lt='2018-02-01',
                   opts.columns=c('coid', 'mdate', 'close_d', 'mv', 'pmkt'),
                   opts.sort='mdate.desc',
                   paginate=TRUE)
yourdata.tbl <- yourdata %>% as_tibble()
glimpse(yourdata.tbl)
#
data.tse <- yourdata.tbl %>% filter(pmkt == 'TSE')



