library("Tejapi")
Tejapi.api_key("1WpypSQZfNGk13As2EZ7kZlZgxud06")
yourdata<- Tejapi('TWN/APRCD',coid='Y9999', mdate.gt='2018-01-01',mdate.lt='2018-02-01', paginate=TRUE)

yourdata <- Tejapi('TRAIL/TAPRCD', mdate.gt='2018-01-01',mdate.lt='2018-02-01',
                   paginate=TRUE)
