load data infile '/Users/ben/projects/blx.sortable/listings.csv'
into table listing
fields terminated by ','
enclosed by '"'
lines terminated by '\r\n'
ignore 1 lines
(title, manufacturer, currency, price)
;