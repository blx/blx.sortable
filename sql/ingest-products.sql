load data infile '/Users/ben/projects/blx.sortable/mysql-data.csv'
into table product
fields terminated by ','
enclosed by '"'
lines terminated by '\r\n'
ignore 1 lines
(name, manufacturer, family, model, @announced_date)
set
# Drop the time component of announced_date because
#   1. Easier than trying to wrangle timezones into mysql
#   2. Highly unlikely to be useful anyway
announced_date = str_to_date(left(@announced_date, 10), '%Y-%m-%d')
;