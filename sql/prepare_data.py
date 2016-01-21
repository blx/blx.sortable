#!/usr/bin/env python2

import json
import csv

def write_csv(jsonlines, outcsv, fields):
    """Converts sequence of JSON object strings into CSV with fields ordered
    as in `fields`."""
    with open(outcsv, 'w') as out:
        csvf = csv.writer(out, quoting=csv.QUOTE_ALL)
        csvf.writerow(fields)
        for line in jsonlines:
            obj = json.loads(line)
            csvf.writerow([obj.get(k, '').encode('utf8')
                           for k in fields])


if __name__ == '__main__':
    from collections import namedtuple

    Input = namedtuple('Input', ['source', 'dest', 'fields'])

    inputs = [
        Input('resources/data/products.txt',
              'products.csv',
              ('product_name', 'manufacturer', 'family', 'model', 'announced-date')),
        Input('resources/data/listings.txt',
              'listings.csv',
              ('title', 'manufacturer', 'currency', 'price'))
    ]

    for f in inputs:
        with open(f.source) as source:
            write_csv(source, f.dest, f.fields)
