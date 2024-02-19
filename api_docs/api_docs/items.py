# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class ApiDocsItem(scrapy.Item):
    t = scrapy.Field()
    parent = scrapy.Field()
    name = scrapy.Field()
    url = scrapy.Field()
    docstring = scrapy.Field()
    attributes = scrapy.Field()
    modules = scrapy.Field()
    classes = scrapy.Field()
    constants = scrapy.Field()
    typedefs = scrapy.Field()
    methods = scrapy.Field()
