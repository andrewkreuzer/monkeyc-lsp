# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy
from itemloaders.processors import TakeFirst

class ApiDocsItem(scrapy.Item):
    type = scrapy.Field(output_processor=TakeFirst())
    parent = scrapy.Field(output_processor=TakeFirst())
    name = scrapy.Field(output_processor=TakeFirst())
    url = scrapy.Field(output_processor=TakeFirst())
    docstring = scrapy.Field(output_processor=TakeFirst())
    attributes = scrapy.Field()
    modules = scrapy.Field()
    classes = scrapy.Field()
    constants = scrapy.Field()
    typedefs = scrapy.Field()
    methods = scrapy.Field()
