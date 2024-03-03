import scrapy
from scrapy.loader import ItemLoader

import html2text

from api_docs.items import ApiDocsItem
from api_docs.parser import parse_signature


def parse_docstring(docstring):
    if not docstring:
        return ""
    return html2text.html2text(docstring).strip()


class ModulesSpider(scrapy.Spider):
    name = "modules"
    allowed_domains = ["developer.garmin.com"]
    start_urls = ["https://developer.garmin.com/connect-iq/api-docs/"]

    def parse(self, response):
        anchors = response.css("span.type a")

        i = ItemLoader(item=ApiDocsItem(), response=response)
        i.add_value("t", "namespace")
        i.add_value("name", "Toybox")
        i.add_value("url", response.url)
        i.add_css("modules", "span.type a::text")
        i.add_value(
            "docstring",
            "The Toybox namespace is the top-level namespace for the Connect IQ SDK. It contains all the modules and classes that are available to Connect IQ apps.",
        )
        yield i.load_item()

        yield from response.follow_all(anchors, callback=self.parse_module)

    def parse_module(self, response, namespace=None):
        name = response.css("h1::text").get().replace("Module: ", "")
        docstring = parse_docstring(response.css("div.docstring").get())
        modules = response.xpath(
            "//p/strong[text()='Modules:']/following-sibling::span/a"
        )
        classes = response.xpath(
            "//p/strong[text()='Classes:']/following-sibling::span/a"
        )

        i = ItemLoader(item=ApiDocsItem(), response=response)
        i.add_value("t", "module" if namespace is None else "namespace")
        i.add_value("parent", namespace)
        i.add_value("name", name)
        i.add_value("url", response.url)
        i.add_value("docstring", docstring)
        i.add_value("modules", modules.css("a::text").getall())
        i.add_value("classes", classes.css("a::text").getall())
        i.add_value(
            "constants",
            [
                self.parse_constant(constant)
                for constant in response.css("dl.constants")
            ],
        )
        i.add_value(
            "typedefs",
            [
                self.parse_typedef(typedef)
                for typedef in response.xpath(
                    "//h2[text()='Typedef Summary ']/following-sibling::ul[1]/li"
                )
            ],
        )
        i.add_value(
            "methods",
            [
                self.parse_method(method)
                for method in response.css("div.method_details_list div.method_details")
            ],
        )

        if modules:
            yield from response.follow_all(
                modules, callback=self.parse_module, cb_kwargs={"namespace": name}
            )
        yield from response.follow_all(
            classes, callback=self.parse_class, cb_kwargs={"module": name}
        )

        yield i.load_item()

    def parse_class(self, response, module):
        docstring = parse_docstring(response.css("div.docstring").get())
        i = ItemLoader(item=ApiDocsItem(), response=response)
        i.add_value("t", "class")
        i.add_value("parent", module)
        i.add_value("name", response.css("h1::text").get().replace("Class: ", ""))
        i.add_value("url", response.url)
        i.add_value("docstring", docstring)
        i.add_value(
            "attributes",
            [
                self.parse_attribute(attr)
                for attr in response.css("div.attr_details dt")
            ],
        )
        i.add_value(
            "methods",
            [
                self.parse_method(method)
                for method in response.css("div.method_details_list div.method_details")
            ],
        )

        yield i.load_item()

    def parse_constant(self, response):
        constants = {}

        def _parse_table(table):
            headings = table.css("th::text").getall()

            constants = []
            for i, row in enumerate(table.css("tr")):
                if i == 0:
                    continue
                row_values = [
                    v.strip()
                    for v in row.xpath("td/text()|td/p/text()").getall()
                    if v.strip()
                ]
                constants.append(dict(zip(headings, row_values)))

            return constants

        constant_modules = response.xpath(".//dt")
        for module in constant_modules:
            module_name = module.css("h3::text").get()
            table = module.xpath(".//following-sibling::table[1]")
            tags = module.xpath(".//following-sibling::div[1]")
            constants[module_name] = {
                "module_constants": _parse_table(table),
                "tags": tags.css("p::text").getall(),
            }
        return constants

    def parse_typedef(self, typedef):
        return {
            "name": typedef.xpath(".//span/a/strong/text()").get(),
            "types": typedef.css("span.type a::text").getall(),
        }

    def parse_attribute(self, attribute):
        name = (
            attribute.css("h3.signature::text")
            .get()
            .replace("var", "")
            .replace("as", "")
            .strip()
        )
        attr_type = attribute.css("h3.signature span.type a::text").get()
        nullable = attribute.css("h3.signature b::text").get() == "Null"
        docstring = parse_docstring(attribute.css("div.docstring").get())
        returns = attribute.css("div.tags ul.return span.type a::text").get()

        return {
            "name": name,
            "docstring": docstring,
            "type": attr_type,
            "nullable": nullable,
            "returns": returns,
        }

    def parse_signature(self, signature):
        name = signature.split("(")[0]
        with open(f"sigs/{name}.txt", "w") as f:
            f.write(signature)
        return_string = signature.split(")")[1]
        void = "Void" in return_string
        nullable = "Null" in return_string

        # DONE: right an actual parser for this :(
        def _parse_parameters(parameter_string):
            parameters = {}
            for param in parameter_string:
                p = param.split("as")
                pn = p[0].strip()
                t = p[1] if len(p) > 1 else None

                parameters[pn] = (
                    [param_type.strip() for param_type in t.split("or")] if t else None
                )
            return parameters

        parameter_string = signature.split("(")[1].split(")")[0]
        parameters = _parse_parameters(parameter_string)

        # TODO: parse array types
        return_types = [
            ret_type.strip()
            for ret_type in return_string.replace("as", "").split("or")
            if "Void" or "Null" not in ret_type
        ]

        return {
            "name": name,
            "void": void,
            "parameters": parameters,
            "nullable": nullable,
            "return_types": return_types,
        }

    def parse_method(self, method):
        signature = parse_signature(
            " ".join(
                v.strip()
                for v in method.css("h3.signature").xpath(".//text()").getall()
            ).strip()
        )
        depricated = method.css("div.deprecated").get() is not None
        return_types = method.css("div.tags ul.return span.type a::text").getall()

        def _parse_parameters(parameter_list):
            if not parameter_list:
                return None
            parameters = []
            for param in parameter_list:
                pn = param.css("span.name::text").get()
                pt = set(
                    param.xpath(
                        ".//span[contains(@class, 'name')]/following-sibling::span"
                    )
                    .css("span.type a::text")
                    .getall()
                )
                # TODO: join parser's paramaters with the ones from the scraper
                # pt.update(signature["parameters"].get(pn) or [])
                ptdict = _parse_parameters(param.xpath(".//li"))
                pds = param.css("div.inline").get()
                parameters.append(
                    {
                        "parameter_name": pn,
                        "type": pt,
                        "type_dictionary_keys": ptdict,
                        "docstring": parse_docstring(pds),
                    }
                )
            return parameters if len(parameters) > 0 else None

        parameter_list = method.css("div.tags ul.param").xpath(
            ".//li[not(ancestor::li)]"
        )
        parameters = _parse_parameters(parameter_list)

        return {
            "method_name": signature["name"],
            "parameters": parameters,
            "nullable": "Null" in signature["type"],
            "void": "void" in signature["type"],
            "depricated": depricated,
            "returns": return_types,
        }
