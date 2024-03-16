import scrapy
from scrapy.loader import ItemLoader

import html2text

from api_docs.items import ApiDocsItem
from api_docs.parser import parse_signature as sig_ast


def parse_docstring(docstring):
    if not docstring:
        return ""
    return html2text.html2text(docstring).strip().replace("\n", " ")


class ModulesSpider(scrapy.Spider):
    name = "modules"
    allowed_domains = ["developer.garmin.com"]
    start_urls = ["https://developer.garmin.com/connect-iq/api-docs/"]

    def parse(self, response):
        anchors = response.css("span.type a")

        i = ItemLoader(item=ApiDocsItem(), response=response)
        i.add_value("type", "namespace")
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
        i.add_value("type", "module" if namespace is None else "namespace")
        i.add_value("parent", namespace)
        i.add_value("name", name)
        i.add_value("url", response.url)
        i.add_value("docstring", docstring)
        i.add_value("modules", modules.css("a::text").getall())
        i.add_value("classes", classes.css("a::text").getall())
        i.add_value("constants", self.parse_constant(response.css("dl.constants")))
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
        i.add_value("type", "class")
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
        constants = []

        def _parse_table(constant_type, table):
            headings = [h.lower() for h in table.css("th::text").getall()]

            consts = []
            for i, row in enumerate(table.css("tr")):
                if i == 0:
                    continue
                row_values = [
                    v.strip()
                    for v in row.xpath("td/text()|td/p/text()").getall()
                    if v.strip()
                ]
                c = dict(zip(headings, row_values))
                c["type"] = constant_type
                consts.append(c)

            return consts

        constant_tables = response.xpath(".//dt")
        for module in constant_tables:
            constant_type = module.css("h3::text").get()
            table = module.xpath(".//following-sibling::table[1]")
            constants.append(_parse_table(constant_type, table))

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
        """Deprecated"""
        name = signature.split("(")[0]
        with open(f"sigs/{name}.txt", "w") as f:
            f.write(signature)
        return_string = signature.split(")")[1]
        void = "Void" in return_string
        nullable = "Null" in return_string

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
        signature = sig_ast(
            " ".join(
                v.strip()
                for v in method.css("h3.signature").xpath(".//text()").getall()
            ).strip()
        )
        depricated = method.css("div.deprecated").get() is not None
        return_types = method.css("div.tags ul.return span.type a::text").getall()

        def _parse_parameters(parameter_list, include_type=False):
            if not parameter_list:
                return None
            parameters = []
            for param in parameter_list:
                parameter = {}
                if name := param.css("span.name::text").get():
                    parameter["name"] = name

                if include_type:
                    parameter["types"] = set(
                        param.xpath(
                            "./span[contains(@class, 'type')]/span/a/text()"
                        ).getall()
                    )

                parameter["docstring"] = parse_docstring(param.css("div.inline").get())

                if dictionary_keys := _parse_parameters(param.xpath(".//li")):
                    parameter["dictionary_keys"] = dictionary_keys

                parameters.append(parameter)

            return parameters if len(parameters) > 0 else None

        parameters = _parse_parameters(method.css("div.tags ul.param").xpath("./li"))
        throws = _parse_parameters(method.css("div.tags ul.throws").xpath("./li"), include_type=True)

        return {
            "name": signature.name,
            "ast": signature.__dict__,
            "parameters": parameters,
            "nullable": "Null" in signature.returns,
            "void": "Void" in signature.returns,
            "depricated": depricated,
            "throws": throws,
            "returns": return_types,
        }
