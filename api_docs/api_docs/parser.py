import re
import json


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer

    def parse_name(self, regex: str = r"^[a-zA-Z_][a-zA-Z0-9_]*$"):
        re_name = re.compile(regex)
        t = self.lexer.next()
        if t == None:
            return None
        if not re_name.match(t):
            raise ValueError(f"Invalid name: '{t}'")
        return t

    def parse_parameters(self, until: set[str | None] | None = None):
        parameters = []
        while self.lexer.peek() != None and self.lexer.peek() not in until:
            param = self.parse_parameter(until)
            parameters.append(param)

        return parameters

    def parse_parameter(self, until: set[str | None] | None = None):
        param = {}
        while True:
            if until and self.lexer.peek() in until:
                return param

            if param.get("name") is None:
                if "..." in self.lexer.peek():
                    param["name"] = self.parse_name(r"^[a-zA-Z_][a-zA-Z0-9_.]*$")
                    param["exampleParameter"] = True
                    continue
                param["name"] = self.parse_name(r"^:?[a-zA-Z_][a-zA-Z0-9_]*$")
                continue

            t = self.lexer.next()
            if t == None or t == ",":
                return param

            if t == "as":
                if param.get("types") is None:
                    param["types"] = []
                t = self.parse_type(until)
                param["types"].extend(t)

    def parse_type(self, until: set[str | None] | None = None):
        param_type = []
        re_type = re.compile(r"^[a-zA-Z_][.a-zA-Z0-9_]*$")
        while True:
            if until and self.lexer.peek() in until or self.lexer.peek() == ",":
                return param_type

            c = self.lexer.next()
            if c == None:
                return param_type

            if c == "{":
                param_type.append(
                    {
                        "type": "Lang.Dictionary",
                        "parameters": self.parse_parameters(set(["}"])),
                    }
                )
                continue

            if c == "Lang.Method":
                if self.lexer.peek() == "(":
                    _ = self.lexer.next()
                parameters = self.parse_parameters(set([")"]))
                _ = self.lexer.next()  # consume the closing parenthesis
                param_type.append(
                    {
                        "type": "Lang.Method",
                        "parameters": parameters,
                        "returns": self.parse_type(set([")", "or"])),
                    }
                )
                continue

            if c == "Lang.Dictionary":
                if self.lexer.peek() == "<":
                    _ = self.lexer.next()  # consume the opening angle bracket
                    key_type = self.parse_type(set([","]))
                    _ = self.lexer.next()  # consume the comma
                    value_type = self.parse_type(set([">"]))
                    param_type.append(
                        {
                            "type": "Lang.Dictionary",
                            "keyType": key_type,
                            "valueType": value_type,
                        }
                    )
                else:
                    param_type.append("Lang.Dictionary")
                continue

            if c == "Lang.Array":
                if self.lexer.peek() == "<":
                    _ = self.lexer.next()
                param_type.append(self.parse_type(set([">"])))
                continue

            if c == "or":
                while self.lexer.peek() == "or":
                    if until:
                        until.add("or")
                    t = self.parse_type(until)
                    param_type.extend(t)
                continue

            if c == "as":
                continue

            if re_type.match(c):
                param_type.append(c)


class Lexer:
    def __init__(self, signature):
        self.index = 0
        self.signature = signature
        match = r"\(|\)|{|}|<|>|,|:?[.A-Za-z0-9]+"
        self.token_list = list(re.findall(match, signature))
        self.tokens = (t for t in self.token_list)

    def get_token_list(self):
        return self.token_list

    def peek(self):
        if self.index >= len(self.token_list):
            return None
        return self.token_list[self.index]

    def next(self):
        self.index += 1
        try:
            return next(self.tokens)
        except:
            return None


def parse_signature(signature: str):
    lexer = Lexer(signature)
    parser = Parser(lexer)

    # assume the first token is the name
    name = parser.parse_name()
    _ = lexer.next()  # consume the opening parenthesis
    parameters = parser.parse_parameters(set([")"]))
    function_type = parser.parse_type()

    return {
        "name": name,
        "parameters": parameters,
        "type": function_type,
    }


# import glob
# files = glob.glob("./sigs/*.txt")
# # files = ["./sigs/generateSignedOAuthHeader.txt"]
# for file in files:
#     print(file)
#     with open(file, "r") as f:
#         signature = f.read()
#         lexer = Lexer(signature)
#         parser = Parser(lexer)

#         # assume the first token is the name
#         name = parser.parse_name()
#         _ = lexer.next() # consume the opening parenthesis
#         parameters = parser.parse_parameters(set([")"]))
#         function_type = parser.parse_type()

#         parsed_signature = {
#             "name": name,
#             "parameters": parameters,
#             "type": function_type,
#         }

#         print(json.dumps(parsed_signature, indent=2))
