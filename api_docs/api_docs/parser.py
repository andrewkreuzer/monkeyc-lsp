import re


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

    def parse_parameters(self, until: set[str] | None = None):
        parameters = []
        while self.lexer.peek() != None and self.lexer.peek() not in until:
            param = self.parse_parameter(until)
            parameters.append(param)

        return parameters

    def parse_parameter(self, until: set[str] | None = None):
        param = {}
        while True:
            if until and self.lexer.peek() in until:
                return param

            if param.get("name") is None:
                if "..." in self.lexer.peek():
                    param["name"] = self.parse_name(r"^[a-zA-Z_][a-zA-Z0-9_.]*$")
                    param["exampleParameter"] = True
                else:
                    param["name"] = self.parse_name(r"^:?[a-zA-Z_][a-zA-Z0-9_]*$")
                continue

            t = self.lexer.next()
            if t == None or t == ",":
                return param

            if t == "as":
                if param.get("types") is None:
                    param["types"] = []
                param_type = self.parse_type(until)
                param["types"].extend(param_type)

    def parse_type(self, until: set[str] | None = None, return_type: bool = False):
        param_type = []
        re_type = re.compile(r"^[a-zA-Z_][.a-zA-Z0-9_]*$")
        while True:
            if until and self.lexer.peek() in until or self.lexer.peek() == ",":
                return param_type

            t = self.lexer.next()
            if t == None:
                return param_type

            if t == "{":
                param_type.append(
                    {
                        "type": "Lang.Dictionary",
                        "keys": self.parse_parameters(set(["}"])),
                    }
                )
                continue

            if t == "Lang.Method":
                if self.lexer.peek() == "(":
                    _ = self.lexer.next()
                parameters = self.parse_parameters(set([")"]))
                _ = self.lexer.next()  # consume the closing parenthesis
                param_type.append(
                    {
                        "type": "Lang.Method",
                        "parameters": parameters,
                        "returns": self.parse_type(until, return_type=True),
                    }
                )
                continue

            if t == "Lang.Dictionary":
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

            if t == "Lang.Array":
                if self.lexer.peek() == "<":
                    _ = self.lexer.next()
                param_type.append(
                    {
                        "type": "Lang.Array",
                        "valueType": self.parse_type(set([">"]))
                    })
                continue

            if t == "or":
                while self.lexer.peek() == "or":
                    _until = set(["or"])
                    if until:
                        _until.union(until)
                    t = self.parse_type(_until)
                    param_type.extend(t)
                continue

            if t == "as":
                continue

            if re_type.match(t):
                param_type.append(t)
                if t == "Void":
                    return param_type


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


class SignatureAST:
    def __init__(self, name, parameters, returns):
        self.name = name
        self.parameters = parameters
        self.returns = returns
    def __str__(self):
        return f"{self.name}({', '.join(self.parameters)}) -> {self.returns}"

def parse_signature(signature: str):
    lexer = Lexer(signature)
    parser = Parser(lexer)

    # assume the first token is the name
    name = parser.parse_name()
    _ = lexer.next()  # consume the opening parenthesis
    parameters = parser.parse_parameters(set([")"]))
    returns = parser.parse_type()

    return SignatureAST(name, parameters, returns)

if __name__ == "__main__":
    import json
    with open ("./sigs/invoke.txt", "r") as f:
        signature = f.read()
        print(json.dumps(parse_signature(signature), indent=2))
