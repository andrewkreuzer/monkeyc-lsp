from api_docs.parser import parse_signature


def test_parser():
    signatures = [
        {
            "signature": "clear()  as Void",
            "name": "clear",
            "returns": [{"type": "Void"}],
            "parameters": [],
        },
        {
            "signature": "abs()  as Lang.Double",
            "name": "abs",
            "returns": [{"type": "Lang.Double"}],
            "parameters": [],
        },
        {
            "signature": "invoke(parameters...)  Lang.Object",
            "name": "invoke",
            "returns": [{"type": "Lang.Object"}],
            "parameters": [{"name": "parameters...", "exampleParameter": True}],
        },
        {
            "signature": "get()  as WatchUi.BitmapResource or Graphics.BufferedBitmap or WatchUi.FontResource or Null",
            "name": "get",
            "returns": [
                {"type": "WatchUi.BitmapResource"},
                {"type": "Graphics.BufferedBitmap"},
                {"type": "WatchUi.FontResource"},
                {"type": "Null"},
            ],
            "parameters": [],
        },
        {
            "signature": "keys()  as Lang.Array < Lang.Object >",
            "name": "keys",
            "returns": [{"type": "Lang.Array", "valueType": [{"type": "Lang.Object"}]}],
            "parameters": [],
        },
        {
            "signature": "acos(x as Lang.Numeric )  as Lang.Decimal",
            "name": "acos",
            "returns": [{"type": "Lang.Decimal"}],
            "parameters": [{"name": "x", "types": [{"type": "Lang.Numeric"}]}],
        },
        {
            "signature": "stdev(data as Lang.Array < Lang.Numeric >, xbar as Lang.Double or Null )  as Lang.Double",
            "name": "stdev",
            "returns": [{"type": "Lang.Double"}],
            "parameters": [
                {
                    "name": "data",
                    "types": [{"type": "Lang.Array", "valueType": [{"type": "Lang.Numeric"}]}],
                },
                {"name": "xbar", "types": [
                    {"type": "Lang.Double"}, {"type": "Null"}]},
            ],
        },
        {
            "signature": "onBikeRadarUpdate(data as Lang.Array < AntPlus.RadarTarget >)  as Void",
            "name": "onBikeRadarUpdate",
            "returns": [{"type": "Void"}],
            "parameters": [
                {
                    "name": "data",
                    "types": [
                        {
                            "type": "Lang.Array",
                            "valueType": [{"type": "AntPlus.RadarTarget"}],
                        }
                    ],
                }
            ],
        },
        {
            "signature": "enableSensorEvents(listener as Null or Lang.Method (info as Sensor.Info ) as Void )  as Void",
            "name": "enableSensorEvents",
            "returns": [{"type": "Void"}],
            "parameters": [
                {
                    "name": "listener",
                    "types": [
                        {"type": "Null"},
                        {
                            "type": "Lang.Method",
                            "parameters": [{"name": "info", "types": [{"type": "Sensor.Info"}]}],
                            "returns": [{"type": "Void"}],
                        },
                    ],
                }
            ],
        },
        {
            "signature": "getVectorFont(options as { :face as Lang.String or Lang.Array < Lang.String >, :size as Lang.Numeric })  as Graphics.VectorFont or Null",
            "name": "getVectorFont",
            "returns": [{"type": "Graphics.VectorFont"}, {"type": "Null"}],
            "parameters": [
                {
                    "name": "options",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":face",
                                    "types": [
                                        {"type": "Lang.String"},
                                        {
                                            "type": "Lang.Array",
                                            "valueType": [{"type": "Lang.String"}],
                                        },
                                    ],
                                },
                                {"name": ":size", "types": [{"type": "Lang.Numeric"}]},
                            ],
                        }
                    ],
                }
            ],
        },
        {
            "signature": "makeWebRequest(url as Lang.String , parameters as Lang.Dictionary or Null , options as { :method as Communications.HttpRequestMethod , :headers as Lang.Dictionary , :responseType as Communications.HttpResponseContentType , :context as Lang.Object or Null , :maxBandwidth as Lang.Number , :fileDownloadProgressCallback as Lang.Method (totalBytesTransferred as Lang.Number , fileSize as Lang.Number or Null ) as Void } or Null , responseCallback as Lang.Method (responseCode as Lang.Number , data as Lang.Dictionary or Lang.String or PersistedContent.Iterator or Null ) as Void or Lang.Method (responseCode as Lang.Number , data as Lang.Dictionary or Lang.String or PersistedContent.Iterator or Null , context as Lang.Object ) as Void )  as Void",
            "name": "makeWebRequest",
            "returns": [{"type": "Void"}],
            "parameters": [
                {"name": "url", "types": [{"type": "Lang.String"}]},
                {"name": "parameters", "types": ["Lang.Dictionary", {"type": "Null"}]},
                {
                    "name": "options",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":method",
                                    "types": [{"type": "Communications.HttpRequestMethod"}],
                                },
                                {"name": ":headers", "types": ["Lang.Dictionary"]},
                                {
                                    "name": ":responseType",
                                    "types": [{"type": "Communications.HttpResponseContentType"}],
                                },
                                {"name": ":context", "types": [{"type": "Lang.Object"}, {"type": "Null"}]},
                                {"name": ":maxBandwidth", "types": [{"type": "Lang.Number"}]},
                                {
                                    "name": ":fileDownloadProgressCallback",
                                    "types": [
                                        {
                                            "type": "Lang.Method",
                                            "parameters": [
                                                {
                                                    "name": "totalBytesTransferred",
                                                    "types": [{"type": "Lang.Number"}],
                                                },
                                                {
                                                    "name": "fileSize",
                                                    "types": [{"type": "Lang.Number"}, {"type": "Null"}],
                                                },
                                            ],
                                            "returns": [{"type": "Void"}],
                                        }
                                    ],
                                },
                            ],
                        },
                        {"type": "Null"},
                    ],
                },
                {
                    "name": "responseCallback",
                    "types": [
                        {
                            "type": "Lang.Method",
                            "parameters": [
                                {"name": "responseCode", "types": [{"type": "Lang.Number"}]},
                                {
                                    "name": "data",
                                    "types": [
                                        "Lang.Dictionary",
                                        {"type": "Lang.String"},
                                        {"type": "PersistedContent.Iterator"},
                                        {"type": "Null"},
                                    ],
                                },
                            ],
                            "returns": [{"type": "Void"}],
                        },
                        {
                            "type": "Lang.Method",
                            "parameters": [
                                {"name": "responseCode", "types": [{"type": "Lang.Number"}]},
                                {
                                    "name": "data",
                                    "types": [
                                        "Lang.Dictionary",
                                        {"type": "Lang.String"},
                                        {"type": "PersistedContent.Iterator"},
                                        {"type": "Null"},
                                    ],
                                },
                                {"name": "context", "types": [{"type": "Lang.Object"}]},
                            ],
                            "returns": [{"type": "Void"}],
                        },
                    ],
                },
            ],
        },
        {
            "signature": "createSession(options as { :sport as ActivityRecording.Sport or Activity.Sport , :subSport as ActivityRecording.SubSport or Activity.SubSport , :name as Lang.String , :poolLength as Lang.Float , :sensorLogger as SensorLogging.SensorLogger , :autoLap as { :type as Lang.Symbol , :entry as Lang.Array < Position.Location >, :exit as Lang.Array < Position.Location > } })  as ActivityRecording.Session",
            "name": "createSession",
            "returns": [{"type": "ActivityRecording.Session"}],
            "parameters": [
                {
                    "name": "options",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":sport",
                                    "types": [
                                        {"type": "ActivityRecording.Sport"},
                                        {"type": "Activity.Sport"},
                                    ],
                                },
                                {
                                    "name": ":subSport",
                                    "types": [
                                        {"type": "ActivityRecording.SubSport"},
                                        {"type": "Activity.SubSport"},
                                    ],
                                },
                                {"name": ":name", "types": [{"type": "Lang.String"}]},
                                {"name": ":poolLength", "types": [{"type": "Lang.Float"}]},
                                {
                                    "name": ":sensorLogger",
                                    "types": [{"type": "SensorLogging.SensorLogger"}],
                                },
                                {
                                    "name": ":autoLap",
                                    "types": [
                                        {
                                            "type": "Lang.Dictionary",
                                            "keys": [
                                                {
                                                    "name": ":type",
                                                    "types": [{"type": "Lang.Symbol"}],
                                                },
                                                {
                                                    "name": ":entry",
                                                    "types": [
                                                        {
                                                            "type": "Lang.Array",
                                                            "valueType": [
                                                                {"type": "Position.Location"}
                                                            ],
                                                        }
                                                    ],
                                                },
                                                {
                                                    "name": ":exit",
                                                    "types": [
                                                        {
                                                            "type": "Lang.Array",
                                                            "valueType": [
                                                                {"type": "Position.Location"}
                                                            ],
                                                        }
                                                    ],
                                                },
                                            ],
                                        }
                                    ],
                                },
                            ],
                        }
                    ],
                }
            ],
        },
        {
            "signature": "generateSignedOAuthHeader(url as Lang.String , params as Lang.Dictionary < Lang.String , Lang.Object >, requestMethod as Communications.HttpRequestMethod , signatureMethod as Communications.SigningMethod , token as Lang.String or Null , tokenSecret as Lang.String , consumerKey as Lang.String , consumerSecret as Lang.String )  as Lang.String",
            "name": "generateSignedOAuthHeader",
            "returns": [{"type": "Lang.String"}],
            "parameters": [
                {"name": "url", "types": [{"type": "Lang.String"}]},
                {
                    "name": "params",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keyType": [{"type": "Lang.String"}],
                            "valueType": [{"type": "Lang.Object"}],
                        }
                    ],
                },
                {
                    "name": "requestMethod",
                    "types": [{"type": "Communications.HttpRequestMethod"}],
                },
                {"name": "signatureMethod", "types": [{"type": "Communications.SigningMethod"}]},
                {"name": "token", "types": [{"type": "Lang.String"}, {"type": "Null"}]},
                {"name": "tokenSecret", "types": [{"type": "Lang.String"}]},
                {"name": "consumerKey", "types": [{"type": "Lang.String"}]},
                {"name": "consumerSecret", "types": [{"type": "Lang.String"}]},
            ],
        },
        {
            "signature": "playTone(options as Attention.Tone or { :toneProfile as Lang.Array < Attention.ToneProfile >, :repeatCount as Lang.Number })  as Void",
            "name": "playTone",
            "returns": [{"type": "Void"}],
            "parameters": [
                {
                    "name": "options",
                    "types": [
                        {"type": "Attention.Tone"},
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":toneProfile",
                                    "types": [
                                        {
                                            "type": "Lang.Array",
                                            "valueType": [{"type": "Attention.ToneProfile"}],
                                        }
                                    ],
                                },
                                {
                                    "name": ":repeatCount",
                                    "types": [
                                        {"type": "Lang.Number"},
                                    ],
                                },
                            ],
                        },
                    ],
                },
            ],
        },
        {
            "signature": "animate(object, property as Lang.Symbol , type as WatchUi.AnimationType , start as Lang.Numeric , stop as Lang.Numeric , period as Lang.Numeric , callback as Null or Lang.Method () as Void )",
            "name": "animate",
            "returns": [],
            "parameters": [
                {"name": "object"},
                {"name": "property", "types": [{"type": "Lang.Symbol"}]},
                {"name": "type", "types": [{"type": "WatchUi.AnimationType"}]},
                {"name": "start", "types": [{"type": "Lang.Numeric"}]},
                {"name": "stop", "types": [{"type": "Lang.Numeric"}]},
                {"name": "period", "types": [{"type": "Lang.Numeric"}]},
                {
                    "name": "callback",
                    "types": [
                        {"type": "Null"},
                        {"type": "Lang.Method", "parameters": [], "returns": [{"type": "Void"}]},
                    ],
                },
            ],
        },
    ]

    for signature in signatures:
        ast = parse_signature(signature["signature"])

        assert ast.name == signature["name"]
        assert ast.parameters == signature["parameters"]
        assert ast.returns == signature["returns"]
