from api_docs.parser import parse_signature


def test_parser():
    signatures = [
        {
            "signature": "clear()  as Void",
            "name": "clear",
            "returns": ["Void"],
            "parameters": [],
        },
        {
            "signature": "abs()  as Lang.Double",
            "name": "abs",
            "returns": ["Lang.Double"],
            "parameters": [],
        },
        {
            "signature": "invoke(parameters...)  Lang.Object",
            "name": "invoke",
            "returns": ["Lang.Object"],
            "parameters": [{"name": "parameters...", "exampleParameter": True}],
        },
        {
            "signature": "get()  as WatchUi.BitmapResource or Graphics.BufferedBitmap or WatchUi.FontResource or Null",
            "name": "get",
            "returns": [
                "WatchUi.BitmapResource",
                "Graphics.BufferedBitmap",
                "WatchUi.FontResource",
                "Null",
            ],
            "parameters": [],
        },
        {
            "signature": "keys()  as Lang.Array < Lang.Object >",
            "name": "keys",
            "returns": [{"type": "Lang.Array", "valueType": ["Lang.Object"]}],
            "parameters": [],
        },
        {
            "signature": "acos(x as Lang.Numeric )  as Lang.Decimal",
            "name": "acos",
            "returns": ["Lang.Decimal"],
            "parameters": [{"name": "x", "types": ["Lang.Numeric"]}],
        },
        {
            "signature": "stdev(data as Lang.Array < Lang.Numeric >, xbar as Lang.Double or Null )  as Lang.Double",
            "name": "stdev",
            "returns": ["Lang.Double"],
            "parameters": [
                {
                    "name": "data",
                    "types": [{"type": "Lang.Array", "valueType": ["Lang.Numeric"]}],
                },
                {"name": "xbar", "types": ["Lang.Double", "Null"]},
            ],
        },
        {
            "signature": "onBikeRadarUpdate(data as Lang.Array < AntPlus.RadarTarget >)  as Void",
            "name": "onBikeRadarUpdate",
            "returns": ["Void"],
            "parameters": [
                {
                    "name": "data",
                    "types": [
                        {
                            "type": "Lang.Array",
                            "valueType": ["AntPlus.RadarTarget"],
                        }
                    ],
                }
            ],
        },
        {
            "signature": "enableSensorEvents(listener as Null or Lang.Method (info as Sensor.Info ) as Void )  as Void",
            "name": "enableSensorEvents",
            "returns": ["Void"],
            "parameters": [
                {
                    "name": "listener",
                    "types": [
                        "Null",
                        {
                            "type": "Lang.Method",
                            "parameters": [{"name": "info", "types": ["Sensor.Info"]}],
                            "returns": ["Void"],
                        },
                    ],
                }
            ],
        },
        {
            "signature": "getVectorFont(options as { :face as Lang.String or Lang.Array < Lang.String >, :size as Lang.Numeric })  as Graphics.VectorFont or Null",
            "name": "getVectorFont",
            "returns": ["Graphics.VectorFont", "Null"],
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
                                        "Lang.String",
                                        {
                                            "type": "Lang.Array",
                                            "valueType": ["Lang.String"],
                                        },
                                    ],
                                },
                                {"name": ":size", "types": ["Lang.Numeric"]},
                            ],
                        }
                    ],
                }
            ],
        },
        {
            "signature": "makeWebRequest(url as Lang.String , parameters as Lang.Dictionary or Null , options as { :method as Communications.HttpRequestMethod , :headers as Lang.Dictionary , :responseType as Communications.HttpResponseContentType , :context as Lang.Object or Null , :maxBandwidth as Lang.Number , :fileDownloadProgressCallback as Lang.Method (totalBytesTransferred as Lang.Number , fileSize as Lang.Number or Null ) as Void } or Null , responseCallback as Lang.Method (responseCode as Lang.Number , data as Lang.Dictionary or Lang.String or PersistedContent.Iterator or Null ) as Void or Lang.Method (responseCode as Lang.Number , data as Lang.Dictionary or Lang.String or PersistedContent.Iterator or Null , context as Lang.Object ) as Void )  as Void",
            "name": "makeWebRequest",
            "returns": ["Void"],
            "parameters": [
                {"name": "url", "types": ["Lang.String"]},
                {"name": "parameters", "types": ["Lang.Dictionary", "Null"]},
                {
                    "name": "options",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":method",
                                    "types": ["Communications.HttpRequestMethod"],
                                },
                                {"name": ":headers", "types": ["Lang.Dictionary"]},
                                {
                                    "name": ":responseType",
                                    "types": ["Communications.HttpResponseContentType"],
                                },
                                {"name": ":context", "types": ["Lang.Object", "Null"]},
                                {"name": ":maxBandwidth", "types": ["Lang.Number"]},
                                {
                                    "name": ":fileDownloadProgressCallback",
                                    "types": [
                                        {
                                            "type": "Lang.Method",
                                            "parameters": [
                                                {
                                                    "name": "totalBytesTransferred",
                                                    "types": ["Lang.Number"],
                                                },
                                                {
                                                    "name": "fileSize",
                                                    "types": ["Lang.Number", "Null"],
                                                },
                                            ],
                                            "returns": ["Void"],
                                        }
                                    ],
                                },
                            ],
                        },
                        "Null",
                    ],
                },
                {
                    "name": "responseCallback",
                    "types": [
                        {
                            "type": "Lang.Method",
                            "parameters": [
                                {"name": "responseCode", "types": ["Lang.Number"]},
                                {
                                    "name": "data",
                                    "types": [
                                        "Lang.Dictionary",
                                        "Lang.String",
                                        "PersistedContent.Iterator",
                                        "Null",
                                    ],
                                },
                            ],
                            "returns": ["Void"],
                        },
                        {
                            "type": "Lang.Method",
                            "parameters": [
                                {"name": "responseCode", "types": ["Lang.Number"]},
                                {
                                    "name": "data",
                                    "types": [
                                        "Lang.Dictionary",
                                        "Lang.String",
                                        "PersistedContent.Iterator",
                                        "Null",
                                    ],
                                },
                                {"name": "context", "types": ["Lang.Object"]},
                            ],
                            "returns": ["Void"],
                        },
                    ],
                },
            ],
        },
        {
            "signature": "createSession(options as { :sport as ActivityRecording.Sport or Activity.Sport , :subSport as ActivityRecording.SubSport or Activity.SubSport , :name as Lang.String , :poolLength as Lang.Float , :sensorLogger as SensorLogging.SensorLogger , :autoLap as { :type as Lang.Symbol , :entry as Lang.Array < Position.Location >, :exit as Lang.Array < Position.Location > } })  as ActivityRecording.Session",
            "name": "createSession",
            "returns": ["ActivityRecording.Session"],
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
                                        "ActivityRecording.Sport",
                                        "Activity.Sport",
                                    ],
                                },
                                {
                                    "name": ":subSport",
                                    "types": [
                                        "ActivityRecording.SubSport",
                                        "Activity.SubSport",
                                    ],
                                },
                                {"name": ":name", "types": ["Lang.String"]},
                                {"name": ":poolLength", "types": ["Lang.Float"]},
                                {
                                    "name": ":sensorLogger",
                                    "types": ["SensorLogging.SensorLogger"],
                                },
                                {
                                    "name": ":autoLap",
                                    "types": [
                                        {
                                            "type": "Lang.Dictionary",
                                            "keys": [
                                                {
                                                    "name": ":type",
                                                    "types": ["Lang.Symbol"],
                                                },
                                                {
                                                    "name": ":entry",
                                                    "types": [
                                                        {
                                                            "type": "Lang.Array",
                                                            "valueType": [
                                                                "Position.Location"
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
                                                                "Position.Location"
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
            "returns": ["Lang.String"],
            "parameters": [
                {"name": "url", "types": ["Lang.String"]},
                {
                    "name": "params",
                    "types": [
                        {
                            "type": "Lang.Dictionary",
                            "keyType": ["Lang.String"],
                            "valueType": ["Lang.Object"],
                        }
                    ],
                },
                {
                    "name": "requestMethod",
                    "types": ["Communications.HttpRequestMethod"],
                },
                {"name": "signatureMethod", "types": ["Communications.SigningMethod"]},
                {"name": "token", "types": ["Lang.String", "Null"]},
                {"name": "tokenSecret", "types": ["Lang.String"]},
                {"name": "consumerKey", "types": ["Lang.String"]},
                {"name": "consumerSecret", "types": ["Lang.String"]},
            ],
        },
        {
            "signature": "playTone(options as Attention.Tone or { :toneProfile as Lang.Array < Attention.ToneProfile >, :repeatCount as Lang.Number })  as Void",
            "name": "playTone",
            "returns": ["Void"],
            "parameters": [
                {
                    "name": "options",
                    "types": [
                        "Attention.Tone",
                        {
                            "type": "Lang.Dictionary",
                            "keys": [
                                {
                                    "name": ":toneProfile",
                                    "types": [
                                        {
                                            "type": "Lang.Array",
                                            "valueType": ["Attention.ToneProfile"],
                                        }
                                    ],
                                },
                                {
                                    "name": ":repeatCount",
                                    "types": [
                                        "Lang.Number",
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
                {"name": "property", "types": ["Lang.Symbol"]},
                {"name": "type", "types": ["WatchUi.AnimationType"]},
                {"name": "start", "types": ["Lang.Numeric"]},
                {"name": "stop", "types": ["Lang.Numeric"]},
                {"name": "period", "types": ["Lang.Numeric"]},
                {
                    "name": "callback",
                    "types": [
                        "Null",
                        {"type": "Lang.Method", "parameters": [], "returns": ["Void"]},
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
