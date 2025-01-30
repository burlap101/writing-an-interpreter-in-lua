LU = require("luaunit")

require("lexer.test_lexer")
require("parser.test_parser")
require("ast.test_ast")
require("evaluator.test_evaluator")

os.exit(LU.LuaUnit.run())
