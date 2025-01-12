LU = require("luaunit")

require("lexer.test_lexer")
require("parser.test_parser")
require("ast.test_ast")

os.exit(LU.LuaUnit.run())
