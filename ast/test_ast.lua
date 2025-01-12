LU = require("luaunit")
local ast = require("ast.ast")
local token = require("token.token")

TestAST = {}
function TestAST:testToString()
	local p = ast.Program:new()
	p.statements = {
		ast.LetStatement:new {
			token = token.Token:new {
				type = token.TokenType.LET,
				literal = "let"
			},
			name = ast.Identifier:new {
				token = token.Token:new {
					type = token.TokenType.IDENT,
					literal = "myVar",
				},
				value = "myVar",
			},
			value = ast.Identifier:new {
				token = token.Token:new {
					type = token.TokenType.IDENT,
					literal = "anotherVar"
				},
				value = "anotherVar",
			}
		}
	}
	LU.assertEquals(tostring(p), "let myVar = anotherVar;")
end
