LU = require("luaunit")
local token = require("token.token")
local lexer = require("lexer.lexer")

TestLexer = {}
	function TestLexer:testNextToken()
		local input = [[
			let five = 5;
			let ten = 10;

			let add = fn(x,y){
				x + y;
			};

			let result = add(five, ten);

			!-/*5;
			5 < 10 > 5;

			if(5 < 10) {
				return true;
			} else {
				return false;
			}

			10 == 10;
			10 != 9;
			"foobar"
			"foo bar"
		]]
		---@class TestCase
		---@field expectedType TokenType
		---@field expectedLiteral string

		---@type TestCase[]
		local tests = {
			{expectedType=token.TokenType.LET, expectedLiteral="let"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="five"},
			{expectedType=token.TokenType.ASSIGN, expectedLiteral="="},
			{expectedType=token.TokenType.INT, expectedLiteral="5"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.LET, expectedLiteral="let"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="ten"},
			{expectedType=token.TokenType.ASSIGN, expectedLiteral="="},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.LET, expectedLiteral="let"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="add"},
			{expectedType=token.TokenType.ASSIGN, expectedLiteral="="},
			{expectedType=token.TokenType.FUNCTION, expectedLiteral="fn"},
			{expectedType=token.TokenType.LPAREN, expectedLiteral="("},
			{expectedType=token.TokenType.IDENT, expectedLiteral="x"},
			{expectedType=token.TokenType.COMMA, expectedLiteral=","},
			{expectedType=token.TokenType.IDENT, expectedLiteral="y"},
			{expectedType=token.TokenType.RPAREN, expectedLiteral=")"},
			{expectedType=token.TokenType.LBRACE, expectedLiteral="{"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="x"},
			{expectedType=token.TokenType.PLUS, expectedLiteral="+"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="y"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.RBRACE, expectedLiteral="}"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.LET, expectedLiteral="let"},
			{expectedType=token.TokenType.IDENT, expectedLiteral="result"},
			{expectedType=token.TokenType.ASSIGN, expectedLiteral="="},
			{expectedType=token.TokenType.IDENT, expectedLiteral="add"},
			{expectedType=token.TokenType.LPAREN, expectedLiteral="("},
			{expectedType=token.TokenType.IDENT, expectedLiteral="five"},
			{expectedType=token.TokenType.COMMA, expectedLiteral=","},
			{expectedType=token.TokenType.IDENT, expectedLiteral="ten"},
			{expectedType=token.TokenType.RPAREN, expectedLiteral=")"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.BANG, expectedLiteral="!"},
			{expectedType=token.TokenType.MINUS, expectedLiteral="-"},
			{expectedType=token.TokenType.SLASH, expectedLiteral="/"},
			{expectedType=token.TokenType.ASTERISK, expectedLiteral="*"},
			{expectedType=token.TokenType.INT, expectedLiteral="5"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.INT, expectedLiteral="5"},
			{expectedType=token.TokenType.LT, expectedLiteral="<"},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.GT, expectedLiteral=">"},
			{expectedType=token.TokenType.INT, expectedLiteral="5"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.IF, expectedLiteral="if"},
			{expectedType=token.TokenType.LPAREN, expectedLiteral="("},
			{expectedType=token.TokenType.INT, expectedLiteral="5"},
			{expectedType=token.TokenType.LT, expectedLiteral="<"},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.RPAREN, expectedLiteral=")"},
			{expectedType=token.TokenType.LBRACE, expectedLiteral="{"},
			{expectedType=token.TokenType.RETURN, expectedLiteral="return"},
			{expectedType=token.TokenType.TRUE, expectedLiteral="true"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.RBRACE, expectedLiteral="}"},
			{expectedType=token.TokenType.ELSE, expectedLiteral="else"},
			{expectedType=token.TokenType.LBRACE, expectedLiteral="{"},
			{expectedType=token.TokenType.RETURN, expectedLiteral="return"},
			{expectedType=token.TokenType.FALSE, expectedLiteral="false"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.RBRACE, expectedLiteral="}"},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.EQ, expectedLiteral="=="},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.INT, expectedLiteral="10"},
			{expectedType=token.TokenType.NOT_EQ, expectedLiteral="!="},
			{expectedType=token.TokenType.INT, expectedLiteral="9"},
			{expectedType=token.TokenType.SEMICOLON, expectedLiteral=";"},
			{expectedType=token.TokenType.STRING, expectedLiteral="foobar"},
			{expectedType=token.TokenType.STRING, expectedLiteral="foo bar"},
			{expectedType=token.TokenType.EOF, expectedLiteral=""},
		}

		local l = lexer.Lexer:new(input)
		for _, tt in ipairs(tests) do
			local tok = l:nextToken()
			LU.assertEquals(tok.type, tt.expectedType)
			LU.assertEquals(tok.literal, tt.expectedLiteral)
		end
	end
--end of TestLexer class
