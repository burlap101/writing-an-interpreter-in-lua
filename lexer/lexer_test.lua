LU = require("luaunit")
local token = require("token.token")
local lexer = require("lexer.lexer")

TestLexer = {}
	function TestLexer:testNextToken()
		local input = "=+(){},;"
		---@class TestCase
		---@field expectedType TokenType
		---@field expectedLiteral string

		---@type TestCase[]
		local tests = {
			{expectedType=token.TokenType.ASSIGN, expected="="},
			{expectedType=token.TokenType.PLUS, expected="+"},
			{expectedType=token.TokenType.LPAREN, expected="("},
			{expectedType=token.TokenType.RPAREN, expected=")"},
			{expectedType=token.TokenType.LBRACE, expected="{"},
			{expectedType=token.TokenType.RBRACE, expected="}"},
			{expectedType=token.TokenType.COMMA, expected=","},
			{expectedType=token.TokenType.SEMICOLON, expected=";"},
			{expectedType=token.TokenType.EOF, expected=""},
		}

		local l = lexer.Lexer:new(input)
		for _, tt in ipairs(tests) do
			local tok = l:nextToken()
			LU.assertEquals(tok.type, tt.expectedType)
			LU.assertEquals(tok.literal, tt.expectedLiteral)
		end
	end
--end of TestLexer class
