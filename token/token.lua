local M = {}

---@enum TokenType
local TokenType = {
	ILLEGAL = "ILLEGAL",
	EOF = "EOF",

	-- Identifiers + literals
	IDENT = "IDENT", -- add, foobar, x, y, ...
	INT = "INT", -- 1343456
	STRING = "STRING", -- "Hello world!"

	--Operators
	ASSIGN = "=",
	PLUS = "+",
	MINUS = "-",
	BANG = "!",
	ASTERISK = "*",
	SLASH = "/",
	EQ = "==",
	NOT_EQ = "!=",
	LT = "<",
	GT = ">",

	--Delimiters
	COMMA = ",",
	SEMICOLON = ";",
	LPAREN = "(",
	RPAREN = ")",
	LBRACE = "{",
	RBRACE = "}",

	--Keywords
	FUNCTION = "FUNCTION",
	LET = "LET",
	TRUE = "TRUE",
	FALSE = "FALSE",
	IF = "IF",
	ELSE = "ELSE",
	RETURN = "RETURN",
}
M.TokenType = TokenType

---@class Token
---@field type TokenType
---@field literal string
local Token = {}
M.Token = Token

---comment
---@param token Token
function Token:new(token)
	local t = setmetatable(token, { __index = self })
	return t
end

local keywords = {
	["fn"] = TokenType.FUNCTION,
	["let"] = TokenType.LET,
	["true"] = TokenType.TRUE,
	["false"] = TokenType.FALSE,
	["if"] = TokenType.IF,
	["else"] = TokenType.ELSE,
	["return"] = TokenType.RETURN,
}

---Checks identifier and returns appropriate type
---@param ident string
---@return TokenType
local function lookupIdent(ident)
	if keywords[ident] then
		return keywords[ident]
	end
	return TokenType.IDENT
end
M.lookupIdent = lookupIdent


return M
