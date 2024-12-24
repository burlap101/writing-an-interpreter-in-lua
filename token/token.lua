local M = {}

---@enum TokenType
local TokenType = {
	ILLEGAL = "ILLEGAL",
	EOF = "EOF",
	-- Identifiers + literals
	IDENT = "IDENT", -- add, foobar, x, y, ...
	INT = "INT", -- 1343456
	--Operators
	ASSIGN = "=",
	PLUS = "+",
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



return M
