local M = {}

local lu = require("luaunit")
local token = require("token.token")

---Kindof like a constructor for a new token. Could just use the constructor
--directly tho
---@param tokenType TokenType
---@param ch string
---@return Token
local function newToken(tokenType, ch)
	return token.Token:new{type = tokenType, literal = ch}
end

---@class Lexer
---@field input string
---@field position integer  current position in input (points to current char)
---@field readPosition integer  current reading position in input (after current char)
---@field ch string  current char under examination
local Lexer = {}
M.Lexer = Lexer

---Constructor for a lexer
---@param input any
---@return Lexer
function Lexer:new(input)
	local l = setmetatable({}, {__index = self})
	l.input = input
	l.readPosition = 1
	l.position = 1
	l.ch = ""
	l:readChar()
	return l
end

function Lexer:readChar()
	if self.readPosition >= #self.input then
		self.ch = ""
	else
		self.ch = string.sub(self.input, self.readPosition, self.readPosition)
	end
	self.position = self.readPosition
	self.readPosition = self.readPosition + 1
end

---comment
---@return Token
function Lexer:nextToken()
	local tok
	if self.ch == "=" then
		tok = newToken(token.TokenType.ASSIGN, self.ch)
	elseif self.ch == ";" then
		tok = newToken(token.TokenType.SEMICOLON, self.ch)
	elseif self.ch == "(" then
		tok = newToken(token.TokenType.LPAREN, self.ch)
	elseif self.ch == ")" then
		tok = newToken(token.TokenType.RPAREN, self.ch)
	elseif self.ch == "," then
		tok = newToken(token.TokenType.COMMA, self.ch)
	elseif self.ch == "+" then
		tok = newToken(token.TokenType.PLUS, self.ch)
	elseif self.ch == "{" then
		tok = newToken(token.TokenType.LBRACE, self.ch)
	elseif self.ch == "}" then
		tok = newToken(token.TokenType.RBRACE, self.ch)
	elseif self.ch == "" then
		tok = token.Token:new{literal = "", type = token.TokenType.EOF}
	end
	self:readChar()
	return tok
end

return M


