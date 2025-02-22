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
	if self.readPosition > string.len(self.input) then
		self.ch = ""
	else
		self.ch = string.sub(self.input, self.readPosition, self.readPosition)
	end
	self.position = self.readPosition
	self.readPosition = self.readPosition + 1
end

---Reads strings for the lexer
---@return string
function Lexer:readString()
	local position = self.position + 1
	while true do
		self:readChar()
		if self.ch == '"' or self.ch == 0 then
			break
		end
	end
	return string.sub(self.input, position, self.position-1)
end

---Retrieves next token and progresses lexer
---@return Token
function Lexer:nextToken()
	local tok

	self:skipWhitespace()

	if self.ch == "=" then
		if self:peekChar() == "=" then
			local ch = self.ch
			self:readChar()
			local literal = ch .. self.ch
			tok = token.Token:new{type=token.TokenType.EQ, literal=literal}
		else
			tok = newToken(token.TokenType.ASSIGN, self.ch)
		end
	elseif self.ch == "+" then
		tok = newToken(token.TokenType.PLUS, self.ch)
	elseif self.ch == "-" then
		tok = newToken(token.TokenType.MINUS, self.ch)
	elseif self.ch == "!" then
		if self:peekChar() == "=" then
			local ch = self.ch
			self:readChar()
			local literal = ch .. self.ch
			tok = token.Token:new{type=token.TokenType.NOT_EQ, literal=literal}
		else
			tok = newToken(token.TokenType.BANG, self.ch)
		end
	elseif self.ch == "/" then
		tok = newToken(token.TokenType.SLASH, self.ch)
	elseif self.ch == "*" then
		tok = newToken(token.TokenType.ASTERISK, self.ch)
	elseif self.ch == "<" then
		tok = newToken(token.TokenType.LT, self.ch)
	elseif self.ch == ">" then
		tok = newToken(token.TokenType.GT, self.ch)
	elseif self.ch == ";" then
		tok = newToken(token.TokenType.SEMICOLON, self.ch)
	elseif self.ch == "," then
		tok = newToken(token.TokenType.COMMA, self.ch)
	elseif self.ch == "(" then
		tok = newToken(token.TokenType.LPAREN, self.ch)
	elseif self.ch == ")" then
		tok = newToken(token.TokenType.RPAREN, self.ch)
	elseif self.ch == "{" then
		tok = newToken(token.TokenType.LBRACE, self.ch)
	elseif self.ch == "}" then
		tok = newToken(token.TokenType.RBRACE, self.ch)
	elseif self.ch == "" then
		tok = token.Token:new{literal = "", type = token.TokenType.EOF}
	elseif self.ch == '"' then
		tok = token.Token:new{literal = self:readString(), type = token.TokenType.STRING}
	else
		if M.isLetter(self.ch) then
			tok = token.Token:new{literal = self:readIdentifier()}
			tok.type = token.lookupIdent(tok.literal)
			-- Early exit necessary as we progress readPosition past the 
			-- last character of the identifier
			return tok
		elseif M.isDigit(self.ch) then
			tok = token.Token:new{
				type=token.TokenType.INT,
				literal=self:readNumber(),
			}
			-- Early exit necessary as we progress readPosition past the 
			-- last character of the number
			return tok
		else
			tok = newToken(token.TokenType.ILLEGAL, self.ch)
		end
	end
	self:readChar()
	return tok
end

---Reads number from input
---@return string
function Lexer:readNumber()
	local position = self.position
	while M.isDigit(self.ch) do
		self:readChar()
	end
	return string.sub(self.input, position, self.position-1)
end

---Determines if supplied single length string is a digit
---@param ch string
---@return boolean
local function isDigit(ch)
	if string.match(ch, "[0-9]") then
		return true
	end
	return false
end
M.isDigit = isDigit

---Returns the next char in input without incrementing position
---@return string
function Lexer:peekChar()
	if self.readPosition > string.len(self.input) then
		return ""
	end
	return string.sub(self.input, self.readPosition, self.readPosition)
end

function Lexer:skipWhitespace()
	while string.match(self.ch, "[ \t\n\r]") do
		self:readChar()
	end
end

---comment
---@return string
function Lexer:readIdentifier()
	local position = self.position
	while M.isLetter(self.ch) do
		self:readChar()
	end
	return string.sub(self.input, position, self.position-1)
end

---Takes a single character and determines if it's a letter
---@param ch string
---@return boolean
local function isLetter(ch)
	if string.match(ch, "[%a_]") then
		return true
	end
	return false
end
M.isLetter = isLetter


return M


