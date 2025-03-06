local ast = require("ast.ast")
local lu = require("luaunit")
local token = require("token.token")
local M = {}

---@enum Precedence
local Precedence = {
	LOWEST = 1,
	EQUALS = 2,
	LESSGREATER = 3,
	SUM = 4,
	PRODUCT = 5,
	PREFIX = 6,
	CALL = 7,
	INDEX = 8,
}
M.Precedence = Precedence

---@type {[TokenType]: Precedence}
local precedences = {
	[token.TokenType.EQ] = Precedence.EQUALS,
	[token.TokenType.NOT_EQ] = Precedence.EQUALS,
	[token.TokenType.LT] = Precedence.LESSGREATER,
	[token.TokenType.GT] = Precedence.LESSGREATER,
	[token.TokenType.PLUS] = Precedence.SUM,
	[token.TokenType.MINUS] = Precedence.SUM,
	[token.TokenType.SLASH] = Precedence.PRODUCT,
	[token.TokenType.ASTERISK] = Precedence.PRODUCT,
	[token.TokenType.LPAREN] = Precedence.CALL,
	[token.TokenType.LBRACKET] = Precedence.INDEX,
}

---@alias PrefixParseFn fun(): ast.Expression?
---@alias InfixParseFn fun(e: ast.Expression?): ast.Expression?

---@class Parser
---@field lexer Lexer
---@field curToken Token
---@field peekToken Token
---@field errors string[]
---@field prefixParseFns {[TokenType]: PrefixParseFn}
---@field infixParseFns {[TokenType]: InfixParseFn}
local Parser = {}
M.Parser = Parser

---Constructor for a parser
---@param lexer Lexer
---@return Parser
function Parser:new(lexer)
	local p = setmetatable({}, { __index = self })
	p.lexer = lexer
	p.errors = {}

	-- Progress parser two steps
	p:nextToken()
	p:nextToken()

	-- Declare and register all prefix functions
	p.prefixParseFns = {}
	p:registerPrefix(token.TokenType.IDENT, p:parseIdentifier())
	p:registerPrefix(token.TokenType.INT, p:parseIntegerLiteral())
	p:registerPrefix(token.TokenType.BANG, p:parsePrefixExpression())
	p:registerPrefix(token.TokenType.MINUS, p:parsePrefixExpression())
	p:registerPrefix(token.TokenType.TRUE, p:parseBoolean())
	p:registerPrefix(token.TokenType.FALSE, p:parseBoolean())
	p:registerPrefix(token.TokenType.LPAREN, p:parseGroupedExpression())
	p:registerPrefix(token.TokenType.IF, p:parseIfExpression())
	p:registerPrefix(token.TokenType.FUNCTION, p:parseFunctionLiteral())
	p:registerPrefix(token.TokenType.STRING, p:parseStringLiteral())
	p:registerPrefix(token.TokenType.LBRACKET, p:parseArrayLiteral())

	-- Declare and registr all infix functions
	p.infixParseFns = {}
	p:registerInfix(token.TokenType.PLUS, p:parseInfixExpression())
	p:registerInfix(token.TokenType.MINUS, p:parseInfixExpression())
	p:registerInfix(token.TokenType.SLASH, p:parseInfixExpression())
	p:registerInfix(token.TokenType.ASTERISK, p:parseInfixExpression())
	p:registerInfix(token.TokenType.EQ, p:parseInfixExpression())
	p:registerInfix(token.TokenType.NOT_EQ, p:parseInfixExpression())
	p:registerInfix(token.TokenType.LT, p:parseInfixExpression())
	p:registerInfix(token.TokenType.GT, p:parseInfixExpression())
	p:registerInfix(token.TokenType.LPAREN, p:parseCallExpression())
	p:registerInfix(token.TokenType.LBRACKET, p:parseIndexExpression())

	-- Return
	return p
end

---Parses a call expression
---@return InfixParseFn
function Parser:parseCallExpression()
	---Inner function definition
	---@param func ast.Expression
	local fn = function(func)
		local exp = ast.CallExpression:new{
			token = self.curToken,
			func = func,
		}
		exp.arguments = self:parseExpressionList(token.TokenType.RPAREN)
		return exp
	end
	return fn
end

---Parses function call arguments
---@return ast.Expression[]?
function Parser:parseCallArguments()
	---@type ast.Expression[]
	local args = {}
	if self:peekTokenIs(token.TokenType.RPAREN) then
		self:nextToken()
		return args
	end
	self:nextToken()
	table.insert(args, self:parseExpression(Precedence.LOWEST))

	while self:peekTokenIs(token.TokenType.COMMA) do
		self:nextToken()
		self:nextToken()
		table.insert(args, self:parseExpression(Precedence.LOWEST))
	end

	if not self:expectPeek(token.TokenType.RPAREN) then
		return nil
	end
	return args
end

---Parses a function literal
---@return PrefixParseFn
function Parser:parseFunctionLiteral()
	return function()
		local lit = ast.FunctionLiteral:new { token = self.curToken }
		if not self:expectPeek(token.TokenType.LPAREN) then
			return nil
		end
		lit.parameters = self:parseFunctionParameters()
		if not self:expectPeek(token.TokenType.LBRACE) then
			return nil
		end
		lit.body = self:parseBlockStatement()
		return lit
	end
end

---Parses function argument identifiers
---@return ast.Identifier[]?
function Parser:parseFunctionParameters()
	---@type ast.Identifier[]
	local identifiers = {}

	if self:peekTokenIs(token.TokenType.RPAREN) then
		self:nextToken()
		return identifiers
	end

	self:nextToken()

	local ident = ast.Identifier:new {
		token = self.curToken,
		value = self.curToken.literal
	}
	table.insert(identifiers, ident)

	while self:peekTokenIs(token.TokenType.COMMA) do
		self:nextToken()
		self:nextToken()
		ident = ast.Identifier:new {
			token = self.curToken,
			value = self.curToken.literal,
		}
		table.insert(identifiers, ident)
	end

	if not self:expectPeek(token.TokenType.RPAREN) then
		return nil
	end

	return identifiers
end

---Parses an if expression
---@return PrefixParseFn
function Parser:parseIfExpression()
	return function()
		local expression = ast.IfExpression:new { token = self.curToken }
		if not self:expectPeek(token.TokenType.LPAREN) then
			return nil
		end
		self:nextToken()

		-- Added default {} for nil case
		expression.condition = self:parseExpression(Precedence.LOWEST) or {}
		if not self:expectPeek(token.TokenType.RPAREN) then
			return nil
		end
		if not self:expectPeek(token.TokenType.LBRACE) then
			return nil
		end
		expression.consequence = self:parseBlockStatement()

		-- Parse the else if it exists
		if self:peekTokenIs(token.TokenType.ELSE) then
			self:nextToken()
			if not self:expectPeek(token.TokenType.LBRACE) then
				return nil
			end
			expression.alternative = self:parseBlockStatement()
		end

		return expression
	end
end

---Parses a block statement i.e. { ... }
---@return ast.BlockStatement
function Parser:parseBlockStatement()
	local block = ast.BlockStatement:new { token = self.curToken }
	---@type ast.Statement[]
	block.statements = {}
	self:nextToken()
	while not self:curTokenIs(token.TokenType.RBRACE) and not self:curTokenIs(token.TokenType.EOF) do
		local stmt = self:parseStatement()
		if stmt ~= nil then
			table.insert(block.statements, stmt)
		end
		self:nextToken()
	end
	return block
end

---Parses a boolean token into ast
---@return PrefixParseFn
function Parser:parseBoolean()
	return function()
		return ast.Bool:new {
			token = self.curToken,
			value = self:curTokenIs(token.TokenType.TRUE)
		}
	end
end

---Determines the precedence of the parser's peekToken
---@return integer
function Parser:peekPrecedence()
	local p = precedences[self.peekToken.type]
	if p ~= nil then
		return p
	end
	return Precedence.LOWEST
end

---Determines the precedence of the parser's curToken
function Parser:curPrecedence()
	local p = precedences[self.curToken.type]
	if p ~= nil then
		return p
	end
	return Precedence.LOWEST
end

---Parses an infix expression e.g. 5 + 5
---@return InfixParseFn
function Parser:parseInfixExpression()
	---@param left ast.Expression
	---@return ast.InfixExpression
	local fn = function(left)
		local expression = ast.InfixExpression:new {
			token = self.curToken,
			operator = self.curToken.literal,
			left = left
		}
		local precedence = self:curPrecedence()
		self:nextToken()
		expression.right = self:parseExpression(precedence)

		return expression
	end
	return fn
end

---Parses a prefix expression e.g. -5
---@return PrefixParseFn
function Parser:parsePrefixExpression()
	return function()
		local expression = ast.PrefixExpression:new {
			token = self.curToken,
			operator = self.curToken.literal,
		}
		self:nextToken()
		expression.right = self:parseExpression(Precedence.PREFIX)
		return expression
	end
end

---Paraenthesised expression parsing function
---@return PrefixParseFn
function Parser:parseGroupedExpression()
	return function()
		self:nextToken()
		local exp = self:parseExpression(Precedence.LOWEST)
		if not self:expectPeek(token.TokenType.RPAREN) then
			return nil
		end
		return exp
	end
end

---Return function that parses an identifier
---@return PrefixParseFn
function Parser:parseIdentifier()
	return function()
		return ast.Identifier:new {
			token = self.curToken,
			value = self.curToken.literal,
		}
	end
end

---Return function that parses an integer literal
---@return PrefixParseFn
function Parser:parseIntegerLiteral()
	return function()
		local lit = ast.IntegerLiteral:new { token = self.curToken }
		local numvalue = tonumber(self.curToken.literal)
		local success, value = pcall(math.floor, numvalue)
		if not success then
			local msg = "could not parse " .. self.curToken.literal .. " as integer"
			table.insert(self.errors, msg)
			return nil
		end
		lit.value = value
		return lit
	end
end

---Register prefix function for given token type
---@param tt TokenType
---@param fn PrefixParseFn
function Parser:registerPrefix(tt, fn)
	self.prefixParseFns[tt] = fn
end

---Register infix function for given token type
---@param tt TokenType
---@param fn InfixParseFn
function Parser:registerInfix(tt, fn)
	self.infixParseFns[tt] = fn
end

---Getter for errors, not sure if needed
---@return string[]
function Parser:getErrors()
	return self.errors
end

---Peeks the error for token type
---@param tt TokenType
function Parser:peekError(tt)
	local msg = "expected next token to be " .. tt .. " got " .. self.peekToken.type .. " instead"
	table.insert(self.errors, msg)
end

---Set tokens and progress lexer
function Parser:nextToken()
	self.curToken = self.peekToken
	self.peekToken = self.lexer:nextToken()
end

---Parse a program
---@return ast.Program
function Parser:parseProgram()
	local program = ast.Program:new()
	program.statements = {}

	while self.curToken.type ~= token.TokenType.EOF do
		local stmt = self:parseStatement()
		if stmt ~= nil then
			table.insert(program.statements, stmt)
		end
		self:nextToken()
	end
	return program
end

---Parse statement entry
---@return ast.Statement?
function Parser:parseStatement()
	if self.curToken.type == token.TokenType.LET then
		return self:parseLetStatement()
	elseif self.curToken.type == token.TokenType.RETURN then
		return self:parseReturnStatement()
	end
	return self:parseExpressionStatement()
end

function Parser:parseExpressionStatement()
	local stmt = ast.ExpressionStatement:new { token = self.curToken }
	stmt.expression = self:parseExpression(Precedence.LOWEST)
	if self:peekTokenIs(token.TokenType.SEMICOLON) then
		self:nextToken()
	end
	return stmt
end

---Parses a let statement
---@return ast.LetStatement?
function Parser:parseLetStatement()
	local stmt = ast.LetStatement:new { token = self.curToken }
	if not self:expectPeek(token.TokenType.IDENT) then
		return nil
	end
	stmt.name = ast.Identifier:new {
		token = self.curToken,
		value = self.curToken.literal
	}
	if not self:expectPeek(token.TokenType.ASSIGN) then
		return nil
	end
	self:nextToken()
	stmt.value = self:parseExpression(Precedence.LOWEST)
	if self:peekTokenIs(token.TokenType.SEMICOLON) then
		self:nextToken()
	end
	return stmt
end

function Parser:parseReturnStatement()
	local stmt = ast.ReturnStatement:new { token = self.curToken }
	self:nextToken()

	stmt.returnValue = self:parseExpression(Precedence.LOWEST)

	if self:peekTokenIs(token.TokenType.SEMICOLON) then
		self:nextToken()
	end

	return stmt
end

---Parse the expression for then given precedence
---@param precedence Precedence
---@return ast.Expression?
function Parser:parseExpression(precedence)
	local prefix = self.prefixParseFns[self.curToken.type]
	if prefix == nil then
		self:noPrefixParseFnError(self.curToken.type)
		return nil
	end
	local leftExp = prefix()
	while not self:peekTokenIs(token.TokenType.SEMICOLON) and precedence < self:peekPrecedence() do
		local infix = self.infixParseFns[self.peekToken.type]
		if infix == nil then
			return leftExp
		end
		self:nextToken()
		leftExp = infix(leftExp)
	end
	return leftExp
end

---Returns Prefix fn to parse Arrays
---@return PrefixParseFn
function Parser:parseArrayLiteral()
	return function()
		local array = ast.ArrayLiteral:new{token = self.curToken}
		array.elements = self:parseExpressionList(token.TokenType.RBRACKET)
		return array
	end
end

---Parse an expression list (e.g. array)
---@param endi TokenType
---@return ast.Expression[]?
function Parser:parseExpressionList(endi)
	---@type ast.Expression[]
	local list = {}
	if self:peekTokenIs(endi) then
		self:nextToken()
		return list
	end
	self:nextToken()
	table.insert(list, self:parseExpression(Precedence.LOWEST))
	while self:peekTokenIs(token.TokenType.COMMA) do
		self:nextToken()
		self:nextToken()
		table.insert(list, self:parseExpression(Precedence.LOWEST))
	end
	if not self:expectPeek(endi) then
		return nil
	end
	return list
end

---Determines if supplied token type matches current token type
---@param t TokenType
---@return boolean
function Parser:curTokenIs(t)
	return self.curToken.type == t
end

---Determines if supplied token type matches peek token type
---@param t TokenType
---@return boolean
function Parser:peekTokenIs(t)
	return self.peekToken.type == t
end

---Determines if the next token is the one expected
---@param t TokenType
---@return boolean
function Parser:expectPeek(t)
	if self:peekTokenIs(t) then
		self:nextToken()
		return true
	else
		self:peekError(t)
		return false
	end
end

---Creates and inserts error for no prefix parse func
---@param t TokenType
function Parser:noPrefixParseFnError(t)
	local msg = "no prefix parse function for " .. tostring(t) .. " found"
	table.insert(self.errors, msg)
end

---Parses a string literal
---@return PrefixParseFn
function Parser:parseStringLiteral()
	return function()
		return ast.StringLiteral:new{token = self.curToken, value = self.curToken.literal}
	end
end

---Infix function for parsing array index expressions
---@return InfixParseFn
function Parser:parseIndexExpression()
	return function(left)
		local exp = ast.IndexExpression:new{token = self.curToken, left = left}
		self:nextToken()
		exp.index = self:parseExpression(Precedence.LOWEST)
		if not self:expectPeek(token.TokenType.RBRACKET) then
			return nil
		end
		return exp
	end
end

return M
