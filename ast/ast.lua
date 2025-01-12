local utils = require("utils")
local lu = require("luaunit")
local M = {}

---@class Node
local Node = {}
M.Node = Node

---Abstract method for a node
---@return string
function Node:tokenLiteral()
	error("not implemented")
end

---@class Statement:Node
local Statement = utils.inheritsFrom(Node)
M.Statement = Statement

---Abstract method for a statement node
function Statement:statementNode()
	error("not implemented")
end

---@class Expression:Node
local Expression = utils.inheritsFrom(Node)
M.Expression = Expression

---Abstract method for an expression node
function Expression:expressionNode()
	error("not implemented")
end

---@class Program:Node
---@field statements Statement[]
local Program = utils.inheritsFrom(Node)
M.Program = Program

function Program:new()
	local p = setmetatable({}, {
		__index = self,
		__tostring = function (t)
			return t:toString()
		end
	})
	return p
end

function Program:toString()
	local out = ""
	for _, s in ipairs(self.statements) do
		out = out .. tostring(s)
	end
	return out
end

---Retrieve first statement token literal
---@return string
function Program:tokenLiteral()
	if #self.statements > 0 then
		return self.statements[1]:tokenLiteral()
	else
		return ""
	end
end

---@class LetStatement:Statement
---@field token Token
---@field name Identifier
---@field value Expression?
local LetStatement = utils.inheritsFrom(Statement)
LetStatement.metatable = {
	__index = LetStatement,
	__tostring = function(t)
		return t:toString()
	end
}
M.LetStatement = LetStatement

function LetStatement:new(ls)
	ls = setmetatable(ls or {}, LetStatement.metatable)
	return ls
end

---Determines if the supplied object is an instance of LetStatement
---@param obj table
---@return boolean
function LetStatement.isInstance(obj)
	return getmetatable(obj) == LetStatement.metatable
end

function LetStatement:toString()
	local out = ""
	out = out .. self:tokenLiteral() .. " "
	out = out .. tostring(self.name)
	out = out .. " = "
	if self.value then
		out = out .. tostring(self.value)
	end
	out = out .. ";"
	return out
end

---Not sure
function LetStatement:statementNode()
end

---Getter for the token.literal
function LetStatement:tokenLiteral()
	return self.token.literal
end

---@class Identifier:Expression
---@field token Token
---@field value string
local Identifier = utils.inheritsFrom(Expression)
Identifier.metatable = {
	__index = Identifier,
	__tostring = function(t)
		return t:toString()
	end
}
M.Identifier = Identifier

function Identifier:new(i)
	i = setmetatable(i or {}, Identifier.metatable)
	return i
end

function Identifier.isInstance(obj)
	return getmetatable(obj) == Identifier.metatable
end

---Creates a string representation of the Identifier
---@return string
function Identifier:toString()
	return self.value
end

function Identifier:expressionNode()
end

function Identifier:tokenLiteral()
	return self.token.literal
end

---@class ReturnStatement:Statement
---@field token Token
---@field returnValue Expression?
local ReturnStatement = utils.inheritsFrom(Statement)
ReturnStatement.metatable = {
	__index = ReturnStatement,
	__tostring = function(t)
		return t:toString()
	end
}
M.ReturnStatement = ReturnStatement

function ReturnStatement:new(rs)
	rs = setmetatable(rs or {}, ReturnStatement.metatable)
	return rs
end

function ReturnStatement.isInstance(obj)
	return getmetatable(obj) == ReturnStatement.metatable
end

---Creates string representation of statement
---@return string
function ReturnStatement:toString()
	local out = ""
	out = out .. self:tokenLiteral() .. " "
	if self.returnValue then
		out = out .. tostring(self.returnValue)
	end
	out = out .. ";"
	return out
end

function ReturnStatement:statementNode()
end

---Getter for token.literal
---@return string
function ReturnStatement:tokenLiteral()
	return self.token.literal
end

---@class ExpressionStatement:Statement
---@field token Token
---@field expression Expression?
local ExpressionStatement = utils.inheritsFrom(Statement)
ExpressionStatement.metatable = {
	__index = ExpressionStatement,
	__tostring = function(t)
		return t:toString()
	end
}
M.ExpressionStatement = ExpressionStatement

function ExpressionStatement:new(es)
	es = setmetatable(es or {}, ExpressionStatement.metatable)
	return es
end

---Creates a string representation of the statement
---@return string
function ExpressionStatement:toString()
	if self.expression then
		return tostring(self.expression)
	end
	return ""
end

---Determines if object is an instance of expression statement
---@param obj table
---@return boolean
function ExpressionStatement.isInstance(obj)
	return getmetatable(obj) == ExpressionStatement.metatable
end

function ExpressionStatement:statementNode()
end

function ExpressionStatement:tokenLiteral()
	return self.token.literal
end

---@class IntegerLiteral:Expression
---@field token Token
---@field value integer
local IntegerLiteral = utils.inheritsFrom(Expression)
IntegerLiteral.metatable = {
	__index = IntegerLiteral,
	__tostring = function (t)
		return t:toString()
	end
}
M.IntegerLiteral = IntegerLiteral

function IntegerLiteral:new(il)
	il = setmetatable(il or {}, IntegerLiteral.metatable)
	return il
end

---Determines if obj is a IntegerLiteral
---@param obj table
---@return boolean
function IntegerLiteral.isInstance(obj)
	return getmetatable(obj) == IntegerLiteral.metatable
end

---Not sure if needed, think it just a Go interface req
function IntegerLiteral:expressionNode()
end

---Returns token.literal
---@return string
function IntegerLiteral:tokenLiteral()
	return self.token.literal
end

function IntegerLiteral:toString()
	return self.token.literal
end

---@class PrefixExpression:Expression
---@field token Token
---@field operator string
---@field right Expression?
local PrefixExpression = utils.inheritsFrom(Expression)
PrefixExpression.metatable = {
	__index = PrefixExpression,
	__tostring = function(t)
		return t:toString()
	end
}
M.PrefixExpression = PrefixExpression

function PrefixExpression:new(pe)
	pe = setmetatable(pe or {}, self.metatable)
	return pe
end

function PrefixExpression:expressionNode()
end

function PrefixExpression.isInstance(obj)
	return getmetatable(obj) == PrefixExpression.metatable
end

---Return token.literal
---@return string
function PrefixExpression:tokenLiteral()
	return self.token.literal
end

function PrefixExpression:toString()
	local out = ""
	out = out .. "("
	out = out .. self.operator
	out = out .. tostring(self.right)
	out = out .. ")"
	return out
end

---@class InfixExpression:Expression
---@field token Token  the operator token, e.g. +
---@field left Expression
---@field operator string
---@field right Expression?
local InfixExpression = utils.inheritsFrom(Expression)
InfixExpression.metatable = {
	__index = InfixExpression,
	__tostring = function(t)
		return t:toString()
	end
}
M.InfixExpression = InfixExpression

function InfixExpression:new(ie)
	ie = setmetatable(ie or {}, self.metatable)
	return ie
end

function InfixExpression.isInstance(obj)
	return getmetatable(obj) == InfixExpression.metatable
end

function InfixExpression:expressionNode()
end

---Getter for token.literal
---@return string
function InfixExpression:tokenLiteral()
	return self.token.literal
end

---Creates string representation of expression
---@return string
function InfixExpression:toString()
	local out = ""
	out = out .. "("
	out = out .. tostring(self.left)
	out = out .. " " .. self.operator .. " "
	if not pcall(tostring, self.right) then
		print(self.right:tokenLiteral())
	end
	out = out .. tostring(self.right)
	out = out .. ")"
	return out
end

---@class Bool:Expression
---@field token Token
---@field value boolean
local Bool = utils.inheritsFrom(Expression)
Bool.metatable = {
	__index = Bool,
	__tostring = function(t)
		return t:toString()
	end
}
M.Bool = Bool

---Constructor for Bool
---@param b Bool
---@return Bool
function Bool:new(b)
	b = setmetatable(b or {}, self.metatable)
	return b
end

---Determine if obj is instance of Bool
---@param obj table
---@return boolean
function Bool.isInstance(obj)
	return getmetatable(obj) == Bool.metatable
end

function Bool:expressionNode()
end

function Bool:tokenLiteral()
	return self.token.literal
end

function Bool:toString()
	return self.token.literal
end

---@class IfExpression:Expression
---@field token Token
---@field condition Expression
---@field consequence BlockStatement
---@field alternative BlockStatement
local IfExpression = utils.inheritsFrom(Expression)
IfExpression.metatable = {
	__index = IfExpression,
	__tostring = function(t)
		return t:toString()
	end
}
M.IfExpression = IfExpression

---Constructor for IfExpression
---@param ie IfExpression
---@return IfExpression
function IfExpression:new(ie)
	ie = setmetatable(ie or {}, self.metatable)
	return ie
end

---Determines if obj is an instance of IfExpression
---@param obj table
---@return boolean
function IfExpression.isInstance(obj)
	return getmetatable(obj) == IfExpression.metatable
end

function IfExpression:expressionNode()
end

function IfExpression:tokenLiteral()
	return self.token.literal
end

function IfExpression:toString()
	local out = ""
	out = out .. "if"
	out = out .. self.condition
	out = out .. " "
	out = out .. self.consequence
	if self.alternative ~= nil then
		out = out .. "else"
		out = out .. self.alternative
	end
	return out
end

---@class BlockStatement:Statement
---@field token Token
---@field statements Statement[]
local BlockStatement = utils.inheritsFrom(Statement)
BlockStatement.metatable = {
	__index = BlockStatement,
	__tostring = function(t)
		return t:toString()
	end
}
M.BlockStatement = BlockStatement

---Constructor for a block statement
---@param bs BlockStatement
---@return BlockStatement
function BlockStatement:new(bs)
	bs = setmetatable(bs or {}, self.metatable)
	return bs
end

function BlockStatement:statementNode()
end

---Getter for token.literal
---@return string
function BlockStatement:tokenLiteral()
	return self.token.literal
end

---String representation of BlockStatement
---@return string
function BlockStatement:toString()
	local out = ""
	for _, s in ipairs(self.statements) do
		out = out .. tostring(s)
	end
	return out
end

---@class FunctionLiteral:Expression
---@field token Token
---@field parameters Identifier?[]
---@field body BlockStatement?
local FunctionLiteral = utils.inheritsFrom(Expression)
FunctionLiteral.metatable = {
	__index = FunctionLiteral,
	__tostring = function(t)
		return t:toString()
	end
}
M.FunctionLiteral = FunctionLiteral

---Constructor for a function literal
---@param fl FunctionLiteral
---@return FunctionLiteral
function FunctionLiteral:new(fl)
	fl = setmetatable(fl or {}, self.metatable)
	return fl
end

---Type checking method
---@param obj table
---@return boolean
function FunctionLiteral.isInstance(obj)
	return getmetatable(obj) == FunctionLiteral.metatable
end

function FunctionLiteral:expressionNode()
end

---Getter for token.literal
---@return string
function FunctionLiteral:tokenLiteral()
	return self.token.literal
end

function FunctionLiteral:toString()
	local out = ""
	---@type string[]
	local params = {}
	for _, p in ipairs(self.parameters) do
		table.insert(params, tostring(p))
	end
	out = out .. self:tokenLiteral()
	out = out .. "("
	out = out .. table.concat(params, ", ")
	out = out .. ")"
	out = out .. self.body

	return out
end

---@class CallExpression
---@field token Token
---@field func Expression
---@field arguments Expression[]?
local CallExpression = utils.inheritsFrom(Expression)
CallExpression.metatable = {
	__index = CallExpression,
	__tostring = function(t)
		return t:toString()
	end
}
M.CallExpression = CallExpression

function CallExpression:new(ce)
	ce = setmetatable(ce, self.metatable)
	return ce
end

---Determine if obj isinstance of CallExpression
---@param obj table
---@return boolean
function CallExpression.isInstance(obj)
	return getmetatable(obj) == CallExpression.metatable
end

function CallExpression:expressionNode()
end

---
---@return string
function CallExpression:tokenLiteral()
	return self.token.literal
end

function CallExpression:toString()
	local out = ""

	---@type string[]
	local args = {}

	for _, a in ipairs(self.arguments) do
		table.insert(args, tostring(a))
	end

	out = out .. tostring(self.func)
	out = out .. "("
	out = out .. table.concat(args, ", ")
	out = out .. ")"
	return out
end

return M
