local ast = require("ast.ast")
local object = require("object.object")
local lu = require("luaunit")
local M = {}

local TRUE = object.Bool:new{value = true}
local FALSE = object.Bool:new{value = false}
local NULL = object.Null:new()
M.TRUE = TRUE
M.FALSE = FALSE
M.NULL = NULL

---EvalStatements
---@param stmts ast.Statement[]
---@return object.Object
local function evalStatements(stmts)
	---@type object.Object
	local result
	for _, statement in ipairs(stmts) do
		result = M.eval(statement)
		if object.ReturnValue.isInstance(result) then
			local returnValue = result  --[[@as object.ReturnValue]]
			return returnValue.value
		end
	end
	return result
end

---Handles native bool to boolean object
---@param input boolean
local function nativeBoolToBooleanObject(input)
	if input then
		return TRUE
	end
	return FALSE
end

---Handles bang operator eval logic
---@param right object.Object
---@return object.Object
local function evalBangOperatorExpression(right)
	if right == TRUE then
		return FALSE
	elseif right == FALSE then
		return TRUE
	elseif right == NULL then
		return TRUE
	end
	return FALSE
end

---Handles the - prefix operator eval logic
---@param right object.Object
---@return object.Object
local function evalMinusPrefixOperatorExpression(right)
	if right:type() ~= object.ObjectTypes.INTEGER_OBJ then
		return NULL
	end
	local r = right --[[@as object.Integer]]
	local value = r.value
	return object.Integer:new{value = -value}
end

---Evaluate a prefix expression
---@param operator string
---@param right object.Object
local function evalPrefixExpression(operator, right)
	if operator == "!" then
		return evalBangOperatorExpression(right)
	elseif operator == "-" then
		return evalMinusPrefixOperatorExpression(right)
	end
	return NULL
end

---Handles eval of infixes of integers
---@param operator string
---@param left object.Object
---@param right object.Object
local function evalIntegerInfixExpression(operator, left, right)
	local l = left  --[[@as object.Integer]]
	local leftVal = l.value
	local r = right --[[@as object.Integer]]
	local rightVal = r.value

	if operator == "+" then
		return object.Integer:new{value = leftVal + rightVal}
	elseif operator == "-" then
		return object.Integer:new{value = leftVal - rightVal}
	elseif operator == "*" then
		return object.Integer:new{value = leftVal * rightVal}
	elseif operator == "/" then
		return object.Integer:new{value = leftVal / rightVal}
	elseif operator == "<" then
		return nativeBoolToBooleanObject(leftVal < rightVal)
	elseif operator == ">" then
		return nativeBoolToBooleanObject(leftVal > rightVal)
	elseif operator == "==" then
		return nativeBoolToBooleanObject(leftVal == rightVal)
	elseif operator == "!=" then
		return nativeBoolToBooleanObject(leftVal ~= rightVal)
	end
	return NULL
end

---Evaluate an infix expression
---@param operator string
---@param left object.Object
---@param right object.Object
---@return object.Object
local function evalInfixExpression(operator, left, right)
	if left:type() == object.ObjectTypes.INTEGER_OBJ and right:type() == object.ObjectTypes.INTEGER_OBJ then
		return evalIntegerInfixExpression(operator, left, right)
	elseif operator == "==" then
		return nativeBoolToBooleanObject(left == right)
	elseif operator == "!=" then
		return nativeBoolToBooleanObject(left ~= right)
	end
	return NULL
end

---Evaluate truthiness
---@param obj object.Object
---@return boolean
local function isTruthy(obj)
	if obj == NULL then
		return false
	elseif obj == TRUE then
		return true
	elseif obj == FALSE then
		return false
	end
	return true
end

---Evaluate if expressions
---@param ie ast.IfExpression
---@return object.Object
local function evalIfExpression(ie)
	local condition = M.eval(ie.condition)
	if isTruthy(condition) then
		return M.eval(ie.consequence)
	elseif ie.alternative ~= nil then
		return M.eval(ie.alternative)
	else
		return NULL
	end
end

---Handles program evaluation
---@param program ast.Program
---@return object.Object
local function evalProgram(program)
	---@type object.Object
	local result
	for _, statement in ipairs(program.statements) do
		result = M.eval(statement)
		if object.ReturnValue.isInstance(result) then
			return result --[[@as object.ReturnValue]]
		end
	end
	return result
end

---comment
---@param block 
---@return object.Object
local function evalBlockStatement(block)
end

---Eval function
---@param node ast.Node
---@return object.Object
local function eval(node)
	if ast.Program.isInstance(node) then
		local program = node  --[[@as ast.Program]]
		return evalStatements(program.statements)
	elseif ast.ExpressionStatement.isInstance(node) then
		local es = node  --[[@as ast.ExpressionStatement]]
		return eval(es.expression)
	elseif ast.IntegerLiteral.isInstance(node) then
		local intLit = node  --[[@as ast.IntegerLiteral]]
		return object.Integer:new{value = intLit.value}
	elseif ast.Bool.isInstance(node) then
		local bool = node  --[[@as ast.Bool]]
		return nativeBoolToBooleanObject(bool.value)
	elseif ast.PrefixExpression.isInstance(node) then
		local n = node  --[[@as ast.PrefixExpression]]
		local right = eval(n.right)
		return evalPrefixExpression(n.operator, right)
	elseif ast.InfixExpression.isInstance(node) then
		local n = node  --[[@as ast.InfixExpression]]
		local left = eval(n.left)
		local right = eval(n.right)
		return evalInfixExpression(n.operator, left, right)
	elseif ast.BlockStatement.isInstance(node) then
		local n = node  --[[@as ast.BlockStatement]]
		return evalStatements(n.statements)
	elseif ast.IfExpression.isInstance(node) then
		local ie = node  --[[@as ast.IfExpression]]
		return evalIfExpression(ie)
	elseif ast.ReturnStatement.isInstance(node) then
		local rs = node  --[[@as ast.ReturnStatement]]
		local val = eval(rs.returnValue)
		return object.ReturnValue:new{value = val}
	elseif ast.Program.isInstance(node) then
		local prog = node  --[[@as ast.Program]]
		return evalProgram(node)
	elseif ast.BlockStatement.isInstance(node) then
		return evalBlockStatement(node)
	end
	return nil
end
M.eval = eval

return M
