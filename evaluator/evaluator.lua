local ast = require("ast.ast")
local object = require("object.object")
local lu = require("luaunit")
local environment = require("object.environment")
local M = {}

local TRUE = object.Bool:new { value = true }
local FALSE = object.Bool:new { value = false }
local NULL = object.Null:new()
M.TRUE = TRUE
M.FALSE = FALSE
M.NULL = NULL


--Defines builtin functions variable
---@type {[string]: object.Builtin}
local builtins = {
	["len"] = object.Builtin:new{
		fn = function (...)
			return NULL
		end
	}
}

---Determines if object is an Error
---@param obj object.Object
---@return boolean
local function isError(obj)
	if obj ~= nil then
		return obj:type() == object.ObjectTypes.ERROR_OBJ
	end
	return false
end

---Creates a new Error obj
---@param format any
---@param ... unknown
---@return object.Error
local function newError(format, ...)
	return object.Error:new { message = string.format(format, ...) }
end

---Handles native bool to boolean object
---@param input boolean
local function nativeBoolToBooleanObject(input)
	if input then
		return TRUE
	end
	return FALSE
end

---Extends function environment
---@param fn object.Func
---@param args object.Object[]
---@return environment.Environment
local function extendFunctionEnv(fn, args)
	local env = environment.Environment:newEnclosedEnvironment(fn.env)
	for paramIdx, param in ipairs(fn.parameters) do
		env:set(param.value, args[paramIdx])
	end
	return env
end

---Unwrap return values and return otherwise return object passed in
---@param obj object.Object
---@return object.Object
local function unwrapReturnValue(obj)
	if object.ReturnValue.isInstance(obj) then
		local rv = obj --[[@as object.ReturnValue]]
		return rv.value
	end
	return obj
end

---Handles application of function
---@param fn object.Object
---@param args object.Object[]
---@return object.Object
local function applyFunction(fn, args)
	if not object.Func.isInstance(fn) then
		return newError("not a function: %s", fn:type())
	end
	local func = fn --[[@as object.Func]]
	local extendedEnv = extendFunctionEnv(func, args)
	local evaluated = M.eval(func.body, extendedEnv)
	return unwrapReturnValue(evaluated)
end

---Handles expressions eval
---@param exps ast.Expression[]
---@param env environment.Environment
---@return object.Object[]
local function evalExpressions(exps, env)
	---@type object.Object[]
	local result = {}

	for _, e in ipairs(exps) do
		local evaluated = M.eval(e, env)
		if isError(evaluated) then
			return { evaluated }
		end
		table.insert(result, evaluated)
	end
	return result
end

---Handles identifier eval logic
---@param node ast.Identifier
---@param env environment.Environment
---@return object.Object
local function evalIdentifier(node, env)
	local val, ok = env:get(node.value)
	if ok then
		return val
	end
	local builtin = builtins[node.value]
	if builtin ~= nil then
		return builtin
	end
	return newError("identifier not found: "..node.value)
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
		return newError("unknown operator: -%s", right:type())
	end
	local r = right --[[@as object.Integer]]
	local value = r.value
	return object.Integer:new { value = -value }
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
	return newError("unknown operator: %s%s", operator, right:type())
end

---Handles eval of infixes of integers
---@param operator string
---@param left object.Object
---@param right object.Object
local function evalIntegerInfixExpression(operator, left, right)
	local l = left --[[@as object.Integer]]
	local leftVal = l.value
	local r = right --[[@as object.Integer]]
	local rightVal = r.value

	if operator == "+" then
		return object.Integer:new { value = leftVal + rightVal }
	elseif operator == "-" then
		return object.Integer:new { value = leftVal - rightVal }
	elseif operator == "*" then
		return object.Integer:new { value = leftVal * rightVal }
	elseif operator == "/" then
		return object.Integer:new { value = leftVal / rightVal }
	elseif operator == "<" then
		return nativeBoolToBooleanObject(leftVal < rightVal)
	elseif operator == ">" then
		return nativeBoolToBooleanObject(leftVal > rightVal)
	elseif operator == "==" then
		return nativeBoolToBooleanObject(leftVal == rightVal)
	elseif operator == "!=" then
		return nativeBoolToBooleanObject(leftVal ~= rightVal)
	end
	return newError(
		"unknown operator: %s %s %s",
		left:type(),
		operator,
		right:type()
	)
end

---Eval string concatenation using + infix operator
---@param operator string
---@param left object.Object
---@param right object.Object
---@return object.Object
local function evalStringInfixExpression(operator, left, right)
	if operator ~= "+" then
		return newError(
		"unknown operator: %s %s %s",
		left:type(),
		operator,
		right:type()
	)
	end
	local l = left --[[@as object.String]]
	local leftVal = l.value
	local r = right --[[@as object.String]]
	local rightVal = r.value
	return object.String:new { value = leftVal .. rightVal }
end

---Evaluate an infix expression
---@param operator string
---@param left object.Object
---@param right object.Object
---@return object.Object
local function evalInfixExpression(operator, left, right)
	if left:type() == object.ObjectTypes.INTEGER_OBJ and right:type() == object.ObjectTypes.INTEGER_OBJ then
		return evalIntegerInfixExpression(operator, left, right)
	elseif left:type() == object.ObjectTypes.STRING_OBJ and right.type() == object.ObjectTypes.STRING_OBJ then
		return evalStringInfixExpression(operator, left, right)
	elseif operator == "==" then
		return nativeBoolToBooleanObject(left == right)
	elseif operator == "!=" then
		return nativeBoolToBooleanObject(left ~= right)
	elseif left:type() ~= right:type() then
		return newError(
			"type mismatch: %s %s %s",
			left:type(),
			operator,
			right:type()
		)
	end
	return newError(
		"unknown operator: %s %s %s",
		left:type(),
		operator,
		right:type()
	)
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
---@param env environment.Environment
---@return object.Object
local function evalIfExpression(ie, env)
	local condition = M.eval(ie.condition, env)
	if isError(condition) then
		return condition
	end
	if isTruthy(condition) then
		return M.eval(ie.consequence, env)
	elseif ie.alternative ~= nil then
		return M.eval(ie.alternative, env)
	else
		return NULL
	end
end

---Handles program evaluation
---@param program ast.Program
---@param env environment.Environment
---@return object.Object
local function evalProgram(program, env)
	---@type object.Object
	local result
	for _, statement in ipairs(program.statements) do
		result = M.eval(statement, env)
		if object.ReturnValue.isInstance(result) then
			local returnValue = result --[[@as object.ReturnValue]]
			return returnValue.value
		elseif object.Error.isInstance(result) then
			return result
		end
	end
	return result
end

---Evaluates block statements
---@param block ast.BlockStatement
---@param env environment.Environment
---@return object.Object
local function evalBlockStatement(block, env)
	---@type object.Object
	local result

	for _, statement in ipairs(block.statements) do
		result = M.eval(statement, env)

		if result ~= nil then
			local rt = result:type()
			if rt == object.ObjectTypes.RETURN_VALUE_OBJ or rt == object.ObjectTypes.ERROR_OBJ then
				return result
			end
		end
	end
	return result --[[@as object.Object]]
end

---Eval function
---@param node ast.Node
---@param env environment.Environment
---@return object.Object
local function eval(node, env)
	if ast.Program.isInstance(node) then
		local program = node --[[@as ast.Program]]
		return evalProgram(program, env)
	elseif ast.ExpressionStatement.isInstance(node) then
		local es = node --[[@as ast.ExpressionStatement]]
		return eval(es.expression, env)
	elseif ast.IntegerLiteral.isInstance(node) then
		local intLit = node --[[@as ast.IntegerLiteral]]
		return object.Integer:new { value = intLit.value }
	elseif ast.Bool.isInstance(node) then
		local bool = node --[[@as ast.Bool]]
		return nativeBoolToBooleanObject(bool.value)
	elseif ast.PrefixExpression.isInstance(node) then
		local n = node --[[@as ast.PrefixExpression]]
		local right = eval(n.right, env)
		if isError(right) then
			return right
		end
		return evalPrefixExpression(n.operator, right)
	elseif ast.InfixExpression.isInstance(node) then
		local n = node --[[@as ast.InfixExpression]]
		local left = eval(n.left, env)
		if isError(left) then
			return left
		end
		local right = eval(n.right, env)
		if isError(right) then
			return right
		end
		return evalInfixExpression(n.operator, left, right)
	elseif ast.IfExpression.isInstance(node) then
		local ie = node --[[@as ast.IfExpression]]
		return evalIfExpression(ie, env)
	elseif ast.ReturnStatement.isInstance(node) then
		local rs = node --[[@as ast.ReturnStatement]]
		local val = eval(rs.returnValue, env)
		if isError(val) then
			return val
		end
		return object.ReturnValue:new { value = val }
	elseif ast.BlockStatement.isInstance(node) then
		local bs = node --[[@as ast.BlockStatement]]
		return evalBlockStatement(bs, env)
	elseif ast.LetStatement.isInstance(node) then
		local ls = node --[[@as ast.LetStatement]]
		local val = eval(ls.value, env)
		if isError(val) then
			return val
		end
		env:set(ls.name.value, val)
	elseif ast.Identifier.isInstance(node) then
		local ident = node --[[@as ast.Identifier]]
		return evalIdentifier(ident, env)
	elseif ast.FunctionLiteral.isInstance(node) then
		local fl = node --[[@as ast.FunctionLiteral]]
		local params = fl.parameters
		local body = fl.body
		return object.Func:new { parameters = params, env = env, body = body }
	elseif ast.CallExpression.isInstance(node) then
		local ce = node --[[@as ast.CallExpression]]
		local func = eval(ce.func, env)
		if isError(func) then
			return func
		end
		local args = evalExpressions(ce.arguments, env)
		if #args == 1 and isError(args[1]) then
			return args[1]
		end
		return applyFunction(func, args)
	elseif ast.StringLiteral.isInstance(node) then
		local str = node --[[@as object.String]]
		return object.String:new { value = str.value }
	end
	return nil
end
M.eval = eval

return M
