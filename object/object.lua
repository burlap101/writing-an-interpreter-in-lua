local utils = require("utils")
local M = {}

---@alias object.ObjectType string
---@alias object.BuiltinFunction function(...):object.Object

---@enum ObjectTypes
local ObjectTypes = {
	INTEGER_OBJ = "INTEGER",
	BOOLEAN_OBJ = "BOOLEAN",
	NULL_OBJ = "NULL",
	RETURN_VALUE_OBJ = "RETURN_VALUE",
	ERROR_OBJ = "ERROR",
	FUNCTION_OBJ = "FUNCTION",
	STRING_OBJ = "STRING",
	BUILTIN_OBJ = "BUILTIN",
	ARRAY_OBJ = "ARRAY",
}
M.ObjectTypes = ObjectTypes

---@class object.Object
local Object = {}
M.Object = Object

---ABC method
---@return object.ObjectType
function Object:type()
	error("not implemented")
end

---ABC method
---@return object.Object
function Object:inspect()
	error("not implemented")
end

---@class object.Integer:object.Object
---@field value integer
local Integer = utils.inheritsFrom(Object)
Integer.metatable = {
	__index = Integer,
}
M.Integer = Integer

---Constructor for integer
---@param i object.Integer
---@return object.Integer
function Integer:new(i)
	i = setmetatable(i or {}, self.metatable)
	return i
end

---Determines if obj is an instance of Integer
---@param obj table
---@return boolean
function Integer.isInstance(obj)
	return getmetatable(obj) == Integer.metatable
end

---Gets the integer as a string
---@return string
function Integer:inspect()
	return tostring(self.value)
end

---Returns object type
---@return object.ObjectType
function Integer:type()
	return ObjectTypes.INTEGER_OBJ
end

---@class object.Bool:object.Object
---@field value boolean
local Bool = utils.inheritsFrom(Object)
Bool.metatable = {
	__index = Bool,
}
M.Bool = Bool

function Bool:new(b)
	b = setmetatable(b or {}, self.metatable)
	return b
end

M.Bool = Bool

---Determines if obj is an instance of Bool
---@param obj table
---@return boolean
function Bool.isInstance(obj)
	return getmetatable(obj) == Bool.metatable
end

function Bool:inspect()
	return tostring(self.value)
end

function Bool:type()
	return ObjectTypes.BOOLEAN_OBJ
end

---@class object.Null:object.Object
local Null = utils.inheritsFrom(Object)
M.Null = Null

function Null:new()
	local o = setmetatable({}, {
		__index = self,
		__tostring = function()
			return "NULL"
		end
	})
	return o
end

function Null:type()
	return ObjectTypes.NULL_OBJ
end

function Null:inspect()
	return "null"
end

---@class object.ReturnValue:object.Object
---@field value object.Object
local ReturnValue = utils.inheritsFrom(Object)
ReturnValue.metatable = {
	__index = ReturnValue,
}

---comment
---@param rv object.ReturnValue
---@return object.ReturnValue
function ReturnValue:new(rv)
	rv = setmetatable(rv or {}, self.metatable)
	return rv
end

function ReturnValue.isInstance(obj)
	return getmetatable(obj) == ReturnValue.metatable
end

---Returns the object type
---@return string
function ReturnValue:type()
	return ObjectTypes.RETURN_VALUE_OBJ
end

---Getter for value:inspect
---@return object.Object
function ReturnValue:inspect()
	return self.value:inspect()
end

M.ReturnValue = ReturnValue

---@class object.Error:object.Object
---@field message string
local Error = utils.inheritsFrom(Object)
Error.metatable = {
	__index = Error,
}
M.Error = Error

---Constructor for Error obj
---@param e object.Error
---@return object.Error
function Error:new(e)
	e = setmetatable(e or {}, self.metatable)
	return e
end

---Determine if supplied obj is Error type
---@param obj table
---@return boolean
function Error.isInstance(obj)
	return getmetatable(obj) == Error.metatable
end

---Getter for object type
---@return string
function Error:type()
	return ObjectTypes.ERROR_OBJ
end

---String representation of the error
---@return string
function Error:inspect()
	return "ERROR: " .. self.message
end

---@class object.Func:object.Object
---@field parameters ast.Identifier[]
---@field body ast.BlockStatement
---@field env environment.Environment
local Func = utils.inheritsFrom(Object)
Func.metatable = { __index = Func }
M.Func = Func

function Func:new(f)
	f = setmetatable(f or {}, self.metatable)
	return f
end

---Determines if object is an instance of Func
---@param obj any
---@return boolean
function Func.isInstance(obj)
	return getmetatable(obj) == Func.metatable
end

---Get ObjectType
---@return string
function Func:type()
	return ObjectTypes.FUNCTION_OBJ
end

---Get string representation of the function
---@return string
function Func:inspect()
	local out = ""
	---@type string[]
	local params = {}
	for _, p in ipairs(self.parameters) do
		table.insert(params, tostring(p))
	end
	out = out .. "fn"
	out = out .. "("
	out = out .. table.concat(params, ", ")
	out = out .. ") {\n"
	out = out .. "\n}"
	return out
end

---@class object.String:object.Object
---@field value string
local String = utils.inheritsFrom(Object)
String.metatable = { __index = String }
M.String = String

---Constructor for new string object
---@param s object.String
---@return object.String
function String:new(s)
	s = setmetatable(s or {}, self.metatable)
	return s
end

---Determines if object is type string
---@param obj table
---@return boolean
function String.isInstance(obj)
	return getmetatable(obj) == String.metatable
end

---Get object type
---@return string
function String:type()
	return ObjectTypes.STRING_OBJ
end

---Get string representation of string
---@return string
function String:inspect()
	return self.value
end

---@class object.Builtin:object.Object
---@field fn object.BuiltinFunction
local Builtin = utils.inheritsFrom(Object)
Builtin.metatable = {__index = Builtin}
M.Builtin = Builtin

---Constructor for a Builtin object
---@param bi object.Builtin
---@return object.Builtin
function Builtin:new(bi)
	bi = setmetatable(bi or {}, self.metatable)
	return bi
end

---Determines if object is Builtin type
---@param obj table
---@return boolean
function Builtin.isInstance(obj)
	return getmetatable(obj) == Builtin.metatable
end

---Gets object type
---@return string
function Builtin:type()
	return ObjectTypes.BUILTIN_OBJ
end

---String representation of builtin
---@return string
function Builtin:inspect()
	return "builtin function"
end

---@class object.Array:object.Object
---@field elements object.Object[]
local Array = utils.inheritsFrom(Object)
Array.metatable = {__index = Array}
M.Array = Array

---Constructor for an array object
---@param a object.Array
---@return object.Array
function Array:new(a)
	a = setmetatable(a or {}, self.metatable)
	return a
end

---Determines if obj is instance of Array
---@param obj table
---@return boolean
function Array.isInstance(obj)
	return getmetatable(obj) == Array.metatable
end

---Getter for object type
---@return string
function Array:type()
	return ObjectTypes.ARRAY_OBJ
end

---Object string representation
---@return string
function Array:inspect()
	local out = ""
	---@type string[]
	local elements = {}
	for _, e in ipairs(self.elements) do
		table.insert(elements, e:inspect())
	end
	out = out .. "["
	out = out .. table.concat(elements, ", ")
	out = out .. "]"
	return out
end

return M
