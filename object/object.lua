local utils = require("utils")
local md5 = require("hashings.md5")
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
	HASH_OBJ = "HASH",
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

---@class object.Hashable:object.Object
---@field hashKey fun(object.Object): object.HashKey
local Hashable = utils.inheritsFrom(Object)
M.Hashable = Hashable

---Determines if obj implements Hashable interface
---@param obj table
---@return boolean
function Hashable.isInstance(obj)
	return M.isHashable(obj)
end

---ABC method
---@return object.HashKey
function Hashable:hashKey()
	error("not implemented")
end

---Module function to test whether object is hashablee
---@param obj any
---@return boolean
local function isHashable(obj)
	return obj.hashKey ~= nil
end
M.isHashable = isHashable

---@class object.Integer:object.Hashable
---@field value integer
local Integer = utils.inheritsFrom(Hashable)
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

---@class object.Bool:object.Hashable
---@field value boolean
local Bool = utils.inheritsFrom(Hashable)
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

---@class object.String:object.Hashable
---@field value string
local String = utils.inheritsFrom(Hashable)
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
Builtin.metatable = { __index = Builtin }
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
Array.metatable = { __index = Array }
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

---@class object.HashKey:object.Object
---@field type object.ObjectType
---@field value integer
local HashKey = utils.inheritsFrom(Object)
HashKey.metatable = {
	__index = HashKey,
	__tostring = function(t)
		return t:toString()
	end
}
M.HashKey = HashKey

function HashKey:new(hk)
	hk = setmetatable(hk or {}, self.metatable)
	return hk
end

function HashKey:toString()
	return self.type .. "|" .. self.value
end

---Case for boolean as hash key
---@return object.HashKey
function Bool:hashKey()
	---@type integer
	local value
	if self.value then
		value = 1
	else
		value = 0
	end
	return HashKey:new { type = self:type(), value = value }
end

---Case for integer key
---@return object.HashKey
function Integer:hashKey()
	return HashKey:new { type = self:type(), value = self.value }
end

function String:hashKey()
	local h = md5:new(self.value)
	assert(h)
	return HashKey:new { type = self:type(), value = tonumber(h:hexdigest(), 16) }
end

---@class object.HashPair:object.Object
---@field key object.Object
---@field value object.Object
local HashPair = utils.inheritsFrom(Object)
HashPair.metatable = {
	__index = HashPair
}
M.HashPair = HashPair

function HashPair:new(hp)
	hp = setmetatable(hp or {}, self.metatable)
	return hp
end

---@alias object.HashKeyString string

---@class object.Hash:object.Object
---@field pairs {[object.HashKeyString]: object.HashPair}
local Hash = utils.inheritsFrom(Object)
Hash.metatable = {
	__index = Hash
}
M.Hash = Hash

---Constructor for Hash
---@param h object.Hash
---@return object.Hash
function Hash:new(h)
	h = setmetatable(h or {}, self.metatable)
	return h
end

---Determines if object is instance of Hash
---@param obj table
---@return boolean
function Hash.isInstance(obj)
	return getmetatable(obj) == Hash.metatable
end

---Getter for ObjectTypes string
---@return ObjectTypes
function Hash:type()
	return ObjectTypes.HASH_OBJ
end

function Hash:inspect()
	local out = ""
	---@type string[]
	local prs = {}
	for _, pr in pairs(self.pairs) do
		table.insert(
			prs,
			string.format(
				"%s: %s",
				pr.key:inspect(),
				pr.value:inspect()
			)
		)
	end
	out = out .. "{"
	out = out .. table.concat(prs, ", ")
	out = out .. "}"
	return out
end

return M
