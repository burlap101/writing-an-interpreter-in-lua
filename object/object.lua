local utils = require("utils")
local M = {}

---@alias object.ObjectType string

---@enum ObjectTypes
local ObjectTypes = {
	INTEGER_OBJ = "INTEGER",
	BOOLEAN_OBJ = "BOOLEAN",
	NULL_OBJ = "NULL",
	RETURN_VALUE_OBJ = "RETURN_VALUE",
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
	local o = setmetatable({}, { __index = self, __tostring = function ()
		return "NULL"
	end})
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

return M
