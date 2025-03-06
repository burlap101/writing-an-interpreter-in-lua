local M = {}

---@class environment.Environment
---@field store {[string]: object.Object}
---@field outer environment.Environment?
local Environment = {}
M.Environment = Environment

---Constructor for Environment objects
---@param env environment.Environment?
---@return environment.Environment
function Environment:new(env)
	env = setmetatable(env or {}, { __index = self })
	env.store = {}
	return env
end

---Creates a new environment enclosed in outer
---@param outer environment.Environment
---@return environment.Environment
function Environment:newEnclosedEnvironment(outer)
	local env = Environment:new()
	env.outer = outer
	return env
end

---Get name from store (if it exists)
---@param name string
---@return object.Object
---@return boolean
function Environment:get(name)
	local obj = self.store[name]
	local ok = obj ~= nil
	if not ok and self.outer ~= nil then
		obj, ok = self.outer:get(name)
	end
	return obj, ok
end

---Set [name] in store with val
---@param name string
---@param val any
---@return object.Object
function Environment:set(name, val)
	self.store[name] = val
	return val
end

return M
