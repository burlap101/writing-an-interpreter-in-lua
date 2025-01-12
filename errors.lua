local utils = require("utils")
local M = {}

---@class Error
---@field code integer
---@field msg string
local Error = {code = 1, msg = "An error occurred."}
Error.__tostring = Error.msg
M.Error = Error

---@class ParseError:Error
---@field mt table
local ParseError = utils.inheritsFrom(Error)

---comment
---@param pe ParseError
---@return ParseError
function ParseError:new(pe)
	pe = setmetatable(pe or {}, {
		__index = self,
		__tostring = function ()
			return "ParseError: "..self.msg
		end
	})
	pe.code = pe.code or 201
	pe.msg = pe.msg or "a parser error occcurred."
	return pe
end
M.ParseError = ParseError

return M
