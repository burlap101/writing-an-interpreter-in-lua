local lu = require("luaunit")
local lexer = require("lexer.lexer")
local token = require("token.token")

local M = {}
local PROMPT = ">>"

---comment
---@param inp file*
---@param outp file*
local function start(inp, outp)
	while true do
		outp:write(PROMPT)
		local scanned = inp:read("l")
		if not scanned then
			return
		end
		local l = lexer.Lexer:new(scanned)
		local tok = l:nextToken()
		while tok.type ~= token.TokenType.EOF do
			print()
			outp:write(lu.prettystr(tok).."\n")
			print()
			tok = l:nextToken()
		end
	end
end
M.start = start

return M

