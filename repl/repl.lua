local lu = require("luaunit")
local lexer = require("lexer.lexer")
local parser = require("parser.parser")
local evaluator = require("evaluator.evaluator")

local M = {}
local PROMPT = ">> "
local MONKEY_FACE = [[
    _____
____________
___O____O___
  ________
  __||||__
    ____
]]

---Print parser errors
---@param outp file*
---@param errors string[]
local function printParserErrors(outp, errors)
	outp:write(MONKEY_FACE)
	outp:write("Whoops! We ran into some monkey business here!\n")
	outp:write("parser errors:\n")
	for _, msg in ipairs(errors) do
		outp:write("\t" .. msg .. "\n")
	end
end

---Repl start func
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
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		if #p:getErrors() ~= 0 then
			printParserErrors(outp, p:getErrors())
			goto continue
		end
		local evaluated = evaluator.eval(program)
		if evaluated ~= nil then
			outp:write(tostring(evaluated:inspect()))
			outp:write("\n")
		end
		::continue::
	end
end
M.start = start

return M
