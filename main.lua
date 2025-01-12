
local repl = require("repl.repl")
local function main()
	local user = os.getenv("USER")
	if not user then
		error("no user found")
	end
	print("Hello "..user.."! This is the Monkey programming language!\n")
	print("Feel free to type in commands\n")
	repl.start(io.stdin, io.stdout)
end

main()
