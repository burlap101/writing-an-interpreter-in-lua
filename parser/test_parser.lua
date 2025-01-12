local lexer = require("lexer.lexer")
local parser = require("parser.parser")
local ast = require("ast.ast")
LU = require("luaunit")

TestParser = {}
---Invoked within a test
---@param p Parser
local function checkParserErrors(p)
	local errors = p.errors
	if #errors == 0 then
		return
	end
	local failMsg = "parser has " .. #errors .. " errors"
	for _, msg in ipairs(errors) do
		failMsg = failMsg .. "\nparser error: " .. msg
	end
	LU.fail(failMsg)
end


---comment
---@param s Statement
---@param name string
---@return boolean
local function testLetStatement(s, name)
	if s:tokenLiteral() ~= "let" then
		LU.fail("s.TokenLiteral not 'let'; got=" .. s:tokenLiteral())
		return false
	end
	local letStmt = s --[[@as LetStatement]]
	if not ast.LetStatement.isInstance(s) then
		LU.fail("s not ast.LetStatement; got=" .. LU.prettystr(getmetatable(s)))
		return false
	end
	if letStmt.name.value ~= name then
		LU.fail("letStmt.name.value not " .. name .. "; got=" .. letStmt.name.value)
		return false
	end
	if letStmt.name:tokenLiteral() ~= name then
		LU.fail("letStmt.name:tokenLiteral() not " .. name .. "; got=" .. letStmt.name:tokenLiteral())
		return false
	end
	return true
end

---Tests a given integer literal and value combo
---@param il Expression
---@param value integer
local function testIntegerLiteral(il, value)
	LU.assertTrue(ast.IntegerLiteral.isInstance(il))
	local integ = il --[[@as IntegerLiteral]]
	LU.assertEquals(integ.value, value)
	LU.assertEquals(integ:tokenLiteral(), tostring(value))
end


---Helper function section 2.8
---@param exp Expression
---@param value string
local function testIdentifier(exp, value)
	LU.assertIsTrue(ast.Identifier.isInstance(exp))
	local ident = exp --[[@as Identifier]]
	LU.assertEquals(ident.value, value)
	LU.assertEquals(ident:tokenLiteral(), value)
end

---comment
---@param exp Expression
---@param value boolean
local function testBooleanExpression(exp, value)
	LU.assertIsTrue(ast.Bool.isInstance(exp))
	local bo = exp --[[@as Bool]]
	LU.assertEquals(bo.value, value)
	LU.assertEquals(bo:tokenLiteral(), tostring(value))
end

---Helper function section 2.8
---@param exp Expression
---@param expected any
local function testLiteralExpression(exp, expected)
	local v = type(expected)
	if v == "number" then
		testIntegerLiteral(exp, expected)
	elseif v == "string" then
		testIdentifier(exp, expected)
	elseif v == "boolean" then
		testBooleanExpression(exp, expected)
	else
		LU.fail("type of exp not handled; got=" .. type(exp))
	end
end

---Helper function section 2.8
---@param exp Expression
---@param left any
---@param operator string
---@param right any
local function testInfixExpression(exp, left, operator, right)
	LU.assertTrue(ast.InfixExpression.isInstance(exp))
	local opExp = exp --[[@as InfixExpression]]
	testLiteralExpression(opExp.left, left)
	LU.assertEquals(opExp.operator, operator)
	testLiteralExpression(opExp.right, right)
end

function TestParser:testLetStatements()
	---@class TestCaseTestLetStatements
	---@field input string
	---@field expectedIdentifier string
	---@field expectedValue any

	local testsArr = {
		{ "let x = 5;",           "x",      5 },
		{ "let y = 10;",          "y",      10 },
		{ "let foobar = 838383;", "foobar", 838383 },
	}

	---@type TestCaseTestLetStatements[]
	local tests = {}
	for _, tc in ipairs(testsArr) do
		---@type TestCaseTestLetStatements
		local test = {
			input = tc[1],
			expectedIdentifier = tc[2],
			expectedValue = tc[3],
		}
		table.insert(tests, test)
	end

	for _, tt in pairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		if program == nil then
			LU.fail("program is nil")
			return
		end

		LU.assertEquals(#program.statements, 1)
		local stmt = program.statements[1]
		testLetStatement(stmt, tt.expectedIdentifier)
	end
end

function TestParser:testReturnStatements()
	---@class TestCaseTestReturnStatements
	---@field input string
	---@field expectedValue any

	---@type TestCaseTestReturnStatements[]
	local tests = {
		{ input = "return 5;",      expectedValue = 5 },
		{ input = "return 10;",     expectedValue = 10 },
		{ input = "return 993322;", expectedValue = 993322 },
	}

	for _, tt in ipairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()

		checkParserErrors(p)

		if program == nil then
			LU.fail("program is nil")
			return
		end

		LU.assertEquals(#program.statements, 1)
		LU.assertIsTrue(ast.ReturnStatement.isInstance(program.statements[1]))

		local returnStmt = program.statements[1] --[[@as ReturnStatement]]
		LU.assertEquals(returnStmt:tokenLiteral(), "return")
		testLiteralExpression(returnStmt.returnValue, tt.expectedValue)
	end
end

function TestParser:testIdentifierExpression()
	local input = "foobar;"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	if program == nil then
		LU.fail("program is nil")
		return
	end

	checkParserErrors(p)
	---@diagnostic disable-next-line: need-check-nil
	LU.assertEquals(#program.statements, 1)
	LU.assertTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	LU.assertTrue(ast.Identifier.isInstance(program.statements[1].expression))
	---@diagnostic disable-next-line: undefined-field
	local exp = program.statements[1].expression ---@as Expression
	LU.assertEquals(exp.value, "foobar")
end

function TestParser:testIntegerLiteralExpression()
	local input = "5;"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()

	checkParserErrors(p)
	if program == nil then
		LU.fail("program is nil")
		return
	end

	LU.assertEquals(#program.statements, 1)
	local stmt = program.statements[1]
	LU.assertTrue(ast.ExpressionStatement.isInstance(stmt))
	---@diagnostic disable-next-line: undefined-field
	local literal = stmt.expression --[[@as IntegerLiteral]]
	LU.assertTrue(ast.IntegerLiteral.isInstance(literal))
	LU.assertEquals(literal.value, 5)
	LU.assertEquals(literal:tokenLiteral(), "5")
end

function TestParser:testParsingPrefixExpressions()
	---@class TestCase
	---@field input string
	---@field operator string
	---@field value integer

	local prefixTestsArr = {
		{ "!5;",     "!", 5 },
		{ "-15;",    "-", 15 },
		{ "!true;",  "!", true },
		{ "!false;", "!", false },
	}
	---@type TestCase[]
	local prefixTests = {}
	for _, pt in ipairs(prefixTestsArr) do
		table.insert(prefixTests, {
			input = pt[1],
			operator = pt[2],
			value = pt[3],
		})
	end

	for i, tt in ipairs(prefixTests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		if program == nil then
			LU.fail("program is nil")
			return
		end
		LU.assertEquals(#program.statements, 1)

		local stmt = program.statements[1]
		LU.assertTrue(ast.ExpressionStatement.isInstance(stmt))

		local exp = stmt.expression --[[@as PrefixExpression]]
		LU.assertTrue(ast.PrefixExpression.isInstance(exp))

		if exp.operator ~= tt.operator then
			LU.fail("Line " .. i .. ": operator expected '" .. tt.operator .. "'; got '" .. exp.operator .. "'")
		end
		testLiteralExpression(exp.right, tt.value)
	end
end

function TestParser:testParsingInfixExpressions()
	---@class TestCaseParsingInfexExpressions
	---@field input string
	---@field leftValue integer
	---@field operator string
	---@field rightValue integer

	local infixTestsArr = {
		{ "5 + 5",          5,     "+",  5 },
		{ "5 - 5",          5,     "-",  5 },
		{ "5 * 5",          5,     "*",  5 },
		{ "5 / 5",          5,     "/",  5 },
		{ "5 > 5",          5,     ">",  5 },
		{ "5 < 5",          5,     "<",  5 },
		{ "5 == 5",         5,     "==", 5 },
		{ "5 != 5",         5,     "!=", 5 },
		{ "true == true",   true,  "==", true },
		{ "true != false",  true,  "!=", false },
		{ "false == false", false, "==", false },
	}

	---@type TestCaseParsingInfexExpressions[]
	local infixTests = {}
	for _, tc in ipairs(infixTestsArr) do
		---@type TestCaseParsingInfexExpressions
		local testCase = {
			input = tc[1],
			leftValue = tc[2],
			operator = tc[3],
			rightValue = tc[4],
		}
		table.insert(infixTests, testCase)
	end

	for _, tt in ipairs(infixTests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		if program == nil then
			LU.fail("program is nil")
			return
		end

		LU.assertEquals(#program.statements, 1)
		local stmt = program.statements[1]
		LU.assertTrue(ast.ExpressionStatement.isInstance(stmt))
		local exp = stmt.expression --[[@as InfixExpression ]]
		LU.assertTrue(ast.InfixExpression.isInstance(exp))
		testInfixExpression(exp, tt.leftValue, tt.operator, tt.rightValue)
	end
end

function TestParser:testOperatorPrecedenceParsing()
	---@class TestCaseOperatorPrecendenceParsing
	---@field input string
	---@field expected string

	local testsArr = {
		{
			"-a*b",
			"((-a) * b)",
		},
		{
			"!-a",
			"(!(-a))",
		},
		{
			"a + b + c",
			"((a + b) + c)",
		},
		{
			"a + b - c",
			"((a + b) - c)",
		},
		{
			"a * b * c",
			"((a * b) * c)",
		},
		{
			"a * b / c",
			"((a * b) / c)",
		},
		{
			"a + b / c",
			"(a + (b / c))",
		},
		{
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		},
		{
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
		{ "5 > 4 == 3 < 4",                            "((5 > 4) == (3 < 4))" },
		{ "5 < 4 != 3 > 4",                            "((5 < 4) != (3 > 4))" },
		{ "3 + 4 * 5 == 3 * 1 + 4 * 5",                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
		{ "true",                                      "true" },
		{ "false",                                     "false" },
		{ "3 > 5 == false",                            "((3 > 5) == false)" },
		{ "3 < 5 == true",                             "((3 < 5) == true)" },
		{ "1 + (2 + 3) + 4",                           "((1 + (2 + 3)) + 4)" },
		{ "(5 + 5) * 2",                               "((5 + 5) * 2)" },
		{ "2 / (5 + 5)",                               "(2 / (5 + 5))" },
		{ "-(5 + 5)",                                  "(-(5 + 5))" },
		{ "!(true == true)",                           "(!(true == true))" },
		{ "a + add(b * c) + d",                        "((a + add((b * c))) + d)" },
		{ "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
		{ "add(a + b + c * d / f + g)",                "add((((a + b) + ((c * d) / f)) + g))" },
	}

	---@type TestCaseOperatorPrecendenceParsing[]
	local tests = {}
	for _, tc in ipairs(testsArr) do
		---@type TestCaseOperatorPrecendenceParsing
		local test = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, test)
	end

	for _, tt in ipairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)
		assert(program, "program is nil")
		local actual = program:toString()
		LU.assertEquals(actual, tt.expected)
	end
end

function TestParser:testBooleanExpression()
	---@class TestCaseTestBooleanExpression
	---@field input string
	---@field expectedBoolean boolean

	---@type TestCaseTestBooleanExpression[]
	local tests = {
		{ input = "true;",  expectedBoolean = true },
		{ input = "false;", expectedBoolean = false },
	}

	for _, tt in ipairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		if program == nil then
			LU.fail("program is nil")
			return
		end

		LU.assertEquals(#program.statements, 1)
		LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
		local stmt = program.statements[1] --[[@as ExpressionStatement]]
		LU.assertIsTrue(ast.Bool.isInstance(stmt.expression))
		local bool = stmt.expression --[[@as Bool]]
		LU.assertEquals(bool.value, tt.expectedBoolean)
	end
end

function TestParser:testIfExpression()
	local input = "if (x < y) { x }"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()

	checkParserErrors(p)

	assert(program, "program is nil")
	LU.assertEquals(#program.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1] --[[@as ExpressionStatement]]
	LU.assertIsTrue(ast.IfExpression.isInstance(stmt.expression))
	local exp = stmt.expression --[[@as IfExpression]]
	testInfixExpression(exp.condition, "x", "<", "y")
	LU.assertEquals(#exp.consequence.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.consequence.statements[1]))
	local consequence = exp.consequence.statements[1] --[[@as ExpressionStatement]]
	testIdentifier(consequence.expression, "x")
	LU.assertIsNil(exp.alternative)
end

function TestParser:testIfElseExpression()
	local input = "if (x < y) { x } else { y }"

	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()

	checkParserErrors(p)

	assert(program)
	LU.assertEquals(#program.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1]  --[[@as ExpressionStatement]]
	LU.assertIsTrue(ast.IfExpression.isInstance(stmt.expression))
	local exp = stmt.expression  --[[@as IfExpression]]
	testInfixExpression(exp.condition, "x", "<", "y")
	LU.assertEquals(#exp.consequence.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.consequence.statements[1]))
	local consequence = exp.consequence.statements[1] --[[@as ExpressionStatement]]
	testIdentifier(consequence.expression, "x")
	LU.assertEquals(#exp.alternative.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.alternative.statements[1]))
	local alternative = exp.alternative.statements[1]  --[[@as ExpressionStatement]]
	testIdentifier(alternative.expression, "y")
end

function TestParser:testFunctionLiteralParsing()
	local input = "fn(x, y) { x + y; }"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)
	assert(program)

	LU.assertEquals(#program.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1]  --[[@as ExpressionStatement]]
	LU.assertIsTrue(ast.FunctionLiteral.isInstance(stmt.expression))
	local func = stmt.expression  --[[@as FunctionLiteral]]
	LU.assertEquals(#func.parameters, 2)
	testLiteralExpression(func.parameters[1], "x")
	testLiteralExpression(func.parameters[2], "y")
	LU.assertEquals(#func.body.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(func.body.statements[1]))
	local bodyStmt = func.body.statements[1]  --[[@as ExpressionStatement]]
	testInfixExpression(bodyStmt.expression, "x", "+", "y")
end

function TestParser:testFunctionParameterParsing()
	---@class TestCaseTestFunctionParameterParsing
	---@field input string
	---@field expectedParams string[]

	---@type TestCaseTestFunctionParameterParsing[]
	local tests = {
		{input = "fn() {};", expectedParams = {}},
		{input = "fn(x) {};", expectedParams = {"x"}},
		{input = "fn(x, y, z) {};", expectedParams = {"x", "y", "z"}},
	}

	for _, tt in ipairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		assert(program)

		LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
		local stmt = program.statements[1]  --[[@as ExpressionStatement]]
		LU.assertIsTrue(ast.FunctionLiteral.isInstance(stmt.expression))
		local func = stmt.expression  --[[@as FunctionLiteral]]
		LU.assertEquals(#func.parameters, #tt.expectedParams)
		for i, ident in ipairs(tt.expectedParams) do
			testLiteralExpression(func.parameters[i], ident)
		end
	end
end

function TestParser:testCallExpressionParsing()
	local input = "add(1, 2 * 3, 4 +5);"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	assert(program)
	LU.assertEquals(#program.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1]  --[[@as ExpressionStatement]]
	LU.assertIsTrue(ast.CallExpression.isInstance(stmt.expression))
	local exp = stmt.expression  --[[@as CallExpression]]
	testIdentifier(exp.func, "add")
	LU.assertEquals(#exp.arguments, 3)
	testLiteralExpression(exp.arguments[1], 1)
	testInfixExpression(exp.arguments[2], 2, "*", 3)
	testInfixExpression(exp.arguments[3], 4, "+", 5)
end
