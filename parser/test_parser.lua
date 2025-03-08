local lexer = require("lexer.lexer")
local parser = require("parser.parser")
local ast = require("ast.ast")
local utils = require("utils")
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
---@param s ast.Statement
---@param name string
---@return boolean
local function testLetStatement(s, name)
	if s:tokenLiteral() ~= "let" then
		LU.fail("s.TokenLiteral not 'let'; got=" .. s:tokenLiteral())
		return false
	end
	local letStmt = s --[[@as ast.LetStatement]]
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
---@param il ast.Expression
---@param value integer
local function testIntegerLiteral(il, value)
	LU.assertTrue(ast.IntegerLiteral.isInstance(il))
	local integ = il --[[@as ast.IntegerLiteral]]
	LU.assertEquals(integ.value, value)
	LU.assertEquals(integ:tokenLiteral(), tostring(value))
end


---Helper function section 2.8
---@param exp ast.Expression
---@param value string
local function testIdentifier(exp, value)
	LU.assertIsTrue(ast.Identifier.isInstance(exp))
	local ident = exp --[[@as ast.Identifier]]
	LU.assertEquals(ident.value, value)
	LU.assertEquals(ident:tokenLiteral(), value)
end

---comment
---@param exp ast.Expression
---@param value boolean
local function testBooleanExpression(exp, value)
	LU.assertIsTrue(ast.Bool.isInstance(exp))
	local bo = exp --[[@as ast.Bool]]
	LU.assertEquals(bo.value, value)
	LU.assertEquals(bo:tokenLiteral(), tostring(value))
end

---Helper function section 2.8
---@param exp ast.Expression
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
---@param exp ast.Expression
---@param left any
---@param operator string
---@param right any
local function testInfixExpression(exp, left, operator, right)
	LU.assertTrue(ast.InfixExpression.isInstance(exp))
	local opExp = exp --[[@as ast.InfixExpression]]
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

		local returnStmt = program.statements[1] --[[@as ast.ReturnStatement]]
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
	assert(program)
	LU.assertEquals(#program.statements, 1)
	LU.assertTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertTrue(ast.Identifier.isInstance(stmt.expression))
	local exp = stmt.expression --[[@as ast.Identifier]]
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
	local literal = stmt.expression --[[@as ast.IntegerLiteral]]
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

		local exp = stmt.expression --[[@as ast.PrefixExpression]]
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
		local exp = stmt.expression --[[@as ast.InfixExpression ]]
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
		{
			"a * [1, 2, 3, 4][b * c] * d",
			"((a * ([1, 2, 3, 4][(b * c)])) * d)",
		},
		{
			"add(a * b[2], b[1], 2 * [1, 2][1])",
			"add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
		},
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
		local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
		LU.assertIsTrue(ast.Bool.isInstance(stmt.expression))
		local bool = stmt.expression --[[@as ast.Bool]]
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
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.IfExpression.isInstance(stmt.expression))
	local exp = stmt.expression --[[@as ast.IfExpression]]
	testInfixExpression(exp.condition, "x", "<", "y")
	LU.assertEquals(#exp.consequence.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.consequence.statements[1]))
	local consequence = exp.consequence.statements[1] --[[@as ast.ExpressionStatement]]
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
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.IfExpression.isInstance(stmt.expression))
	local exp = stmt.expression --[[@as ast.IfExpression]]
	testInfixExpression(exp.condition, "x", "<", "y")
	LU.assertEquals(#exp.consequence.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.consequence.statements[1]))
	local consequence = exp.consequence.statements[1] --[[@as ast.ExpressionStatement]]
	testIdentifier(consequence.expression, "x")
	LU.assertEquals(#exp.alternative.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(exp.alternative.statements[1]))
	local alternative = exp.alternative.statements[1] --[[@as ast.ExpressionStatement]]
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
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.FunctionLiteral.isInstance(stmt.expression))
	local func = stmt.expression --[[@as ast.FunctionLiteral]]
	LU.assertEquals(#func.parameters, 2)
	testLiteralExpression(func.parameters[1], "x")
	testLiteralExpression(func.parameters[2], "y")
	LU.assertEquals(#func.body.statements, 1)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(func.body.statements[1]))
	local bodyStmt = func.body.statements[1] --[[@as ast.ExpressionStatement]]
	testInfixExpression(bodyStmt.expression, "x", "+", "y")
end

function TestParser:testFunctionParameterParsing()
	---@class TestCaseTestFunctionParameterParsing
	---@field input string
	---@field expectedParams string[]

	---@type TestCaseTestFunctionParameterParsing[]
	local tests = {
		{ input = "fn() {};",        expectedParams = {} },
		{ input = "fn(x) {};",       expectedParams = { "x" } },
		{ input = "fn(x, y, z) {};", expectedParams = { "x", "y", "z" } },
	}

	for _, tt in ipairs(tests) do
		local l = lexer.Lexer:new(tt.input)
		local p = parser.Parser:new(l)
		local program = p:parseProgram()
		checkParserErrors(p)

		assert(program)

		LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
		local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
		LU.assertIsTrue(ast.FunctionLiteral.isInstance(stmt.expression))
		local func = stmt.expression --[[@as ast.FunctionLiteral]]
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
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.CallExpression.isInstance(stmt.expression))
	local exp = stmt.expression --[[@as ast.CallExpression]]
	testIdentifier(exp.func, "add")
	LU.assertEquals(#exp.arguments, 3)
	testLiteralExpression(exp.arguments[1], 1)
	testInfixExpression(exp.arguments[2], 2, "*", 3)
	testInfixExpression(exp.arguments[3], 4, "+", 5)
end

function TestParser:testStringLiteralExpression()
	local input = [["hello world";]]

	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)
	assert(program)

	LU.assertTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertTrue(ast.StringLiteral.isInstance(stmt.expression))
	local literal = stmt.expression --[[@as ast.StringLiteral]]
	LU.assertEquals(literal.value, "hello world")
end

function TestParser:testParsingArrayLiterals()
	local input = "[1, 2 * 2, 3 + 3]"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	assert(program)
	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.ArrayLiteral.isInstance(stmt.expression))
	local array = stmt.expression --[[@as ast.ArrayLiteral]]
	LU.assertEquals(#array.elements, 3)
	testIntegerLiteral(array.elements[1], 1)
	testInfixExpression(array.elements[2], 2, "*", 2)
	testInfixExpression(array.elements[3], 3, "+", 3)
end

function TestParser:testParsingIndexExpressions()
	local input = "myArray[1 + 1]"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	LU.assertIsTrue(ast.ExpressionStatement.isInstance(program.statements[1]))
	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.IndexExpression.isInstance(stmt.expression))
	local indexExp = stmt.expression --[[@as ast.IndexExpression]]
	testIdentifier(indexExp.left, "myArray")
	testInfixExpression(indexExp.index, 1, "+", 1)
end

function TestParser:testParsingHashLiteralsStringKeys()
	local input = [[{"one": 1, "two": 2, "three": 3}]]
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.HashLiteral.isInstance(stmt.expression))
	local hash = stmt.expression --[[@as ast.HashLiteral]]
	LU.assertEquals(#utils.tableKeys(hash.pairs), 3)

	---@type {[string]: integer}
	local expected = {
		one = 1,
		two = 2,
		three = 3,
	}

	for k, v in pairs(hash.pairs) do
		LU.assertIsTrue(ast.StringLiteral.isInstance(k))
		local literal = k --[[@as string]]
		local expectedValue = expected[tostring(literal)]
		testIntegerLiteral(v, expectedValue)
	end
end

function TestParser:testParsingEmptyHashLiteral()
	local input = "{}"
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.HashLiteral.isInstance(stmt.expression))
	local hash = stmt.expression --[[@as ast.HashLiteral]]
	LU.assertEquals(#utils.tableKeys(hash.pairs), 0)
end

function TestParser:testParsingHashLiteralsWithExpressions()
	local input = [[{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}]]
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	checkParserErrors(p)

	local stmt = program.statements[1] --[[@as ast.ExpressionStatement]]
	LU.assertIsTrue(ast.HashLiteral.isInstance(stmt.expression))
	local hash = stmt.expression  --[[@as ast.HashLiteral]]
	LU.assertEquals(#utils.tableKeys(hash.pairs), 3)

	---@type {[string]: fun(e: ast.Expression)}
	local tests = {
		one = function(e)
			testInfixExpression(e, 0, "+", 1)
		end,
		two = function(e)
			testInfixExpression(e, 10, "-", 8)
		end,
		three = function(e)
			testInfixExpression(e, 15, "/", 5)
		end,
	}

	for k, v in pairs(hash.pairs) do
		LU.assertIsTrue(ast.StringLiteral.isInstance(k))
		local literal = k  --[[@as ast.StringLiteral]]
		local testFunc = tests[tostring(literal)]
		LU.assertNotIsNil(testFunc, string.format("No test function for key %q found", tostring(literal)))
		testFunc(v)
	end
end
