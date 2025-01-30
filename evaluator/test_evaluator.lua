local lexer = require("lexer.lexer")
local parser = require("parser.parser")
local object = require("object.object")
local evaluator = require("evaluator.evaluator")
LU = require("luaunit")

TestEvaluator = {}

---testEval
---@param input string
---@return object.Object
local function testEval(input)
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	return evaluator.eval(program)
end

---testNullObject
---@param obj object.Object
local function testNullObject(obj)
	LU.assertEquals(obj, evaluator.NULL)
end

---testIntegerObject
---@param obj object.Object
---@param expected integer
local function testIntegerObject(obj, expected)
	LU.assertIsTrue(object.Integer.isInstance(obj))
	local result = obj --[[@as object.Integer]]
	LU.assertEquals(result.value, expected)
end

---Tests a boolean object
---@param obj object.Object
---@param expected boolean
local function testBooleanObject(obj, expected)
	LU.assertIsTrue(object.Bool.isInstance(obj))
	local result = obj --[[@as object.Bool]]
	LU.assertEquals(result.value, expected)
end

function TestEvaluator:testEvalIntegerExpression()
	---@class TestCaseTestEvalIntegerExpression
	---@field input string
	---@field expected integer

	---@type TestCaseTestEvalIntegerExpression[]
	local tests = {
		{ input = "5",                   expected = 5 },
		{ input = "10",                  expected = 10 },
		{ input = "-5",                  expected = -5 },
		{ input = "-10",                 expected = -10 },
		{ input = "5+5+5+5-10",          expected = 10 },
		{ input = "2*2*2*2*2",           expected = 32 },
		{ input = "-50+100+-50",         expected = 0 },
		{ input = "5*2+10",              expected = 20 },
		{ input = "5+2*10",              expected = 25 },
		{ input = "20+2*-10",            expected = 0 },
		{ input = "50/2*2+10",           expected = 60 },
		{ input = "2*(5+10)",            expected = 30 },
		{ input = "3*3*3+10",            expected = 37 },
		{ input = "3*(3*3)+10",          expected = 37 },
		{ input = "(5+10*2+15/3)*2+-10", expected = 50 },
	}

	for _, tt in pairs(tests) do
		local evaluated = testEval(tt.input)
		testIntegerObject(evaluated, tt.expected)
	end
end

function TestEvaluator:testEvalBooleanExpression()
	---@class TestCaseTestEvalBooleanExpression
	---@field input string
	---@field expected boolean

	---@type TestCaseTestEvalBooleanExpression[]
	local tests = {
		{ input = "true",   expected = true },
		{ input = "false",  expected = false },
		{ input = "1 < 2",  expected = true },
		{ input = "1 > 2",  expected = false },
		{ input = "1 < 1",  expected = false },
		{ input = "1 > 1",  expected = false },
		{ input = "1 == 1", expected = true },
		{ input = "1 != 1", expected = false },
		{ input = "1 == 2", expected = false },
		{ input = "1 != 2", expected = true },
	}

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		testBooleanObject(evaluated, tt.expected)
	end
end

function TestEvaluator:testBangOperator()
	---@class TestCaseTestBangOperator
	---@field input string
	---@field expected boolean

	---@type TestCaseTestBangOperator[]
	local tests = {
		{ input = "!true",   expected = false },
		{ input = "!false",  expected = true },
		{ input = "!5",      expected = false },
		{ input = "!!true",  expected = true },
		{ input = "!!false", expected = false },
		{ input = "!!5",     expected = true },
	}

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		testBooleanObject(evaluated, tt.expected)
	end
end

function TestEvaluator:testIfElseExpression()
	---@class TestCaseTestIfElseExpression
	---@field input string
	---@field expected any

	local testsArr = {
		{ "if (true) { 10 }",              10 },
		{ "if (false) { 10 }",             nil },
		{ "if (1) { 10 }",                 10 },
		{ "if (1 < 2) { 10 }",             10 },
		{ "if (1 > 2) { 10 }",             nil },
		{ "if (1 > 2) { 10 } else { 20 }", 20 },
		{ "if (1 < 2) { 10 } else { 20 }", 10 },
	}
	---@type TestCaseTestIfElseExpression[]
	local tests = {}

	for _, tc in ipairs(testsArr) do
		---@type TestCaseTestIfElseExpression
		local tcase = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, tcase)
	end

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		if type(tt.expected) == "number" then
			testIntegerObject(evaluated, tt.expected)
		else
			testNullObject(evaluated)
		end
	end
end

function TestEvaluator:testReturnStatements()
	---@class TestCaseTestEvaluatorReturnStatements
	---@field input string
	---@field expected integer

	local testsArr = {
		{"return 10;", 10},
		{"return 10; 9;", 10},
		{"return 2 * 5; 9;", 10},
		{"9;return 2 * 5; 9;", 10},
		{[[
			if (10 > 1) {
				if (10 > 1) {
					return 10;
				}
				return 1;
			}
		]], 10},
	}
	---@type TestCaseTestEvaluatorReturnStatements[]
	local tests = {}

	for _, tc in ipairs(testsArr) do
		---@type TestCaseTestEvaluatorReturnStatements
		local testCase = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, testCase)
	end

	for	_, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		testIntegerObject(evaluated, tt.expected)
	end
end
