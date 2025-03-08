local lexer = require("lexer.lexer")
local parser = require("parser.parser")
local object = require("object.object")
local evaluator = require("evaluator.evaluator")
local environment = require("object.environment")
LU = require("luaunit")

TestEvaluator = {}

---testEval
---@param input string
---@return object.Object
local function testEval(input)
	local l = lexer.Lexer:new(input)
	local p = parser.Parser:new(l)
	local program = p:parseProgram()
	local env = environment.Environment:new()
	return evaluator.eval(program, env)
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
	if not object.Integer.isInstance(obj) then
		print(LU.prettystr(obj))
	end
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
		{ "return 10;",         10 },
		{ "return 10; 9;",      10 },
		{ "return 2 * 5; 9;",   10 },
		{ "9;return 2 * 5; 9;", 10 },
		{ [[
			if (10 > 1) {
				if (10 > 1) {
					return 10;
				}
				return 1;
			}
		]], 10 },
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

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		testIntegerObject(evaluated, tt.expected)
	end
end

function TestEvaluator:testErrorHandling()
	---@class TestCaseTestErrorHandling
	---@field input string
	---@field expectedMessage string

	local testsArr = {
		{
			"5 + true;",
			"type mismatch: INTEGER + BOOLEAN",
		},
		{
			"5 + true; 5;",
			"type mismatch: INTEGER + BOOLEAN",
		},
		{
			"-true",
			"unknown operator: -BOOLEAN",
		},
		{
			"true + false;",
			"unknown operator: BOOLEAN + BOOLEAN",
		},
		{
			"5; true + false; 5",
			"unknown operator: BOOLEAN + BOOLEAN",
		},
		{
			"if (10 > 1) { true + false; }",
			"unknown operator: BOOLEAN + BOOLEAN",
		},
		{
			[[
			if (10 > 1) {
				if (10 > 1) {
					return true + false;
				}
				return 1
			}
			]],
			"unknown operator: BOOLEAN + BOOLEAN",
		},
		{
			"foobar",
			"identifier not found: foobar",
		},
		{
			[["Hello" - "World"]],
			"unknown operator: STRING - STRING",
		},
		{
			'{"name": "Monkey"}[fn(x) { x }];',
			"unusable as hash key: FUNCTION",
		},
	}

	---@type TestCaseTestErrorHandling[]
	local tests = {}
	for _, tc in ipairs(testsArr) do
		---@type TestCaseTestErrorHandling
		local test = {
			input = tc[1],
			expectedMessage = tc[2],
		}
		table.insert(tests, test)
	end

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		LU.assertIsTrue(object.Error.isInstance(evaluated))
		local errObj = evaluated --[[@as object.Error]]
		LU.assertEquals(errObj.message, tt.expectedMessage)
	end
end

function TestEvaluator:testLetStatements()
	---@class TestEvaluatorTestLetStatements
	---@field input string
	---@field expected integer

	local testsArr = {
		{ "let a = 5; a;",                               5 },
		{ "let a = 5 * 5; a;",                           25 },
		{ "let a = 5; let b = a; b;",                    5 },
		{ "let a = 5; let b = a; let c = a + b + 5; c;", 15 },
	}

	---@type TestEvaluatorTestLetStatements[]
	local tests = {}
	for _, tc in ipairs(testsArr) do
		---@type TestEvaluatorTestLetStatements
		local tcase = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, tcase)
	end

	for _, tt in ipairs(tests) do
		testIntegerObject(testEval(tt.input), tt.expected)
	end
end

function TestEvaluator:testFunctionObject()
	local input = "fn(x) { x + 2; };"
	local evaluated = testEval(input)
	LU.assertIsTrue(object.Func.isInstance(evaluated))
	local fn = evaluated --[[@as object.Func]]
	LU.assertEquals(#fn.parameters, 1)
	LU.assertEquals(tostring(fn.parameters[1]), "x")
	local expectedBody = "(x + 2)"
	LU.assertEquals(tostring(fn.body), expectedBody)
end

function TestEvaluator:testFunctionApplication()
	---@class TestCaseTestEvaluatorTestFunctionApplication
	---@field input string
	---@field expected integer
	local testArr = {
		{ "let identity = fn(x) { x; }; identity(5);",             5 },
		{ "let identity = fn(x) { return x; }; identity(5);",      5 },
		{ "let double = fn(x) { x * 2; }; double (5);",            10 },
		{ "let add = fn(x, y) { x + y; }; add(5, 5);",             10 },
		{ "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20 },
		{ "fn(x) { x; }(5)",                                       5 },
	}

	---@type TestCaseTestEvaluatorTestFunctionApplication[]
	local tests = {}
	for _, tc in ipairs(testArr) do
		---@type TestCaseTestEvaluatorTestFunctionApplication
		local case = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, case)
	end

	for _, tt in ipairs(tests) do
		testIntegerObject(testEval(tt.input), tt.expected)
	end
end

function TestEvaluator:testClosures()
	local input = [[
		let newAdder = fn(x) {
			fn(y) { x + y };
		};

		let addTwo = newAdder(2);
		addTwo(2);
	]]
	testIntegerObject(testEval(input), 4)
end

function TestEvaluator:testStringLiteral()
	local input = [["Hello World!"]]
	local evaluated = testEval(input)
	LU.assertTrue(object.String.isInstance(evaluated))
	local str = evaluated --[[@as object.String]]
	LU.assertEquals(str.value, "Hello World!")
end

function TestEvaluator:testStringConcatenation()
	local input = [["Hello" + " " + "World!"]]
	local evaluated = testEval(input)
	LU.assertIsTrue(object.String.isInstance(evaluated))
	local str = evaluated --[[@as object.String]]
	LU.assertEquals(str.value, "Hello World!")
end

function TestEvaluator:testBuiltinFunctions()
	---@class TestCaseTestEvaluatorTestBuiltinFunctions
	---@field input string
	---@field expected any

	local testsArr = {
		{ [[len("")]],            0 },
		{ [[len("four")]],        4 },
		{ [[len("hello world")]], 11 },
		{ [[len(1)]],             "argument to `len` not supported, got INTEGER" },
		{ [[len("one","two")]],   "wrong number of arguments. got=2, want=1" },
	}

	---@type TestCaseTestEvaluatorTestBuiltinFunctions[]
	local tests = {}
	for _, tc in ipairs(testsArr) do
		---@type TestCaseTestEvaluatorTestBuiltinFunctions
		local testCase = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, testCase)
	end

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		if type(tt.expected) == "number" then
			testIntegerObject(evaluated, math.floor(tt.expected))
		elseif type(tt.expected) == "string" then
			LU.assertIsTrue(object.Error.isInstance(evaluated))
			local errObj = evaluated --[[@as object.Error]]
			LU.assertEquals(errObj.message, tt.expected)
		end
	end
end

function TestEvaluator:testArrayLiterals()
	local input = "[1, 2 * 2, 3 + 3]"
	local evaluated = testEval(input)
	LU.assertIsTrue(object.Array.isInstance(evaluated))
	local result = evaluated --[[@as object.Array]]
	LU.assertEquals(#result.elements, 3)
	testIntegerObject(result.elements[1], 1)
	testIntegerObject(result.elements[2], 4)
	testIntegerObject(result.elements[3], 6)
end

function TestEvaluator:testArrayIndexExpressions()
	---@class TestCaseTestEvaluatorTestArrayIndexExpressions
	---@field input string
	---@field expected integer?

	---@type table<string, integer?>
	local testArr = {
		{ "[1, 2, 3][0]",                                                   1 },
		{ "[1, 2, 3][1]",                                                   2 },
		{ "[1, 2, 3][2]",                                                   3 },
		{ "let i = 0; [1][i];",                                             1 },
		{ "[1, 2, 3][1 + 1];",                                              3 },
		{ "let myArray = [1, 2, 3]; myArray[2];",                           3 },
		{ "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6 },
		{ "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",        2 },
		{ "[1, 2, 3][3]",                                                   nil },
		{ "[1, 2, 3][-1]",                                                  nil },
	}

	---@type TestCaseTestEvaluatorTestArrayIndexExpressions[]
	local tests = {}
	for _, tc in ipairs(testArr) do
		---@type TestCaseTestEvaluatorTestArrayIndexExpressions
		local test = {
			input = tc[1],
			expected = tc[2],
		}
		table.insert(tests, test)
	end

	for _, tt in ipairs(tests) do
		local evaluated = testEval(tt.input)
		local integer = tt.expected
		if integer ~= nil then
			testIntegerObject(evaluated, math.floor(integer))
		else
			testNullObject(evaluated)
		end
	end
end

function TestEvaluator:testHashLiterals()
	local input = [[
		let two = "two";
		{
			"one": 10 - 9,
			two: 1 + 1,
			"thr" + "ee": 6 / 2,
			4: 4,
			true: 5,
			false: 6,
		}
	]]
	local evaluated = testEval(input)
	LU.assertIsTrue(object.Hash.isInstance(evaluated))
	local result = evaluated --[[@as object.Hash]]
	---@type {[string]: integer}
	local expected = {
		[object.String:new { value = "one" }:hashKey():toString()] = 1,
		[object.String:new { value = "two" }:hashKey():toString()] = 2,
		[object.String:new { value = "three" }:hashKey():toString()] = 3,
		[object.Integer:new { value = 4 }:hashKey():toString()] = 4,
		[evaluator.TRUE:hashKey():toString()] = 5,
		[evaluator.FALSE:hashKey():toString()] = 6,
	}
	LU.assertEquals(#result.pairs, #expected)
	for expectedKey, expectedValue in pairs(expected) do
		local pr = result.pairs[expectedKey]
		LU.assertNotIsNil(pr)
		testIntegerObject(pr.value, expectedValue)
	end
end

function TestEvaluator:testHashIndexExpressions()
	---@class TestCaseTestEvaluatorTestHashIndexExpressions
	---@field input string
	---@field expected any
	
	---@type {[string]: any}
	local testsArr = {
		['{"foo": 5}["foo"]'] = 5,
		['{"foo": 5}["bar"]'] = nil,
		['let key = "foo"; {"foo": 5}[key]'] = 5,
		['{}["foo"]'] = nil,
		['{5: 5}[5]'] = 5,
		['{true: 5}[true]'] = 5,
		['{false: 5}[false]'] = 5,
	}
	---@type TestCaseTestEvaluatorTestHashIndexExpressions[]
	local tests = {}

	for k, v in pairs(testsArr) do
		---@type TestCaseTestEvaluatorTestHashIndexExpressions
		local tc = {
			input = k,
			expected = v,
		}
		table.insert(tests, tc)
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
