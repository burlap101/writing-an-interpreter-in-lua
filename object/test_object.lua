local object = require("object.object")
LU = require("luaunit")

TestObject = {}

function TestObject:testStringHashKey()
	local hello1 = object.String:new { value = "Hello World" }
	local hello2 = object.String:new { value = "Hello World" }
	local diff1 = object.String:new { value = "My name is johnny" }
	local diff2 = object.String:new { value = "My name is johnny" }

	local sameContentErrMsg = "strings with same content have different hash keys"
	LU.assertEquals(hello1:hashKey(), hello2:hashKey(), sameContentErrMsg)
	LU.assertEquals(diff1:hashKey(), diff2:hashKey(), sameContentErrMsg)
	LU.assertNotEquals(hello1:hashKey(), diff1:hashKey(), "strings with different content have same hash keys")
end
