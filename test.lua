-- Comment out `local` to see globals working instead of upvalues :)
local number = 0

function add_one(x)
	number = number + 1
	return x + number
end
