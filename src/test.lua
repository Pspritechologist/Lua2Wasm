local msg = "Hello, World!"
print(msg)

local a = 50
local b = 25
local c = true

if c then
	msg = "Hello, Mars!"
else
	msg = "Hello, Venus!"
end

print(msg)

msg = false

if msg then
	msg = "First block"
elseif msg then
	msg = "Second block"
elseif msg then
	msg = "Third block"
else
	msg = "Fourth block"
end

print(msg)
