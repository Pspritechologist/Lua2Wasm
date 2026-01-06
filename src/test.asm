# All ops are case-insensitive.
# Basic math and logical ops available (add, sub, eq, etc.).
# Create values with LoadTab(dst), LoadNum(dst, literal), LoadStr(dst, str-index), LoadNull(dst), LoadBool(dst, bool-literal)
# The str-index '0' is always an empty string.
# Values are stored in stack slots (registers). Max 256 slots. Slot-count is auto-calculated from usage.
# SkpIf and SkpIfNot skip the next instruction if the condition is met.
# GoTo unconditionally jumps to a relative instruction offset and can be negative.
# The next instruction would typically be a GoTo op to create if/loop structures.
# All numbers are floating point.
# Tables can be interacted with via Set(tab, key, val) and Get(dst, tab, key).
# Tables do not have array-portions and do not treat numbers specially.

loadstr 0 0
put 0

loadnum 1 50
loadnum 2 25
gte 3 1 2

skpif 3 # If
goto 3 # Then
loadstr 0 1
goto 2 # Else
loadstr 0 2

put 0

loadbool 0 false

skpif 0 # If
goto 3 # Then
loadstr 0 4
goto 10 # Else
skpif 0 # If
goto 3 # Then
loadstr 0 5
goto 6 # Else
skpif 0 # If
goto 3 # Then
loadstr 0 6
goto 2 # Else
loadstr 0 7

put 0

-DATA-
Hello, World!
Hello, Mars!
Hello, Venus!

First block
Second block
Third block
Fourth block
