# All ops are case-insensitive.
# Basic math and logical ops available (add, sub, eq, etc.).
# Create values with LoadTab(dst), LoadNum(dst, literal), LoadStr(dst, str-index), LoadNull(dst), LoadBool(dst, bool-literal)
# The str-index '0' is always an empty string.
# Values are stored in stack slots (registers). Max 256 slots. Slot-count is auto-calculated from usage.
# JumpIf and JumpIfNot skip the *next* instruction if the condition is met.
# The next instruction would typically be a GoTo op to create if/loop structures.
# All numbers are floating point.

loadnum 0 0
loadnum 1 1
loadnum 2 10000000.0

sub 2 2 1
lte 3 2 0
jmpifnot 3
goto -4

# After all ops use the -DATA- header to begin listing string literals. One per line.
-DATA-
Hello, World!
