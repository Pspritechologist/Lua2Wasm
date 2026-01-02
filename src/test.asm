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

# Table demonstration - showcasing table operations and nested structures
# Slots: 0=main_table, 1=person_table, 2=address_table, 3=scores_table
#        4-7=string keys, 8-12=values, 13=temp storage, 14=retrieved values

# Create the main table and nested tables
loadtab 0          # main_table = {}
loadtab 1          # person_table = {}
loadtab 2          # address_table = {}
loadtab 3          # scores_table = {}

# Load string keys for various operations
loadstr 4 1        # "name"
loadstr 5 2        # "age" 
loadstr 6 3        # "address"
loadstr 7 4        # "scores"
loadstr 8 5        # "street"
loadstr 9 6        # "city"
loadstr 10 7       # "math"
loadstr 11 8       # "science"
loadstr 12 9       # "english"

# Create person data
loadstr 13 10      # "Alice"
set 1 4 13         # person["name"] = "Alice"
loadnum 13 25      # age = 25
set 1 5 13         # person["age"] = 25

# Create address data  
loadstr 13 11      # "123 Main St"
set 2 8 13         # address["street"] = "123 Main St"
loadstr 13 12      # "Springfield"
set 2 9 13         # address["city"] = "Springfield"

# Create scores data
loadnum 13 95.5    # math score
set 3 10 13        # scores["math"] = 95.5
loadnum 13 87.2    # science score
set 3 11 13        # scores["science"] = 87.2  
loadnum 13 92.0    # english score
set 3 12 13        # scores["english"] = 92.0

# Link nested tables to person
set 1 6 2          # person["address"] = address_table
set 1 7 3          # person["scores"] = scores_table

# Store person in main table
set 0 4 1          # main["name"] = person_table (using "name" as key for demo)

# Demonstrate retrieval operations
# Get the person table back
copy 14 4          # key = "name"  
get 14 0           # retrieve person from main table using key in slot 14
copy 15 14         # store retrieved person table in slot 15

# Get nested address from person
copy 14 6          # key = "address"
get 14 15          # get address table from person
copy 16 14         # store address table in slot 16

# Get city from address
copy 14 9          # key = "city"
get 14 16          # get city from address table
# Result: slot 14 now contains "Springfield"

# Get scores table and calculate average
copy 17 7          # key = "scores"
get 17 15          # get scores table from person
copy 18 17         # store scores table in slot 18

# Get individual scores
copy 19 10         # key = "math"
get 19 18          # get math score 
copy 20 19         # store math score (95.5)

copy 19 11         # key = "science" 
get 19 18          # get science score
copy 21 19         # store science score (87.2)

copy 19 12         # key = "english"
get 19 18          # get english score  
copy 22 19         # store english score (92.0)

# Calculate average score: (math + science + english) / 3
add 23 20 21       # temp = math + science
add 24 23 22       # temp2 = temp + english
loadnum 25 3       # divisor = 3
div 26 24 25       # average = total / 3

# Store average back in scores table
loadstr 27 13      # "average"  
set 18 27 26       # scores["average"] = calculated_average

# Demonstrate boolean and null storage
loadstr 28 14      # "is_active"
loadbool 29 true   # active = true
set 1 28 29        # person["is_active"] = true

loadstr 30 15      # "middle_name"
loadnull 31        # null value
set 1 30 31        # person["middle_name"] = null

# Final verification - get the average we calculated
copy 32 13         # key = "average"
get 32 18          # retrieve average from scores table
# Result: slot 32 now contains the calculated average (≈91.57)

# After all ops use the -DATA- header to begin listing string literals. One per line.
-DATA-
name
age
address
scores
street
city
math
science
english
Alice
123 Main St
Springfield
average
is_active
middle_name
