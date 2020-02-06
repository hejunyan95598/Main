#%% Ask for help
help(print)

#%% Line separation
# Same line
1+1; 2+2

# Separate lines
1+1
2+2

# %% Reference in python 
x = [1,2,3]
y = x # This does not create a new list, but rather assign y as a reference of x, both point to the same list
y[0] = 100
x # x also changes

# The following line can use explicit copy to solve the issue
x = [1,2,3]
y = list(x)
y [0] = 100
x # x does not change

#%% Multiplication, division, modulo, exponentiation and round
print(3 * 5)
print(10 / 2)
print(18 % 7) 
print(4 ** 2)
print(round(3.1415,2))
#%% Max,min,mean,median,standard deviation
TODO:
q = [1,2,3,4,5,6]
max(q)
min(q)
mean(q)
median(q)
stdev(q)

#%% How much is your $100 worth after 7 years?
print(100*1.1**7)

#%% Create a variable
x = 100
# Create a str variable
desc = "compound interest"
# Create a bool variable
profitable = True

# %%Type of variable
type(profitable)

# %% Convert variable type
a = str(profitable)
b = int(profitable)
c = float(profitable)
d = bool(profitable)

print(type(a))
print(type(b))
print(type(c))
print(type(d))

# %% Python List 
hall = 11.25
kit = 18.0
liv = 20.0
bed = 10.75
bath = 9.50

names = ["hallway", "kitchen", "living room", "bedroom", "bathroom"]    
areas = [hall, kit, liv, bed, bath]

print(names)
print(areas)

# %% List can contain different types

example = ["hallway", hall, "kitchen", kit, "living room", liv, "bedroom", bed, "bathroom", bath]
print(example)

# %% List within lsit
house = [["hallway", hall],
         ["kitchen", kit],
         ["living room", liv],
         ["bedroom", bed],
         ["bathroom", bath]]
    
# Print out house
print(house)

# Print out the type of house
print(type(house))

# %% Subsetting list
# First element of list has index of 0, first has index of 1...
# The last element of lust has index of -1
a = [1,2,3,4,5,6,7,8,9,10]
a[0]
a[1]
a[-1]

# [x : y] give the subset of the list where x is included and y is not
a[0:6] # returns the first 6 elements
a[6:10] # returns the last 4 elements

# if do not specify begin and end, list will start from the beginning/end at the end point
a[:6] # returns the first 6 elements
a[6:] # returns the last 4 elements

# if list_a is a list with list, list_a[x][y] will return the (y+1)th element in the (x+1)th list 
house
house[1][1]
house[-2][0] 

# %% Change value in list
a[0] = 100
a
a[0:3] = ["a","b","c","d"]
a

# %% Add/delete value to/from list
a = [1,2,3,4,5,6,7,8,9,10]
a + ["test num",123,321] # this WILL NOT modify the list
a
del(a[0:4]) # this WILL modify the list
a