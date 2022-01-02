###########
# Question 15.2

# In the videos, we saw the “diet problem”. (The diet problem is one of the first large-scale optimization
# problems to be studied in practice. Back in the 1930’s and 40’s, the Army wanted to meet the nutritional
# requirements of its soldiers while minimizing the cost.) In this homework you get to solve a diet problem with real data.
# The data is given in the file diet.xls.

# 1.	Formulate an optimization model (a linear program) to find the cheapest diet that satisfies the maximum and minimum daily nutrition constraints,
# and solve it using PuLP.  Turn in your code and the solution. (The optimal solution should be a diet of air-popped popcorn, poached eggs, oranges,
# raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)
# 2.	Please add to your model the following constraints (which might require adding more variables) and solve the new model:
# a.	If a food is selected, then a minimum of 1/10 serving must be chosen. (Hint: now you will need two variables for each food i: whether it is chosen,
# and how much is part of the diet. You’ll also need to write a constraint to link them.)
# b.	Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
# c.	To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected. [If something is ambiguous (e.g., should bean-and-bacon soup be considered meat?),
# just call it whatever you think is appropriate – I want you to learn how to write this type of constraint, but I don’t really care whether we agree on how to classify foods!]

# If you want to see what a more full-sized problem would look like, try solving your models for the file diet_large.xls, which is a low-cholesterol diet model (rather than minimizing cost,
# the goal is to minimize cholesterol intake).  I don’t know anyone who’d want to eat this diet – the optimal solution includes dried chrysanthemum garland, raw beluga whale flipper,
# freeze-dried parsley, etc. – which shows why it’s necessary to add additional constraints beyond the basic ones we saw in the video!
#	[Note: there are many optimal solutions, all with zero cholesterol, so you might get a different one.  It probably won’t be much more appetizing than mine.]

###########


####### Homework 11 Question 2 using PuLP ######

# ---------- Import modules -----------

# import PuLP and pandas modules

from pulp import *
import pandas as pd

# ------------ Read data ---------------

data = pd.read_excel("diet.xls", header=0)  # read all data

dataTable = data[0:64]  # These are datarows, last two are min and max values
dataTable = dataTable.values.tolist()  # Convert dataframe to list

nutrientNames = list(data.columns.values)  # column headers
#print(nutrientNames)

# create master foods dictionary
foods = [x[0] for x in data]
#print('##FOODS##',foods)
calories = dict([(x[0], float(x[3])) for x in dataTable])
#print('##CALORIES ###',calories)
cholesterol = dict([(x[0], float(x[4])) for x in dataTable])
totalFat = dict([(x[0], float(x[5])) for x in dataTable])
sodium = dict([(x[0], float(x[6])) for x in dataTable])
carbs = dict([(x[0], float(x[7])) for x in dataTable])
fiber = dict([(x[0], float(x[8])) for x in dataTable])
protien = dict([(x[0], float(x[9])) for x in dataTable])
vitaminA = dict([(x[0], float(x[10])) for x in dataTable])
vitaminC = dict([(x[0], float(x[11])) for x in dataTable])
calcium = dict([(x[0], float(x[12])) for x in dataTable])
iron = dict([(x[0], float(x[13])) for x in dataTable])

#minVal = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
#maxVal = [2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]

#Dynamically read min and max values
minVal = data[65:66].values.tolist()  # minimum nutrient values
maxVal = data[66:67].values.tolist()  # maximum nutrient values

# ------------ Extract individual vectors of data ------------
#
# python "dict" structure to dynamically create nutrients and cost for each food
# which is more efficient

foods = [j[0] for j in dataTable]  # list of food names

cost = dict([(j[0], float(j[1])) for j in dataTable])  # cost for each food

nutrients = []
for i in range(0, 11):  # for loop running through each nutrient: 11 times starting with 0
    nutrients.append(dict([(j[0], float(j[i + 3])) for j in dataTable]))  # amount of nutrient i in food j

# ------------ Create a new LP Problem ------------
#
# This problem is a minimization problem (find the *lowest* cost), so "LpMinimize" is the second parameter.

prob = LpProblem('Foodoptimization', LpMinimize)  # 2 parameters: "name" and "sense"

# ------------ Define the variables ---------------
#
# One variable (we chose the name "foodVars") for each food.
# Lower limit of each variable is 0, since we can't eat negative amounts of anything.

foodVars = LpVariable.dicts("Foods", foods, 0)
foodVars_selected = LpVariable.dicts("food_select", foods, 0, 1,
                                     LpBinary)  # Create binary integer variables for whether a food is eaten

# ------------ Create objective function ------------
#
# Note that the first function we add is taken to be the objective function

prob += lpSum([cost[f] * foodVars[f] for f in foods]), 'TotalCost'

# ------------ Add constraints for each nutrient ------------

for i in range(0, 11):  # for loop running through each nutrient: 11 times starting with 0
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) >= minVal[0][i + 3], 'minnutrient ' + nutrientNames[
        i]
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) <= maxVal[0][i + 3], 'maxnutrient ' + nutrientNames[
        i]

# ------------ Adding additional constraints ------------

# CONSTRAINT A

# If a food is eaten, must eat at least 0.1 serving

for food in foods:
    prob += foodVars[food] >= 0.1 * foodVars_selected[food]

# If any of a food is eaten, its binary variable must be 1

for food in foods:
    prob += foodVars_selected[food] >= foodVars[food] * 0.0000001

# CONSTRAINT B

# Include at most 1 of celery and frozen brocolli

prob += foodVars_selected['Frozen Broccoli'] + foodVars_selected['Celery, Raw'] <= 1

# CONSTRAINT C

# At least 3 kinds of meat/poultry/fish/eggs

prob += foodVars_selected['Roasted Chicken'] + foodVars_selected['Poached Eggs'] \
        + foodVars_selected['Scrambled Eggs'] + foodVars_selected['Bologna,Turkey'] \
        + foodVars_selected['Frankfurter, Beef'] + foodVars_selected['Ham,Sliced,Extralean'] \
        + foodVars_selected['Kielbasa,Prk'] + foodVars_selected['Pizza W/Pepperoni'] \
        + foodVars_selected['Hamburger W/Toppings'] \
        + foodVars_selected['Hotdog, Plain'] + foodVars_selected['Pork'] \
        + foodVars_selected['Sardines in Oil'] + foodVars_selected['White Tuna in Water'] \
        + foodVars_selected['Chicknoodl Soup'] + foodVars_selected['Splt Pea&Hamsoup'] \
        + foodVars_selected['Vegetbeef Soup'] + foodVars_selected['Neweng Clamchwd'] \
        + foodVars_selected['New E Clamchwd,W/Mlk'] + foodVars_selected['Beanbacn Soup,W/Watr'] >= 3

# ------------ Solve the optimization problem ------------

prob.solve()

# ------------ Print the output in a readable format -----------

print()
print("---------The solution to the diet problem is----------")
for var in prob.variables():
    if var.varValue > 0 and "food_select" not in var.name:  # Print non binary variables
        print(str(var.varValue) + " units of " + str(var).replace('Foods_', ''))
print()
print("Total cost of food = $%.2f" % value(prob.objective))