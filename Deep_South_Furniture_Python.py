############### Problem Definition ###############
# Deep South Furniture has produced high quality
# diner tables and chairs since 1983. Rhett Butler, the
# new general manager who was recently hired, is
# considering re-designing the production flow and
# composition of the diner sets to increase profits
# and possible decrease inventory costs. Given the
# current resourcing, each department (assembly and
# shipping) has time constraints. The company has
# six assembly resources working up to 40 hrs per
# week (depending on the work load) getting paid $12
# per hour while shipping resources department has
# 4 resources working up to 40 hrs per week, getting
# paid $11 per hour. Mr. Butler gathered some data
# from the IT department and decided to hire an
# external consulting group to help him re-design the
# production and shipping flow.

##########################################################
# Product   # Assembly Time (hrs) # Shipping Time (mins) #
# Chair_603 # 3                   # 120                  #
# Table_752 # 5                   # 180                  #
##########################################################

######################
# Product   # Demand #
# Chair_603 # 360    #
# Table 752 # 80     #
######################

#############################
# Product   # Selling Price #
# Chair_603 # $160          #
# Table_752 # $480          #
#############################

############### Equations ###############
#   max 102(x1) + 387(x2)
#   s.t.  3(x1) + 5(x2) <= 240
#         2(x1) + 3(x2) <= 160
#         x1 <= 360
#         x2 <= 80
#         x1 >= 0
#         x2 >= 0


# %% Imports
from scipy.optimize import linprog


# %% Define our constraints and constants

# Since linprog always minimizes your target function, you must use the fact
# that max(f(x)) == -min(-f(x)) to maximize. Therefore, we use -102 and -387
# instead of 102 and 387.

c = [-102, -387]
A = [[3, 5], [2, 3]]
b = [240, 160]
x0_bounds = (0, 360)
x1_bounds = (0, 80)


# %% Maximize the funtion and print the results
result = linprog(c, A_ub=A, b_ub=b, bounds=(x0_bounds, x1_bounds), options={'disp': True})
print(result)


# %% Incorporate the 'at least 4 chairs per table' constraint

# Since scipy is uses upper bounds by default, we have to write x1 - 4(x2) >= 0
# as -x1 + 4(x2) < 0. Everything else is the same.

c = [-102, -387]
A = [[3, 5], [2, 3], [-1, 4]]
b = [240, 160, 0]
x0_bounds = (0, 360)
x1_bounds = (0, 80)


# %% Maximize the funtion and print the results
result = linprog(c, A_ub=A, b_ub=b, bounds=(x0_bounds, x1_bounds), options={'disp': True})
print(result)
