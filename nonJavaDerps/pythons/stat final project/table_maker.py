#all the possible flags
ALL_POSS_STATES = "ABCDEF"

#df = index + 1, alpha = .05
chi_sq_values = [3.84,5.99,7.81,9.49,11.07,12.59,14.07,15.51,16.92,18.31,19.68,21.03,
                 22.36,23.68,25,26.3,27.59,28.87,30.14,31.41,32.67,33.92,35.17,36.42,37.65]

#stores variables by variable name. Value gives index in the string and then labels for each
#state, in order.
survey = {"Gender":(0,["Female","Male"]),
          "Grade level":(1,["9","10","11","12"]),
          "Ethnicity":(2,["East Asian","South Asian","Caucasian","African","Hispanic","Other"]),
          "Cut in line":(3,["Yes","no"]),
          "Line choice":(4,["Fried Chicken","Pasta","Pizza","Hot Lunch", "Deli", "Baegel"]),
          "Spend amount":(5,["0-2","2-4","4-6","6-8","8-10"]),
          "Has lab day":(6,["1st half","2nd half","none","both"]),
          "Grade of lab day":(7,["9","10","11","12"]),
          "Cut in line on lab day":(8,["Yes","no"]),
          "Lab day line choice":(9,["Fried Chicken","Pasta","Pizza","Hot Lunch", "Deli", "Baegel"]),
          "Lab day spending":(10,["0-2","2-4","4-6","6-8","8-10"])}

def get_cell_checker(cell_x_var,cell_y_var,cell_x,cell_y):
    """
    Gets whether or not a survey response string is applicable for a given cell.
    """
    x_match = lambda entry: entry[survey[cell_x_var][0]] == ALL_POSS_STATES[cell_x]
    y_match = lambda entry: entry[survey[cell_y_var][0]] == ALL_POSS_STATES[cell_y]
    return lambda entry: x_match(entry) and y_match(entry)

def get_table(x_var, y_var, data):
    """
    Generates the table of observed values for x and y given data.
    """
    to_ret = [["Headers"] + survey[x_var][1] + ["Totals"]]
    for i in range(len(survey[y_var][1])):
        row_to_add = [survey[y_var][1][i]]
        for j in range(len(survey[x_var][1])):
            row_to_add += [len(list(filter(get_cell_checker(x_var,y_var,j,i),data)))]
        row_to_add += [sum(row_to_add[1:])]
        to_ret += [row_to_add]
    to_ret += [["Totals"] + [sum(to_ret[j][i] for j in range(1,len(to_ret))) for i in range(1,len(to_ret[0]))]]
    #Eliminate columns or rows of 0's from the matrix.
    for i in range(len(to_ret) - 2, 0, -1):
        if to_ret[i][len(to_ret[0]) - 1] == 0:
            to_ret = to_ret[:i] + to_ret[i + 1:]
    for i in range(len(to_ret[0]) - 2, 0, -1):
        if to_ret[len(to_ret) - 1][i] == 0:
            to_ret = [[to_ret[j][k] for k in range(len(to_ret[0])) if k != i] for j in range(len(to_ret))]
    return to_ret
    
def get_expected_values(x_var, y_var, data):
    """
    Gets the table of expected values for two variables and the data.
    """
    full = get_table(x_var, y_var, data)
    y_sums = full[len(full) - 1][1:len(full[0]) - 1]
    x_sums = [full[i][len(full[0]) - 1] for i in range(1,len(full) - 1)]
    body = [[j*i/(full[len(full) - 1][len(full[0]) - 1]) for j in y_sums] for i in x_sums]
    to_ret = [full[0]]
    for i in range(1,len(full) - 1):
        to_ret += [[full[i][0]] + body[i - 1] + [full[i][len(full[0]) - 1]]]
    to_ret += [full[len(full) - 1]]
    return to_ret

def get_chi_square(x_var,y_var,data):
    """
    Gets the chi-squared statistic, degrees of freedom and observed and expected matrices in that order.
    """
    obs_mat = get_table(x_var,y_var,data)
    exp_mat = get_expected_values(x_var,y_var,data)
    df = (len(exp_mat) - 3) * (len(exp_mat[0]) - 3)
    return (sum(((exp_mat[i][j] - obs_mat[i][j])**2)/exp_mat[i][j] for i in range(1, len(exp_mat) - 1) for j in range(1, len(exp_mat[0]) - 1)),
            df, obs_mat, exp_mat)

def conditions_met(exp_mat):
    """
    Returns whether the conditions for inference are met
    and a user-friendly string on the conditions.
    """
    exp_mat = [[i for i in j[1:len(j) - 1]] for j in exp_mat[1:len(exp_mat) - 1]]
    if [len(list(filter(lambda x: x < 1,i))) for i in exp_mat].count(0) < len(exp_mat):
        return False,"All expected counts are not more than or equal to 1."
    if sum(len(list(filter(lambda x: x < 5,i))) for i in exp_mat) > .2*len(exp_mat)*len(exp_mat[0]):
        return False,"More than 20% of expecteds are less than 5."
    return True, "All expected counts are more than 1 and less than 20% are less than 5."

def print_mat(mat):
    """
    Prints the contents of a matrix.
    """
    for i in mat:
        line = ""
        for j in i:
            line += str(j) + "\t"
        print(line)    

def conduct_test(x_var, y_var, data):
    """
    Prints out the test for an A from Robles...
    Returns whether the null was rejected.
    """
    test = get_chi_square(x_var, y_var, data)
    print("H0: there is no association between " + x_var + " and " + y_var + ".")
    print("Ha: there is an association between " + x_var + " and " + y_var + ".")
    conds = conditions_met(test[3])
    if conds[0]:
        print(conds[1])
    else:
        print(conds[1] + " We must proceed with caution.")
    print("Observed data:")
    print_mat(test[2])
    print("Expected data:")
    print_mat(test[3])
    print("Chi squared test statistic: " + str(test[0]))
    print("Degrees of freedom: " + str(test[1]))
    reject_null = test[0] > chi_sq_values[test[1] - 1]
    if reject_null:
        print("At alpha = .05, we reject H0 in favor of Ha.")
    else:
        print("At alpha = .05, we fail to reject H0.")
    return reject_null

def get_key(index):
    """
    Gets the key within the survey dictionary that would be at the index in a
    response string.
    """
    for k in survey:
        if survey[k][0] == index:
            return k
    return None    

def load_file(path):
    """
    Loads a file.
    Returns the errors in the file as
    a dictionary with line numbers pointing to the line
    and then the data as a list of strings (excluding invalids).
    """
    errs = {}
    data = []
    with open(path) as txt:
        line_no = 0
        for line in txt:
            line = line.upper()
            for i in range(len(line) - 1):
                key = get_key(i)
                if key == None:
                    errs[line_no] = line
                    break
                elif line[i] not in ALL_POSS_STATES[:len(survey[key][1])]:
                    errs[line_no] = line
                    break
            else:
                if len(line) >= len(survey):
                    data += [line]
                else:
                    errs[line_no] = line
            line_no += 1
    return errs,data

def get_var():
    """
    Prompts the user for a variable choice.
    """
    x_var = "opt"
    while x_var not in survey:
        if x_var == "opt":
            print("These are the variables you can select from: ")
            for k in survey:
                print(k)
        x_var = input("Please select a variable or enter opt for the list of options. ")
    return x_var    

def exec_main():
    """
    Handles all interaction with user,
    loading the file and conducting all desired tests.
    """
    path = input("Type in the file to load: ")
    errs,data = load_file(path)
    print("The following lines in the file have errors:")
    print(errs)
    
    to_cont = True
    while to_cont:
        print("You will now be prompted to chose two variables to test for association.")
        x_var = get_var()
        print("Please select the next variable...")
        y_var = get_var()
        print("Here are the test results: ")
        conduct_test(x_var, y_var, data)
        to_cont = input("Type in yes to conduct more tests: ") == "yes"

if __name__ == "__main__":
    exec_main()
        
        
