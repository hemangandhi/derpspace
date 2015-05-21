#{name: (pos in string, # of categories, [names for each category])}

ALL_POSS_STATES = "ABCDEF"

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
    x_match = lambda entry: entry[survey[cell_x_var][0]] == ALL_POSS_STATES[cell_x]
    y_match = lambda entry: entry[survey[cell_y_var][0]] == ALL_POSS_STATES[cell_y]
    return lambda entry: x_match(entry) and y_match(entry)

def get_table(x_var, y_var, data):
    to_ret = [["Headers"] + survey[x_var][1] + ["Totals"]]
    for i in range(len(survey[y_var][1])):
        row_to_add = [survey[y_var][1][i]]
        for j in range(len(survey[x_var][1])):
            row_to_add += [len(list(filter(get_cell_checker(x_var,y_var,j,i),data)))]
        row_to_add += [sum(row_to_add[1:])]
        to_ret += [row_to_add]
    to_ret += [["Totals"] + [sum(to_ret[j][i] for j in range(1,len(to_ret))) for i in range(1,len(to_ret[0]))]]
    return to_ret
    
def get_expected_values(x_var, y_var, data):
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
    return (sum(((exp_mat[i][j] - obs_mat[i][j])**2)/exp_mat[i][j] for i in range(1, len(exp_mat) - 1) for j in range(1, len(exp_mat[0]))),
            df, obs_mat, exp_mat)


