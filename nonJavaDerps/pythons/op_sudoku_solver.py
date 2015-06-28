from functools import reduce

apply_func = lambda func,args: reduce(func,args)

is_not_val_str = lambda x,mat: set(x) != set(i for i in range(1,len(mat) + 1))

def op_test_box(box, mat):
    if (len(box[2]) == 1 and box[0] == mat[box[2][0][1]][box[2][0][0]]):
        return True
    elif box[1] == int.__sub__ and box[0] == abs(apply_func(box[1],[mat[i[1]][i[0]] for i in box[2]])):
        return True
    elif box[1] == int.__truediv__ and apply_func(box[1],[mat[i[1]][i[0]] for i in box[2]]) in [box[0],1/box[0]]:
        return True
    else:
        return apply_func(box[1],[mat[i[1]][i[0]] for i in box[2]]) == box[0]

def fully_test_box(box, solve, dim):
    mat = [[0 for i in range(dim)] for j in range(dim)]
    for i in solve:
        mat[i[1]][i[0]] = solve[i]
    if not op_test_box(box, mat):
        return False
    
    for i in solve:
        for j in solve:
            if i == j:
                continue
            elif i[0] == j[0] and solve[i] == solve[j]:
                return False
            elif i[1] == j[1] and solve[i] == solve[j]:
                return False

    print("Box",box,"passed all tests with",solve)
    return True            
        
def test_sol(mat, boxes):
    if len(list(filter(lambda x: is_not_val_str(x,mat),mat))) != 0:
        return False
    elif sum(is_not_val_str([mat[i][j] for i in range(len(mat))],mat) for j in range(len(mat[0]))) != 0:
        return False
    else:
        return sum(not op_test_box(i,mat) for i in boxes) == 0

def sol(dim, boxes, temp_mat = None, rows = None, cols = None):
    if len(boxes) == 0:
        return temp_mat
    
    if rows == None:
        rows = [set(i for i in range(1, dim + 1)) for j in range(dim)]
    if cols == None:    
        cols = [set(i for i in range(1, dim + 1)) for j in range(dim)]
    if temp_mat == None:
        temp_mat = [[0 for i in range(dim)] for j in range(dim)]

    def get_poss(x,y):
        if temp_mat[y][x] != 0:
            return None
        else:
            return rows[y]&cols[x]

    def resolve_box(box):
        ind_to_vals = {(i[0],i[1]):list(get_poss(i[0],i[1])) for i in box[2]}
        if len(ind_to_vals) > 1:
            len_prod = apply_func(int.__mul__,map(len,ind_to_vals.values()))
        else:
            len_prod = len(list(ind_to_vals.values())[0])
        valid_sols = filter(lambda x: fully_test_box(box,x,dim),[{i:ind_to_vals[i][j%len(ind_to_vals[i])] for i in ind_to_vals} for j in range(len_prod)])
        to_ret = list(valid_sols)
        if len(to_ret) == 0:
            print("Unable to solve:",box, "Placements:",ind_to_vals)
            return None
        return to_ret    

    def place_box(box_sol, side_eff = True):
        if side_eff:
            print("Placing",box_sol
                  )
            for i in box_sol:
                temp_mat[i[1]][i[0]] = box_sol[i]
                rows[i[1]].remove(box_sol[i])
                cols[i[0]].remove(box_sol[i])

    min_box_ind = 0
    min_box_sol = None
    for i in range(len(boxes)):
        if min_box_sol == None:
            min_box_ind = i
            min_box_sol = resolve_box(boxes[i])
            continue
        sols = resolve_box(boxes[i])
        print("sols for",boxes[i],":",sols)
        if len(sols) < len(min_box_sol) and len(sols) > 0:
            min_box_sol = sols
            min_box_ind = i

#    print(min_box_ind)
    if len(min_box_sol) == 1:
        place_box(min_box_sol[0])
        sol(dim,boxes[:min_box_ind] + boxes[min_box_ind + 1:],temp_mat,rows,cols)
    else:
        print('Not yet impl')
        return 0

#test:
#sol(2,[[3,int.__add__,[(0,0),(1,0)]],[1,None,[(1,0)]],[2,None,[(1,1)]]])
