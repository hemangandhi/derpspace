
def mk_inner_fn(dict_for_fn, array_of_fns, my_idx):
    def fn(st):
        if len(st) == 0:
            return my_idx
        elif st[0] not in dict_for_fn:
            return fn(st[1:])
        else:
            return array_of_fns[0][dict_for_fn[st[0]]](st[1:])
    return fn

def mk_fsm(fsm_dict, init_st, fin_sts):
    nd_to_num = {i[1]: i[0] for i in enumerate(fsm_dict.keys())}
    od = {i : {j : nd_to_num[fsm_dict[i][j]] for j in fsm_dict[i]} for i in fsm_dict}
    arr = [[]]
    kys = sorted(od.keys(), key = lambda x: nd_to_num[x])
    arr[0] = list(map(lambda x: mk_inner_fn(od[x], arr, nd_to_num[x]), kys))
    def fsm(st):
        return arr[0][nd_to_num[init_st]](st) in {nd_to_num[i] for i in fin_sts}
    return fsm


