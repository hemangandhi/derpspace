
def mk_inner_fn(dict_for_fn, array_of_fns, my_idx):
    def fn(st):
        #if the string is empty, return the state number
        if len(st) == 0:
            return my_idx
        #if there's no specification, loop back to this state on the next token
        elif st[0] not in dict_for_fn:
            return fn(st[1:])
        #otherwise, transition as required.
        else:
            return array_of_fns[0][dict_for_fn[st[0]]](st[1:])
    return fn

def mk_fsm(fsm_dict, init_st, fin_sts):
    """
    The fsm_dict is expected to be of type {node_type: {edge_type: node_type}}.
    The init_st is expected to be of node_type.
    fin_sts is expected also to be of the node type.
    """
    #map nodes to numbers
    nd_to_num = {i[1]: i[0] for i in enumerate(fsm_dict.keys())}
    #map all the transitions to their node number
    od = {i : {j : nd_to_num[fsm_dict[i][j]] for j in fsm_dict[i]} for i in fsm_dict}
    arr = [[]]
    #sort the transitions
    kys = sorted(od.keys(), key = lambda x: nd_to_num[x])
    #put in all the inner functions in order.
    arr[0] = list(map(lambda x: mk_inner_fn(od[x], arr, nd_to_num[x]), kys))
    def fsm(st):
        return arr[0][nd_to_num[init_st]](st) in {nd_to_num[i] for i in fin_sts}
    return fsm


