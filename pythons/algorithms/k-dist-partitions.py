from itertools import permutations

def dedup(fn):
    def wrapper(ls, k):
        i = fn(ls, k)
        # i = list(i)
        rv = set(i)
        # print(len(i), len(rv))
        return rv
    return wrapper

#DOESN'T WORK AND I DON'T KNOW WHY
@dedup
def k_dist_perms_part(ls, k):
    def rec_helper(ls, k): #avoids deduping until the very end
        if len(ls) <= k:
            yield from permutations(ls)
        else:
            for i in range(1, k + 2):
                lhss = list(permutations(ls[:i]))
                for rhs in k_dist_perms_part(ls[i:], k):
                    yield from (lhs + rhs for lhs in lhss)
    return rec_helper(ls, k)

@dedup
def k_dist_perms_build(ls, k):
    def build_helper(i, built):
        if i == len(ls):
            yield built
        else:
            for j in range(max(0, i - k), min(len(ls), i + k + 1)):
                if j not in built:
                    cp = built.copy()
                    cp[j] = i
                    yield from build_helper(i + 1, cp)

    #conjecture: all k-distance permutations are the inverse of some k-distance permutation
    def built_to_perm(built):
        return tuple(ls[built[i]] for i in range(len(ls)))

    return map(built_to_perm, build_helper(0, {}))

def k_dist_perm_checker(init, k):
    def is_k_dist(perm):
        return all(p in perm[max(i - k, 0): min(i + k + 1, len(init))]\
                for i, p in enumerate(init))
    return is_k_dist

if __name__ == "__main__":
    test_list, test_k = list(range(6)), 4
    naive = set(filter(k_dist_perm_checker(test_list, test_k), permutations(test_list)))
    non_naive = k_dist_perms_part(test_list, test_k)
    print(naive - non_naive)
    print(non_naive == naive)
