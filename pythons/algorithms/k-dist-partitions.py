from itertools import permutations

def k_dist_perms(ls, k):
    if len(ls) <= k:
        yield from permutations(ls)
    else:
        for i in range(k + 1):
            lhss = list(permutations(ls[:i]))
            for rhs in k_dist_perms(ls[i:], k):
                yield from (lhs + rhs for lhs in lhss)

def k_dist_perm_checker(init, k):
    def is_k_dist(perm):
        return all(p in init[i - k: i + k + 1] for i, p in enumerate(perm))

if __name__ == "__main__":
    test_list, test_k = [], 0
    naive = list(filter(k_dist_perm_checker(test_list, test_k), permutations(test_list)))
    print(list(k_dist_perms(test_list, test_k)) == naive)
