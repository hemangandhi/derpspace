from itertools import permutations

def k_dist_perms(ls, k):
    if len(ls) <= k:
        yield from permutations(ls)
    else:
        for i in range(1, k + 1):
            lhss = list(permutations(ls[:i]))
            for rhs in k_dist_perms(ls[i:], k):
                yield from (lhs + rhs for lhs in lhss)

k_dist_perms_dedup = lambda ls, k: set(k_dist_perms(ls, k))

def k_dist_perm_checker(init, k):
    def is_k_dist(perm):
        return all(p in init[i - k: i + k + 1] for i, p in enumerate(perm))

if __name__ == "__main__":
    test_list, test_k = [1, 2, 3], 2
    naive = list(filter(k_dist_perm_checker(test_list, test_k), permutations(test_list)))
    print(naive)
    non_naive = list(k_dist_perms_dedup(test_list, test_k))
    print(non_naive)
    print(non_naive == naive)
