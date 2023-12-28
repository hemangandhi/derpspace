
def load_file(file):
    clouds = set()
    n = 0
    m = 0
    with open(file) as f:
        fst = True
        for i, line in enumerate(f):
            if fst:
                fst = False
                continue
            for j, c in enumerate(line):
                if c == 'C':
                    clouds.add((i - 1, j))
                m = max(m, len(line.strip()))
            n = i
    return n, m, clouds

def nbr_coords(i, j, n, m):
    for ii in range(-1, 2):
        for jj in range(-1, 2):
            yield ((i + ii) % n, (j + jj) % m)

def is_cloud(i, j, n, m, clouds):
    return len(list(filter(lambda c: c in clouds, nbr_coords(i, j, n, m)))) % 2 == 1

def update_clouds(n, m, clouds):
    return {(ni, nj) for i, j in clouds for ni, nj in nbr_coords(i, j, n, m) if is_cloud(ni, nj, n, m, clouds)}

def print_clouds(n, m, c):
    print('\n'.join(''.join('C' if (i, j) in c else ' ' for j in range(m)) for i in range(n)))

def interact():
    path = input(' gimme file > ')
    n, m, c = load_file(path)
    cs = [c]
    ns = dict()
    go = True
    gen = 1
    c = update_clouds(n, m, c)
    ns[len(c)] = ns.get(len(c), []) + [gen]
    try:
        while c not in cs:
            gen += 1
            c = update_clouds(n, m, c)
            ns[len(c)] = ns.get(len(c), []) + [gen]
        print(f'{gen} repeated with {cs.index(c)}')
        print(ns)
    except:
        print(f'you gave up at {gen}')
        print(ns)

if __name__ == "__main__":
    interact()
