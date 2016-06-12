#algorithm thanks to Divesh. Mirrors clojure/online-problems/src/online-problems/sphere.clj. (You can compare them line-by-line.)

def distance(p1, p2):
    return sum(map(lambda x, y: (x - y) ** 2, p1, p2)) ** 0.5

def sq_to_pt_dist(pt, sq1, sq2):
    def is_in(x, y, z):
        return z in [x, y] or \
                (x - z < 0) != (y - z < 0)
    ins = map(is_in, sq1, sq2, pt)

    def to_pt(x, y, z, i):
        if i:
            return 0
        else:
            return min(abs(x - z), abs(y - z))

    return distance(map(to_pt, sq1, sq2, pt, ins), (0 for i in pt))

def sq_sph_intersect(pt, rad, sq1, sq2):
    return sq_to_pt_dist(pt, sq1, sq2) <= rad
