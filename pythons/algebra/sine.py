
def truncate_num_to(num, to):
    tos = int(num / to)
    return num - to * tos

def turtle_sine(resolution):
    f = [0]
    df = [1]
    while len(f) < 2 or f[-2] <= f[-1]:
        f.append(f[-1] + resolution * df[-1])
        df.append(df[-1] - resolution * f[-2])
    half_pi_ish = len(f) * resolution
    def sine(x):
        in_period = truncate_num_to(x, 4 * half_pi_ish)
        negate = 1 if in_period < (2 * half_pi_ish) else -1
        in_half_period = truncate_num_to(x, 2 * half_pi_ish)
        flip_half_pi = in_half_period > half_pi_ish
        if flip_half_pi:
            res_index = int((half_pi_ish - in_half_period) / resolution)
        else:
            res_index = int(in_half_period / resolution)
        return negate * f[res_index]
    return sine, f, df

