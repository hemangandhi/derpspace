from functools import wraps

def truncate_num_to(num, to):
    tos = int(num / to)
    if num < 0:
        tos -= 1
    return num - to * tos


def extend_sin(half_pi_ish):
    def extender(sine):
        @wraps(sine)
        def extended(x):
            in_period = truncate_num_to(x, 4 * half_pi_ish)
            negate = 1 if in_period < (2 * half_pi_ish) else -1
            in_half_period = truncate_num_to(x, 2 * half_pi_ish)
            flip_half_pi = in_half_period > half_pi_ish
            if flip_half_pi:
                value = sine(half_pi_ish - x)
            else:
                value = sine(x)
            return negate * value
        return extended
    return extender


def turtle_sine(resolution):
    f = [0]
    df = [1]
    while len(f) < 2 or f[-2] <= f[-1]:
        f.append(f[-1] + resolution * df[-1])
        df.append(df[-1] - resolution * f[-2])

    half_pi_ish = len(f) * resolution

    @extend_sin(half_pi_ish)
    def sine(x):
        return f[int(x / resolution)]
    return sine, f, df


def take(it, n):
    for i, v in zip(range(n), it):
        yield v


def taylor_sine(terms, pi_error):
    def coeffs():
        demon = 1
        ctr = 1
        while True:
            yield 1/demon
            ctr += 1
            demon *= ctr
            ctr += 1
            demon *= ctr

    def horner_eval(terms):
        poly = list(take(coeffs(), terms))[1:]

        def sine(x):
            total = 0
            x_sq = x ** 2
            for coeff in reversed(poly):
                total = x_sq * (total + coeff)
            return x + total
        return sine

    bad_sine = horner_eval(terms)

    def half_pi_within_error(error):
        low = 0
        high = 1.5
        ic = 0
        y_l = bad_sine(low)
        y_h = bad_sine(high)
        while abs(y_l - y_h) > error:
            print(low, high, y_l, y_h)
            new_x = (high - low) / (y_h - y_l) + low - (high - low)/(y_h - y_l) * y_l
            if abs(low - new_x) > abs(high - new_x):
                low = new_x
            else:
                high = new_x
            y_l = bad_sine(low)
            y_h = bad_sine(high)
            ic += 1
        print(low, high, y_l, y_h)
        y_l = bad_sine(low)
        y_h = bad_sine(high)
        return (high - low) / (y_h - y_l) + low - (high - low)/(y_h - y_l) * y_l, ic

    half_pi_ish, rounds = half_pi_within_error(pi_error)
    return extend_sin(half_pi_ish)(bad_sine), rounds, half_pi_ish


