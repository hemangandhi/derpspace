def fall(ticks, freq = 1, m = 1, rho = 1, A = 1, b = 1, v0 = 0, cyl_height = 10):
    """Ticks = # of clock ticks,
       Freq = frequency of ticks per second."""
    g = 9.8
    x0 = 0
    accel = lambda x,v: -1*(rho*min(x,cyl_height)*A*g + b*v + m*g)/m

    ret_obj = {'init':{'freq':freq,'m':m,'rho':rho,'A':A,'b':b,'v0':v0}}

    for i in range(ticks):
        del_v = accel(x0,v0)
        v0 += del_v/freq
        x0 += v0/freq

        ret_obj[i] = {'accel':del_v,'v':v0,'x':x0,'t':i/freq}

    return ret_obj

def run():
    data = fall(250000,10**4)
    with open('data.csv','a') as f:
        for i in data:
            if all(j in data[i] for j in ['t','x','v','accel']):
                f.write("\t".join(str(data[i][j]) for j in ['t','x','v','accel']) + '\n')


if __name__ == "__main__":
    run()
