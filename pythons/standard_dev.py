def mean(data):
    return sum(data)/len(data)

def std_dev(data):
    x_bar = mean(data)
    return (sum([(x_bar - i)**2 for i in data])/(len(data) - 1))**.5

def std_dev_consec(low, high):
    sum_xi_sq = high*(high + 1)*(2*high + 1)/6
    sum_xi_sq = sum_xi_sq - low*(low + 1)*(2*low + 1)/6
    sum_xi = (high**2 + high - low**2 - low)/2
    middle = -2* sum_xi * (low + high)/2
    right =  (high - low + 1)*(low + high)**2 / 4
    variance = (sum_xi_sq + middle + right)/(high - low)
    return variance**.5
