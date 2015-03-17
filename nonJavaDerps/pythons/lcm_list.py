def lcm(a):
    """Get the lcm of a list of integers. No zeroes or negatives."""
    if len(a) == 0:
        return 0
    big = max(a)
    count = big
    while len([i for i in a if count % i != 0]) != 0:
        count = count + big
    return count

def get_int_list(quit_cond,prompt):
    """
    Gets a list of integers from the user. Quits when quit_cond (assumed to be
    a 1-arg predicate - gives the user input as a string arg) is True.

    The string prompt is displayed at any time - even on illegal input.
    
    Returns the inputted list.
    """
    nums = []
    toQuit = False
    while not toQuit:
        line = input(prompt)
        while not line.isdigit() and not quit_cond(line):
            line = input("That was not a number, " + prompt)
        if not quit_cond(line):
            nums.append(int(line))
        else:
            break
    return nums

if __name__=="__main__":
    print("Type in a list of numbers to find the lcm of.")
    print("Input q to finish inputting the list")
    print(str(lcm(get_int_list(lambda x: x=="q", "Enter the number or enter q > ")))
          + " is the lcm.")
