factorial = lambda x: 1 if x == 0 else x*factorial(x-1)

def string_permute(string):
	if len(string) == 1:
		yield string
	else:
		yields = []
		for i in range(len(string)):
			for j in string_permute(string[:i] + string[i+1:]):
				if yields.count(j) == 0:
					yields += [j]
					yield j
				if yields.count(string[i] + j) == 0:
					yields += [string[i] + j]
					yield string[i] + j
					
def num_perms(string):
	toRet = 0
	for i in range(len(string)+1):
		toRet = toRet + factorial(len(string))/factorial(i)
	return toRet - 1					

toPerm = input("type in a string to permute: ")
print("There are " + str(num_perms(toPerm)) + ' permutations of the string...')
for i in string_permute(toPerm):
	print(i)
	toQuit = input("Type in quit to quit> ") == "quit"
	if toQuit:
		break
else:
	print("You made it!")
			
input('Goodbye > ')		