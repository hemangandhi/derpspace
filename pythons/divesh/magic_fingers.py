#! /usr/bin/env python3

def prompt_start():
	names = {}
	n = input("Enter a name to add a player or just press enter to start! > ")
	while len(n) > 0:
		st = len(names)
		names[n] = [1,1]
		if len(names) == st:
			print("This player already exists! Enter another name!")
		n = input("Enter a name to add a player or just press enter to start! > ")
	return names
	
def prompt_turn(name, state):
	src = input("Pick whether to use you left hand (l) or right (r) hand > ")
	while src not in ['l','r']:
		src = input("Pick whether to use you left hand (l) or right (r) hand > ")	
	src = src != 'l'
	if state[name][src] == 0:
		print("Cannot play a 0!")
		prompt_turn(name, state)
		return
	print("The current players are :")
	print(list(state.keys()))
	dest = input("Pick a player (you can pick yourself) > ")
	while dest not in state:
		dest = input("Pick a player (you can pick yourself) > ")
	if dest == name:
		if state[name][not src] == 0:
			if state[name][src] % 2 == 0:
				state[name][src] /= 2
				state[name][not src] = state[name][src]
			else:
				print("Illegal move! Try again!")
				prompt_turn(name, state)
		else:
			state[name][not src] += state[name][src]
			state[name][not src] %= 5
	else:
		h = input("Pick a hand to play to (l or r) > ")
		while h not in ['l','r']:
			h = input("Pick a hand to play to (l or r) > ")
		state[dest][h != 'l'] += state[name][src]
		state[dest][h != 'l'] %= 5
	if state[dest][0] == 0 and state[dest][1] == 0:
		print(dest,"lost!")
		del state[dest]

def print_state(name, state):
	for i in sorted(state):
		if i == name:
			print(">",i,"Left:",state[i][0],"Right:",state[i][1])
		else:
			print(" ",i,"Left:",state[i][0],"Right:",state[i][1])

def main():
	p = prompt_start()
	while len(p) > 1:
		for i in sorted(p):
			print("The current state is:")
			print_state(i, p)
			prompt_turn(i, p)
			if len(p) <= 1:
				break

if __name__ == "__main__":
	main()
