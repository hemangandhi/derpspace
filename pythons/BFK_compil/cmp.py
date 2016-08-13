mem = dict()
mem_ptr = 0        mem[mem_ptr] = input()        while mem_ptr in mem and mem_ptr[mem] != 0:                mem_ptr += 1                if mem_ptr in mem:
                        mem[mem_ptr] += 1
                else:
                        mem[mem_ptr] = 1                print(mem[mem_ptr] if mem_ptr in mem else 0)                mem_ptr -= 1                if mem_ptr in mem:
                        mem[mem_ptr] -= 1
                else:
                        mem[mem_ptr] = -1                