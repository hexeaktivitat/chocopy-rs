def is_zero(items: [int], idx: int) -> bool:
    val:int = 0 # Type is explicitly declared
    val = items[idx]
    return val == 0

mylist: [int] = None
mylist = [1,0,1]
print(is_zero(mylist, 1)) # Prints 'True'
