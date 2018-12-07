myfile = open("input1.txt","r")
numbers = []

for line in myfile:
    numbers.append( int(line.replace("+","")))

print( sum( numbers) )

pastnumbers = []
s = 0
stopbool = True

while stopbool:
    for x in numbers:
        if s in pastnumbers: 
            print(s)
            stopbool = False
        pastnumbers.append(s)
        s = s + x