import pandas as pd

myfile = open("input4.txt","r")

lines = []

for st in myfile:
    lines.append(st)

lines.sort()

gr = 0

events = []
sleeptimes = []
guards = []

for x in lines:
    st = x.split()[1][:-1]
    if x.split()[2] == "Guard":
        gr = int(x.split()[3][1:])
        if [gr,[0]*60] not in guards:
            guards.append([gr,[0]*60])
        continue
    ev = x.split()[2]
    hr = int(st[0:2])
    mn = int(st[3:5])
    events.append((gr,hr,mn,ev))
    
guards.sort()

for x in events:
    gr = x[0]
    if x[3] == 'falls':
        start = x[2]
    elif x[3] == 'wakes':
        end = x[2]
        #~ print(end-start)
        sleeptimes.append((gr,(end-start),start,end)) 
        
for x in sleeptimes:
    for m in range(60):
        if x[2] <= m:
            if x[3] >= m:
                #~ print("yay")
                indexes = [i for i,y in enumerate(guards) if y[0] == x[0]]
                #~ print(guards[indexes[0]])
                guards[indexes[0]][1][m] = guards[indexes[0]][1][m]+1
        
df = pd.DataFrame(sleeptimes)
df.columns = ['guard','minutes','start','end']
agg = df.groupby(['guard'])['minutes'].agg('sum').reset_index()
agg.columns = ['guard','tot']
agg = agg.sort_values(by=['tot'],ascending=False).reset_index()
dude = agg['guard'][0]

indexes = [i for i,x in enumerate(guards) if x[0] == dude]

slep = guards[indexes[0]][1]
print(dude*slep.index(max(slep)))

ng = []
ng1 = []

for x in guards:
    ng.append([x[0]]+x[1])
    ng1.append(x[1])

slepdf = pd.DataFrame(ng)


