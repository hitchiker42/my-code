from random import randint
f=lambda :randint(1,6)
p=set()
for i in range(0,100000):
    x=(f(),f(),f(),f(),f())
    p.add(x)
total=0
lt=0
for i in p:
    if sum(i)<12:
        lt+=1
    total+=1
print("total:"+str(total))
print("lt:"+str(lt))
print("ratio:"+str(lt/total))
