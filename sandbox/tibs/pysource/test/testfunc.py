# Stuff needed to make the examples compile...
class Fred:
    def __getitem__(self,index):
        pass
a = Fred()
b = [1,2,3,4]
d = Fred()

def func1(a,b=1,c='jim',d=None,e=[],f={'a':1,a:1},g=(1,2,3)):
    pass

def func2(a,*args,**kargs):
    pass

def func2a(a,*args):
    pass

def func2b(a,**kargs):
    pass

def func3((a,b,c),d):
    pass

def func4(a=a,b=Fred,c=Fred()):
    pass

def func5(a=[x for x in [1,2,3] if x > 2],
          b=[(x,y) for x in [1,2,3] for y in [3,4,5]]):
    pass

def func6(a=b[1:2],c=d[1,4:6,...,8:9:10],
          e=lambda x: x*2,
          f=lambda x,a=a: func5(x,a)):
    pass
