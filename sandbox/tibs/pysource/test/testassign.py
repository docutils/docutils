class Jim:

    def __init__(self):
        a = 1
        a = [2,3]
        a,b = 4,5
        a,b = [6,7]
        a,self.a = 8,9
        a = b = self.a = 10
        a,b = c,d = 11,12
        self.a,self.b = 13,14
        self.a = self.b = 15
