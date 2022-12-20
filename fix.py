with open('restaurants1.csv', 'w') as f:
    with open('restaurants.csv', 'r') as x:
        lines = x.readlines()
        for line in lines:
            l = line.replace(" , ", ", ")
            f.write(l)