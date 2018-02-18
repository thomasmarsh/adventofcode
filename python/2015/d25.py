target = (3029, 2947)

z = 20151125

x = 1
y = 1

while True:
    if y == 1:
        y = x + 1
        x = 1
    else:
        y -= 1
        x += 1
    z = (z * 252533) % 33554393

    if (x, y) == target:
        print(z)
        break
