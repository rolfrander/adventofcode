from mip import Model, minimize, INTEGER, CBC

bus = [[17,0], [13,2], [19,3]]

def parse(input):
    data = input.split(",")
    return list(filter(lambda x: x[0] != -1, map(lambda id, i: [int(id), i] if id != "x" else [-1, -1], data, range(len(data)))))


def execute(input):
    bus = parse(input)
    m = Model("bus")
    x = [ m.add_var(var_type=INTEGER, lb=1, name="bus{}".format(i)) for i in range(len(bus))]
    time = m.add_var(var_type=INTEGER, lb=100000000000000, name="time")
    m.objective = minimize(time)

    for i in range(len(bus)):
        m += bus[i][0]*x[i]-time == bus[i][1]

    m.write("bus.lp")
    m.optimize(max_seconds=300)
    return time.x


execute("17,x,13,19")
execute("67,7,59,61")
execute("67,x,7,59,61")
execute("67,7,x,59,61")
execute("1789,37,47,1889")
execute("19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29")



for id,off in bus:
    print("{} {} {}".format(id, off, (19+off) % id))



