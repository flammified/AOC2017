num = "59734319985939030811765904366903137260910165905695158121249344919210773577393954674010919824826738360814888134986551286413123711859735220485817087501645023012862056770562086941211936950697030938202612254550462022980226861233574193029160694064215374466136221530381567459741646888344484734266467332251047728070024125520587386498883584434047046536404479146202115798487093358109344892308178339525320609279967726482426508894019310795012241745215724094733535028040247643657351828004785071021308564438115967543080568369816648970492598237916926533604385924158979160977915469240727071971448914826471542444436509363281495503481363933620112863817909354757361550"

num = num * 10000
def phase(signal, offset):
    # for i in range(offset, len(signal)):

    counter = 0
    for idx in range(len(signal) - 1, offset - 1, -1):
        counter += int(signal[idx])
        counter = counter % 10
        signal[idx] = str(counter)

    return signal


offset = int(num[0:7])
num = list(num)

for y in range(0, 100):
    num = phase(num, offset)

print("".join(num)[offset:offset+8])
