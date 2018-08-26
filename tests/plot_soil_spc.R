plot(soil.wav, soil.spc.prim.mean, type='l') # lowest
lines(soil.wav, soil.spc.oilpalm.mean, col='red')
lines(soil.wav, soil.spc.prim.min, col='green')
lines(soil.wav, soil.spc.prim.max, col='blue') # brightest
lines(soil.wav, soil.spc.OP.min, col='magenta')
lines(soil.wav, soil.spc.OP.max, col='cyan')