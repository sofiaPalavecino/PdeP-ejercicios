module Library where
import PdePreludat

esMultiploDe :: Number -> Number -> Bool
esMultiploDe divisor numero = mod numero divisor == 0

esMultiploDeTres :: Number -> Bool
esMultiploDeTres = esMultiploDe 3

cubo :: Number -> Number
cubo numero = numero * numero

areaRectangulo :: Number -> Number -> Number
areaRectangulo base altura = base * altura

esBisiesto :: Number -> Bool
esBisiesto anio
    | ( esMultiploDe 4 anio || esMultiploDe 400 anio ) && not(esMultiploDe 100 anio) = True
    | otherwise = False

celciusAFarh :: Number -> Number
celciusAFarh temperatura = (temperatura * 9/5) + 32

farhToCelcius :: Number -> Number
farhToCelcius temperatura = (temperatura - 32) * 5/9

haceFrioF :: Number -> Bool
haceFrioF temperatura = temperatura < celciusAFarh 8

mcm :: Number -> Number -> Number
mcm a b = div (a * b) (gcd a b)

-- Dispersión Río Paraná

maxNivel :: Number -> Number -> Number -> Number
maxNivel nivel1 nivel2 nivel3 = max nivel1 (max nivel2 nivel3)

minNivel :: Number -> Number -> Number -> Number
minNivel nivel1 nivel2 nivel3 = min nivel1 (min nivel2 nivel3)

dispersion :: Number -> Number -> Number -> Number
dispersion nivel1 nivel2 nivel3 = maxNivel nivel1 nivel2 nivel3 - minNivel nivel1 nivel2 nivel3

diasParejos :: Number -> Number -> Number -> Bool
diasParejos nivel1 nivel2 nivel3 = dispersion nivel1 nivel2 nivel3 < 30

diasLocos :: Number -> Number -> Number -> Bool
diasLocos nivel1 nivel2 nivel3 = dispersion nivel1 nivel2 nivel3 > 100

diasNormales :: Number -> Number -> Number -> Bool
diasNormales nivel1 nivel2 nivel3 = not(diasLocos nivel1 nivel2 nivel3) && not(diasParejos nivel1 nivel2 nivel3)

-- Plantación de pinos

