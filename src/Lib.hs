{-
Nombre: Quispe Choque, Erik
Legajo: 1722876

-}
module Lib where
import Text.Show.Functions

laVerdad = True


{-
Completá tus datos al inicio del archivo y hacé el primer commit & push.
Toda la solución del parcial debe estar en este archivo.
Recordá ir haciendo commit & push a medida que resuelvas el parcial.
-}
data Turista = UnTurista {
    nivelCansancio :: Int,
    nivelStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show)

--Islas
type Excursion = Turista->Turista

irAlaPlaya::Excursion
irAlaPlaya turista 
    |viajaSolo turista =modificarCansancio (+5) turista
    |(not.viajaSolo) turista = reducirStress (1) turista

apreciarAlgunElemento::String->Excursion
apreciarAlgunElemento paisaje  = reducirStress (length paisaje) 

salirConGenteQueHabla::String->Excursion
salirConGenteQueHabla idioma turista = turista {idiomas = idioma:idiomas turista,viajaSolo = True}

modificarStress::(Int->Int)->Turista->Turista
modificarStress funcion turista =turista{nivelStress =(funcion .nivelStress) turista }

reducirStress::Int->Turista->Turista
reducirStress valor turista =turista{nivelStress =nivelStress turista - valor}

modificarCansancio::(Int->Int)->Turista->Turista
modificarCansancio funcion turista =turista {nivelCansancio = (funcion.nivelCansancio) turista}

caminar :: Int->Excursion
caminar minutos turista = modificarStress (+ (nivelIntesidad minutos)) . modificarCansancio (+(nivelIntesidad minutos)) $ turista

nivelIntesidad::Int -> Int
nivelIntesidad minutos = minutos `div` 4

data Marea = Fuerte | Moderada | Tranquila deriving Show

paseoBarco:: Marea -> Excursion
paseoBarco Tranquila turista = (modificarStress (+6) . modificarCansancio (+10)) turista
paseoBarco Moderada turista =turista
paseoBarco Fuerte turista = caminar (10) .apreciarAlgunElemento "algo" .salirConGenteQueHabla ("aleman") $ turista

ana = UnTurista 0 21 False ["español"]
beto = UnTurista 15 15 True ["aleman"]
cathi = UnTurista 15 15 True ["aleman","catalan"]

--punto 2

obtenerPorcentaje::Int->Int->Int
obtenerPorcentaje porcentaje valor = valor * porcentaje `div` 100

hacerExcursion :: Turista ->Excursion->Turista
hacerExcursion turista excursion = modificarStress (+(obtenerPorcentaje 10 (nivelStress turista))) . excursion $ turista
--parte b
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista ->Int 

deltaExcursionSegun :: Indice->Turista->Excursion->Int
deltaExcursionSegun  indice turista excursion = deltaSegun indice (excursion turista) (turista)

--cantIdiomas ::Turista->Int
--cantIdiomas = length . idiomas

--esEducativa::Turista->Excursion->Bool
--esEducativa turista excursion = deltaExcursionSegun (cantIdiomas) turista excursion

excursionesDesestresantes::Turista->[Excursion]->[Excursion]
excursionesDesestresantes turista excursiones = filter (esDesestresante turista) excursiones

esDesestresante::Turista->Excursion->Bool
esDesestresante turista excursion = (deltaExcursionSegun nivelStress turista excursion) <= 3
--punto 3
type Tour = [Excursion]
completo::Tour
completo =[caminar 20,apreciarAlgunElemento "cascada",caminar 40,salirConGenteQueHabla "malmequiano"]
ladoB::Excursion->Tour
ladoB escursion=[paseoBarco Tranquila,escursion,caminar 120]

islaVecina::Marea->Tour

islaVecina Fuerte =[paseoBarco Fuerte,apreciarAlgunElemento "lago",paseoBarco Fuerte]
islaVecina maerea =[paseoBarco maerea,irAlaPlaya,paseoBarco maerea]
--parte a
transformarTour::Tour->Excursion
transformarTour tour = foldl1 (.) tour

hacerTour::Turista->Tour->Turista
hacerTour turista tour =transformarTour tour $ (pagar turista tour) 

pagar::Turista->Tour ->Turista
pagar turista tour = modificarStress (+ (length tour)) turista
--parte b






--punto 4
tourInfinito :: Excursion -> Tour
tourInfinito excursion = excursion : tourInfinito excursion
--parte a
vistaPlayasInfinitas= tourInfinito irAlaPlaya

--parte b
