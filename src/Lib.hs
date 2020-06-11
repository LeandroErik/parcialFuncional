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
--punto 1
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
    |viajaSolo turista =reducirCansancio 5 turista
    |(not.viajaSolo) turista = reducirStress 1 turista

apreciarAlgunElemento::String->Excursion
apreciarAlgunElemento paisaje  = reducirStress (length paisaje) 

salirConGenteQueHabla::String->Excursion
salirConGenteQueHabla idioma turista = turista {idiomas = idioma:idiomas turista,viajaSolo = True}

{-
lo habia pensado con una funcion asi
modificarStress::(Int->Int)->Turista->Turista
modificarStress funcion turista =turista{nivelStress =(funcion .nivelStress) turista }
lo mismo con cansansio pero a la hora de restar no me funcionaba por eso creo nuevas funciones
esta solo funcionaba para sumar

-}
--type Indice = Turista ->Int 

reducirCansancio::Int->Turista->Turista
reducirCansancio valor turista =turista{nivelCansancio =nivelCansancio turista - valor}

reducirStress::Int->Turista->Turista
reducirStress valor turista =turista{nivelStress =nivelStress turista - valor}

modificarCansancio::(Int->Int)->Turista->Turista
modificarCansancio funcion turista =turista {nivelCansancio = (funcion.nivelCansancio) turista}

modificarStress::(Int->Int)->Turista->Turista
modificarStress funcion turista =turista {nivelStress = (funcion.nivelStress) turista}


caminar :: Int->Excursion
caminar minutos turista = reducirStress (nivelIntesidad minutos) . modificarCansancio (+(nivelIntesidad minutos)) $ turista


nivelIntesidad::Int -> Int
nivelIntesidad minutos = minutos `div` 4


data Marea = Fuerte | Moderada | Tranquila deriving Show


paseoBarco:: Marea -> Excursion
paseoBarco Tranquila turista = (modificarStress (+6) . modificarCansancio (+10)) turista
paseoBarco Moderada turista =turista
paseoBarco Fuerte turista = caminar 10 .apreciarAlgunElemento "mar" . salirConGenteQueHabla ("aleman") $ turista

--parte b
ana = UnTurista 0 21 False ["español"]
beto = UnTurista 15 15 True ["aleman"]
cathi = UnTurista 15 15 True ["aleman","catalan"]

--PARTE 2
--parte a
obtenerPorcentaje::Int->Int->Int
obtenerPorcentaje porcentaje valor = valor * porcentaje `div` 100

hacerExcursion :: Turista ->Excursion->Turista
hacerExcursion turista excursion = reducirStress (obtenerPorcentaje 10 (nivelStress (excursion turista))) . excursion $ turista

--parte b
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista ->Int 

deltaExcursionSegun :: Indice->Turista->Excursion->Int
deltaExcursionSegun  indice turista excursion = deltaSegun indice (hacerExcursion turista excursion) (turista)

cantIdiomas ::Turista->Int
cantIdiomas = length . idiomas

esEducativa::Turista->Excursion->Bool
esEducativa turista excursion = (>0) . deltaExcursionSegun (cantIdiomas) turista $ excursion

excursionesDesestresantes::Turista->[Excursion]->[Excursion]
excursionesDesestresantes turista excursiones = filter (esDesestresante turista) excursiones

esDesestresante::Turista->Excursion->Bool
esDesestresante turista excursion = (<= 3). deltaExcursionSegun nivelStress turista $ excursion

--punto 3
type Tour = [Excursion]

completo::Tour
completo =[caminar 20,apreciarAlgunElemento "cascada" , caminar 40 ,salirConGenteQueHabla "melmacquiano"]

ladoB::Excursion->Tour
ladoB excursionElegida=[paseoBarco Tranquila , excursionElegida , caminar 120]

islaVecina::Marea->Tour
islaVecina Fuerte =[paseoBarco Fuerte ,apreciarAlgunElemento "lago" ,paseoBarco Fuerte ]
islaVecina marea =[paseoBarco marea , irAlaPlaya , paseoBarco marea ]

--parte a
transformarTour::Tour -> Excursion
transformarTour tour = foldl1 (.) tour

hacerTour::Turista -> Tour->Turista
hacerTour turista tour = transformarTour tour $ (pagar turista tour) 

pagar::Turista -> Tour ->Turista
pagar turista tour = modificarStress (+(length tour)) turista

--parte b

tourConvicente::Turista->Tour->Bool
tourConvicente turista tour  =any (condicionConvincente turista) tour

condicionConvincente::Turista -> Excursion->Bool
condicionConvincente turista escursion = esDesestresante turista escursion  &&  dejaAcompañado turista escursion

dejaAcompañado::Turista->Excursion->Bool
dejaAcompañado turista excursion = (not.viajaSolo) (hacerExcursion turista excursion)

--parte c
saberEfectividad::[Turista]->Tour->Int
saberEfectividad turistas tour = sum . map (espiritualidad tour ) $ turistas

--falta implementar tour convincente filter (tourConvicente turista) tour
espiritualidad ::Tour->Turista->Int
espiritualidad tour turista = sum . map (perdidas2Indices nivelStress nivelCansancio turista) $ tour
perdidas2Indices::Indice->Indice->Turista->Excursion->Int
perdidas2Indices indice1 indice2 turista excursion = abs (deltaExcursionSegun indice1 turista excursion) + abs(deltaExcursionSegun indice2 turista excursion)


--punto 4
tourInfinito :: Excursion -> Tour
tourInfinito excursion = excursion : tourInfinito excursion
--parte a
vistaPlayasInfinitas= tourInfinito irAlaPlaya

--parte b
--Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
--Si se puede saber porque al utlizar la funcion de que si es convicente el tour ,solo necesita que almenos uno sea desestresante ,razon por
--la cual no importa si la lista es infinita ya que si almenos alguno cumple ,devuelve verdadero,gracias a la evaluacion diferida(lazy evaluation) que primero 
--se fija en la funcion ,que necesita, y no tanto en los valores,en este caso el "any" utiliza evaluacion diferida

--Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
--si existe ya que si ponemos una condicion de corte en la lista infinita dentro de la funcion,como un take o un head bastaria,
--en cambio si no podriamos modificar la funcion interna no habria casos para saber la efectividad del tour

