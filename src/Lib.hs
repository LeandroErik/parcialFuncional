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
    |(not.viajaSolo) turista = modificarStress (+1) turista

salirConGenteQueHabla::String->Excursion
salirConGenteQueHabla idioma turista = turista {idiomas = idioma:idiomas turista,viajaSolo = True}


modificarStress::(Int->Int)->Turista->Turista
modificarStress funcion turista =turista{nivelStress = (funcion.nivelStress) turista}

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
paseoBarco Fuerte turista = caminar (10) . salirConGenteQueHabla ("aleman") $ turista

ana = UnTurista 0 21 False ["español"]
beto = UnTurista 15 15 True ["aleman"]
cathi = UnTurista 15 15 True ["aleman","catalan"]


