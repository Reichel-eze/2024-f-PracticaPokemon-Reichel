module Library where
import PdePreludat

data Pokemon = UnPokemon {
    nombrePoke :: String,
    tipo :: TipoPokemon
} deriving (Show , Eq)

data TipoPokemon = Planta | Agua | Fuego deriving (Show, Eq)

tieneVentajaContra :: TipoPokemon -> TipoPokemon -> Bool
tieneVentajaContra Planta Agua = True       -- uso pattern matching..
tieneVentajaContra Agua Fuego = True
tieneVentajaContra Fuego Planta = True
tieneVentajaContra _ _ = False              -- los demas casos mas generales

-- 1) Dado el código inicial, se quiere conocer a qué pokemones les puede ganar un pokemon dado 
-- (es decir, a cuáles aventaja por su tipo)

lesPuedeGanar :: Pokemon -> [Pokemon] -> [Pokemon]
lesPuedeGanar pokemon = filter (leGana pokemon) 

leGana :: Pokemon -> Pokemon -> Bool
leGana pokemonAtacante pokemonDefensor = tieneVentajaContra (tipo pokemonAtacante) (tipo pokemonDefensor) 

pierdeContra :: Pokemon -> Pokemon -> Bool
pierdeContra pokemonAtacante pokemonDefensor = leGana pokemonDefensor pokemonAtacante

-- 2) Teniendo un pokemon, se quiere conocer a cuántos puede ganarle de una lista de pokemones.

cantidadVictorias :: Pokemon -> [Pokemon] -> Number
cantidadVictorias pokemon = length . lesPuedeGanar pokemon 

-- 3) Conocer el pokemon que a más pokemones les puede ganar de una lista.

elMasPicante :: [Pokemon] -> Pokemon
elMasPicante pokemones = foldl1 (elMejorDeLosDos pokemones) pokemones
-- La funcion reduccion: necesita como parametro a la lista de pokemones (para analizar la cantidad de victorias)
-- La semilla: es el primer pokemon de la lista de pokemones 
-- La lista: es la lista de pokemones

-- El mejor de los dos es aquel pokemon que tiene una mayor cantidad de victorias de una lista de pokemones
elMejorDeLosDos :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
elMejorDeLosDos pokemones pokemon1 pokemon2 
    | cantidadVictorias pokemon1 pokemones >= cantidadVictorias pokemon2 pokemones = pokemon1
    | otherwise                                                                    = pokemon2

-- Algunos pokemones

charmander :: Pokemon
charmander = UnPokemon "charmander" Fuego

bulbasur :: Pokemon
bulbasur = UnPokemon "bulbasur" Planta

oddish :: Pokemon
oddish = UnPokemon "oddish" Planta

squirtle :: Pokemon
squirtle = UnPokemon "squirtle" Agua

-- 4) Se sabe que un destino a donde puede pelear un pokemon puede ser un gimnasio o una liga.

-- Los gimnasios son consecutivos (se sabe cuál es el siguiente de cada uno) 
-- Y al final de un camino siempre hay una liga. 
-- Por ahora sólo nos interesan los pokemones contrincantes que existen en una liga.

data Destino = UnGimnasio {nombre:: String, siguiente:: Destino} 
                   | UnaLiga {contrincantes:: [Pokemon] } deriving (Show, Eq)

-- Se desea saber si un pokemon está al horno en un destino. 
-- En un gimnasio, un pokemon siempre está al horno, 
-- y en una liga, está al horno cuando todos los contrincantes pueden ganarle. 

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno pokemon (UnGimnasio _ _) = True                                     -- un pokemon siempre esta al horno en un gimnasio
estaAlHorno pokemon liga = ((not. any (leGana pokemon)). contrincantes) liga    -- not any (seria que no existe ninguno --> en este caso "no existe ningun contrincante que el pokemon le puede ganar")
                                                                                -- "es decir, todos los contricantes le ganan, no existe ningun contrincante que pueda ganarle"
-- Este es el caso en el cual se utiliza un all en vez de un not any
estaAlHorno' :: Pokemon -> Destino -> Bool
estaAlHorno' pokemon (UnGimnasio _ _) = True
estaAlHorno' pokemon liga = (all (pierdeContra pokemon) . contrincantes) liga

-- 5) Saber si puedo viajar de un destino al otro. 
-- Consideraciones a tener en cuenta:
-- Desde una Liga no puedo viajar a otro destino.
-- Desde unGimnasio puedo viajar a miDestino si miDestino se encuentra entre los siguientes destinos de unGimnasio. 
-- Es decir, miDestino debe estar en el camino a seguir de unGimnasio.

puedoViajar :: Destino -> Destino -> Bool
puedoViajar (UnaLiga contrincantes) destino = (UnaLiga contrincantes) == destino       -- desde una liga no puedo viajar a otro destino (por ese simplemente al unico lugar que puedo viajar es cuando es igual al destino!!)
puedoViajar origen destino = origen == destino || puedoViajar (siguiente origen) destino
-- 1ERO. Chequeo el caso base "si el origen es igual al destino"
-- 2DOS. Hago la recursividad chequeando el siguiente del origen y vuelvo a chequear si es igual, etc...

-- Algunos destinos

gymRoca :: Destino
gymRoca = UnGimnasio "gymRoca" gymAgua

gymAgua :: Destino
gymAgua = UnGimnasio "gymAgua" gymElectrico

gymElectrico :: Destino
gymElectrico = UnGimnasio "gymElectrico" ligaKanto

ligaKanto :: Destino
ligaKanto = UnaLiga [bulbasur, oddish]

gymFuego :: Destino
gymFuego = UnGimnasio "gymFuego" gymPlanta

gymPlanta :: Destino
gymPlanta = UnGimnasio "gymPlanta" ligaGali

ligaGali :: Destino
ligaGali = UnaLiga [squirtle, charmander]