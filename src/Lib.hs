module Lib where
import Text.Show.Functions

laVerdad = True
 
 -- MODELADO DE Festival
    {-Festivales
    De cada festival se sabe el lugar donde se realiza, la cantidad y estado de ánimo inicial del público y las bandas que tocan en él, ordenadas cronológicamente.
    Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, tiene un público de 20000 personas con ánimo inicial “indiferente”.-}
type Lugar = String
type CantidadPersonas = Int
type Animo = String

type DescripBanda = [String]
type Decibeles = Int

data Festival = UnFestival {
    lugar :: String,
    cantidadPersonas :: Int,
    animoInicial :: String,
    bandas :: [String]
} deriving (Show, Eq)

data Banda = UnaBanda {
    nombre :: String,
    genero :: Genero,
    descripcion :: [String],
    decibeles :: Int
} deriving Show

festivalHullabalooza = UnFestival "Argentina" 20000 "indiferente" ["Miranda", "Los Redondos", "Metallica", "Soda"]


{-Los Generos-}
type Genero = Festival -> Festival


aumentarPublico :: CantidadPersonas -> Festival -> Festival
aumentarPublico cantidadAaumentar festival = festival {cantidadPersonas = cantidadPersonas festival + cantidadAaumentar}

cambiarAnimo :: Animo -> Festival -> Festival
cambiarAnimo animo festival = festival {animoInicial = animo}

    -- rock nacional: hace que el público aumente en 100 personas
rockNacional :: Festival -> Festival
rockNacional = aumentarPublico 100

    -- pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", duplican la cantidad y dejan el público "eufórico". 
pop :: Genero
pop festival
    | animoInicial festival == "indiferente" = (cambiarAnimo "euforico" .aumentarPublico (cantidadPersonas festival)) festival
    | otherwise = festival

    {-Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros-}

metal :: Animo -> Genero
metal terminacion festival = (agregarTerminacionAlAnimo terminacion.aumentoPorcentual) festival

agregarTerminacionAlAnimo :: Animo -> Festival -> Festival
agregarTerminacionAlAnimo terminacion festival = cambiarAnimo (animoInicial festival ++ " " ++ terminacion) festival

aumentoPorcentual :: Festival -> Festival
aumentoPorcentual festival = aumentarPublico (div (cantidadPersonas festival) 100) festival


    -- Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
heavyMetal :: Genero
heavyMetal = metal "pesado"

trashMetal :: Genero
trashMetal = metal "basura"
