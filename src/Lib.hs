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
    bandas :: [Banda]
} deriving (Show)

data Banda = UnaBanda {
    nombre :: String,
    genero :: Genero,
    descripcion :: [String],
    decibeles :: Int
} deriving Show

festivalHullabalooza = UnFestival "Argentina" 20000 "indiferente" [miranda, losRedondos, metallica, soda]


{-LOS GENEROS-}
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


-- LAS BANDAS --
{-Las bandas tienen un conjunto de descripciones realizadas por los críticos y los decibeles a los que suelen tocar. Además, cada vez que tocan,
 las bandas movilizan al público del festival de acuerdo al género al que pertenezcan.-}
    
    -- Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional. 
losRedondos = UnaBanda {
    nombre = "LosRedondos", 
    descripcion = ["legendaria", "pogojosa"], 
    decibeles = 45, 
    genero = rockNacional
    }

    -- Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.
soda = UnaBanda {
    nombre = "Soda", 
    descripcion = ["irrepetible"], 
    decibeles = 40, 
    genero = rockNacional
    }
    
    -- Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e "inodora".
miranda = UnaBanda {
    nombre = "Miranda", 
    descripcion = ["insipida", "incolora", "inodora"], 
    decibeles = 60, 
    genero = pop
    }

    -- Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del heavy metal.
metallica = UnaBanda {
    nombre = "Metallica", 
    descripcion = ["legendaria", "vendida"], 
    decibeles = 60, 
    genero = heavyMetal
    }

-- PUNTO 1 --
{-Modelar el festival, las bandas y los géneros de manera de poder definir la función tocar, que hace que 
 la banda toque y altere al público del festival de acuerdo a su género. (Definir también esa función) -}

tocar :: Banda -> Festival -> Festival
tocar banda = genero banda
