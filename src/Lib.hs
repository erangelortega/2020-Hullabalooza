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
    animoDelFestival :: String,
    bandas :: [Banda]
} deriving (Show)

data Banda = UnaBanda {
    nombre :: String,
    genero :: Genero,
    descripcion :: [String],
    decibeles :: Int
} deriving Show


{-LOS GENEROS-}
type Genero = Festival -> Festival

aumentarPublico :: CantidadPersonas -> Festival -> Festival
aumentarPublico cantidadAaumentar festival = festival {cantidadPersonas = cantidadPersonas festival + cantidadAaumentar}

cambiarAnimo :: Animo -> Festival -> Festival
cambiarAnimo animo festival = festival {animoDelFestival = animo}


    -- rock nacional: hace que el público aumente en 100 personas
rockNacional :: Festival -> Festival
rockNacional = aumentarPublico 100

    -- pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", duplican la cantidad y dejan el público "eufórico". 
pop :: Genero
pop festival
    | animoDelFestival festival == "indiferente" = (cambiarAnimo "euforico" .aumentarPublico (cantidadPersonas festival)) festival
    | otherwise = festival

    {-Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros-}

metal :: Animo -> Genero
metal terminacion festival = (agregarTerminacionAlAnimo terminacion.aumentoPorcentual) festival

agregarTerminacionAlAnimo :: Animo -> Festival -> Festival
agregarTerminacionAlAnimo terminacion festival = cambiarAnimo (animoDelFestival festival ++ " " ++ terminacion) festival

aumentoPorcentual :: Festival -> Festival
aumentoPorcentual festival = aumentarPublico (div (cantidadPersonas festival) 100) festival

-- SUBGENEROS DEL METAL
    -- Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
heavyMetal :: Genero
heavyMetal = metal "pesado"

    --Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo. 
trashMetal :: Genero
trashMetal = metal "basura"


-- PUNTO 1 --
{-Modelar el festival, las bandas y los géneros de manera de poder definir la función tocar, que hace que 
 la banda toque y altere al público del festival de acuerdo a su género. (Definir también esa función) -}

tocar :: Banda -> Festival -> Festival
tocar banda = genero banda


-- PUNTO 2 --
{-Representar las bandas, géneros y el festival dados como ejemplo. Agregar una banda que sea trash metal.-}

    -- FESTIVAL
festivalHullabalooza = UnFestival "Argentina" 20000 "indiferente" [miranda, losRedondos, metallica, soda]

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

    -- banda de TRASH METAL
kreator = UnaBanda {
    nombre = "Kreator", 
    descripcion = ["Impactante", "inigualable"], 
    decibeles = 50, 
    genero = trashMetal
    }


-- PUNTO 3 --
{-Agregar la siguiente banda:
The strokes, que toca a 45 decibeles y está descripta como “suicidio asistido”, “emocional” y “linda”. No pertenece a ninguno de los géneros conocidos, pero hay expertos que afirman que es una fusión musical entre el pop y el heavy metal.  -}
theStrokes = UnaBanda {
    nombre = "The strokes", 
    descripcion = ["suicidio asistido", "emocional", "linda"], 
    decibeles = 45, 
    genero = heavyMetal.pop
    }


-- PUNTO 4 --
{-Definir la función suceder, que hace que suceda un festival. El resultado debe ser el mismo festival pero con el público en su situación final, luego de haber tocado todas las bandas. -}
suceder :: Festival -> Festival
suceder festival = foldl (flip tocar) festival (bandas festival)

suceder' :: Festival -> Festival
suceder' festival = (last . foldl (festivalesSucesivos) [festival]) (bandas festival)
    where festivalesSucesivos = (\festivalGenerado banda -> festivalGenerado ++ [tocar banda (last festivalGenerado)])


-- PUNTO 5 --
    {-Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo:
    Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”. 
    Acústica: Es la que toca a más de 55 decibeles. 
    Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.
    Definir las funciones que permitan clasificar a las bandas. Una banda puede clasificarse de más de una manera a la vez o ninguna.
    -}

type CriterioDeClasificacion = Banda -> Bool
criterios = [vendida, acustica, legendaria]

clasificarUnaBanda :: Banda -> [CriterioDeClasificacion]
clasificarUnaBanda banda = filter (verificarCriterio banda)  criterios

verificarCriterio :: Banda -> CriterioDeClasificacion -> Bool
verificarCriterio banda criterio = criterio banda

    -- desarrollo de criterios de clasificacion 
vendida :: CriterioDeClasificacion
vendida banda = ((> 2) . length . descripcion) banda || (elem "vendida". descripcion ) banda

acustica :: CriterioDeClasificacion
acustica = (> 55) . decibeles 

legendaria :: CriterioDeClasificacion
legendaria banda = (elem "legendaria" . descripcion) banda && ((> 40) . decibeles) banda





