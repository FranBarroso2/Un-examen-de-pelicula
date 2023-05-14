data Pelicula = UnaPelicula {
    nombre :: String,
    anio :: Int,
    actores :: [String],
    minutos :: Int
}

taxiDriver, machete,  harryPotter9 :: Pelicula
taxiDriver = UnaPelicula "Taxi Driver" 1976 ["De Niro","Foster"] 113
machete = UnaPelicula "Machete" 1979 ["De Niro","Rodriguez"] 105
harryPotter9 = UnaPelicula "Harry Potter 9" 2022 ["Watson","Radcliffe","Grint"] 1000

--Punto 1

trabajaEn :: Pelicula -> String -> Bool
trabajaEn pelicula actor = actor `elem` actores pelicula

--Punto 2
type Genero = (String, [String])
todosLosActores :: [Genero]
todosLosActores = [("comedia", ["Carrey", "Grint", "Stiller"]),("accion", ["Stallone", "Willis","Schwarzenegger"]), ("drama", ["De Niro", "Foster"])]

comedia,accion,drama :: Genero
comedia = ("comedia", ["Carrey", "Grint", "Stiller"])
accion = ("accion", ["Stallone", "Willis","Schwarzenegger"])
drama = ("drama", ["De Niro", "Foster"])

películaEsDeCiertoGenero :: Genero -> Pelicula -> Bool
películaEsDeCiertoGenero genero pelicula = length (filter (esDeGenero genero ) (actores pelicula)) >= div (length (actores pelicula)) 2 + 1

esDeGenero :: Genero -> String -> Bool
esDeGenero genero actor = actor `elem` snd genero

-- filter ((x>1)listado) [1,2,3] -> [2,3]
-- map ((x+1)listado) [1,2,3] -> [2,3,4]
-- all ((x>1)listado) [1,2,3] -> False // [2,3,4] -> True
-- any ((x>1)listado) [1,2,3] -> True *Porque hay uno que si se encuentra*
-- sum suma toda la lista [1,2,3] -> 6
--fold se cae

--Punto 3
type Premios = Pelicula -> Bool
clasico,plomo,tresMultitud,premialeta :: Premios
clasico pelicula =siEsMayorN 1970 (anio pelicula) && siEsMayorN (anio pelicula) 1979 
plomo pelicula = siEsMayorN 180 (minutos pelicula)
tresMultitud pelicula = siEsMayorN 3 (length(actores pelicula)) 
nMultitud :: Int -> Premios
nMultitud cantidad pelicula = siEsMayorN cantidad (length(actores pelicula))
siEsMayorN :: Int -> Int -> Bool
siEsMayorN multitud cantidadActores = cantidadActores > multitud
premialeta pelicula = "De Niro" `elem` actores pelicula

--Punto 4
type Festival =[Premios]
cannes,berlin,argentina :: Festival
cannes = [clasico, tresMultitud]
berlin = [clasico, plomo, nMultitud 4]
argentina = [nMultitud 2,plomo]

cuantosPremiosGano :: Pelicula -> Festival -> Int
cuantosPremiosGano pelicula festival = length(filter(ganaPremio pelicula) festival)

ganaPremio :: Pelicula -> Premios -> Bool
ganaPremio pelicula premio = premio pelicula

--Punto 5
{-No se podria saber si una pelicula es de cierto genero porque nunca terminaria de hacer
el filter entonces nunca sabriamos si una pelicula pertenece a un genio
-}
