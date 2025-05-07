{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | PatiVegano | BaconDeTofu | Papas | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente PatiVegano = 10
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa
    {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
    }

ingredienteBase :: Hamburguesa -> [Ingrediente]
ingredienteBase unaHamburguesa
    | elem Carne (ingredientes unaHamburguesa)  = [Carne]
    | elem Pollo (ingredientes unaHamburguesa) = [Pollo]
    | elem PatiVegano (ingredientes unaHamburguesa) = [PatiVegano]
    | otherwise = []

agregarIngrediente :: Hamburguesa -> [Ingrediente] -> Hamburguesa
agregarIngrediente unaHamburguesa nuevoIngrediente = unaHamburguesa {
    ingredientes = ingredientes unaHamburguesa ++ nuevoIngrediente}

agrandar :: Hamburguesa -> Hamburguesa
agrandar = agregarIngrediente <*> ingredienteBase

realizarDescuento :: Number -> Number -> Number
realizarDescuento precio rebaja = precio - precio * (rebaja / 100)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento rebaja unaHamburguesa = unaHamburguesa {
    precioBase = realizarDescuento (precioBase unaHamburguesa) rebaja}

pdepBurger :: Hamburguesa
pdepBurger = cuartoDeLibra {
    precioBase = precioBase (descuento 20 cuartoDeLibra),
    ingredientes = ingredientes (agregarIngrediente (agrandar (agrandar cuartoDeLibra)) [Panceta, Cheddar])
}

precioTotalIngredientes :: Hamburguesa -> Number
precioTotalIngredientes =  sum.map precioIngrediente.ingredientes

precioFinal :: Hamburguesa -> Number
precioFinal unaHamburguesa = precioBase unaHamburguesa + precioTotalIngredientes unaHamburguesa

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente cuartoDeLibra [Carne, Cheddar]

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente dobleCuarto [Curry]

delDia :: Hamburguesa -> Hamburguesa
delDia unaHamburguesa = agregarIngrediente (descuento 30 unaHamburguesa) [Papas]

hacerIngredienteVeggie :: Ingrediente -> Ingrediente
hacerIngredienteVeggie unIngrediente
    | unIngrediente == Carne || unIngrediente == Pollo = PatiVegano
    | unIngrediente == Cheddar = QuesoDeAlmendras
    | unIngrediente == Panceta = BaconDeTofu
    | otherwise = unIngrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie unaHamburguesa = unaHamburguesa {
    ingredientes = map hacerIngredienteVeggie (ingredientes unaHamburguesa)
}

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan unIngrediente
    | unIngrediente == Pan = PanIntegral
    | otherwise = unIngrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati unaHamburguesa = unaHamburguesa {
    ingredientes = map cambiarPan (ingredientes unaHamburguesa)}

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie.cambiarPanDePati $ dobleCuarto
