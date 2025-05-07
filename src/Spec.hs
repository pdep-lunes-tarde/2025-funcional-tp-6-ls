module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
import GHC.Num.BigNat (bigNatAdd)

hamburguesaConPollo :: Hamburguesa
hamburguesaConPollo = Hamburguesa {
    precioBase = 15,
    ingredientes = [Pan, Pollo, Cheddar, Pan]
}

hamburguesaSinElementoBase :: Hamburguesa
hamburguesaSinElementoBase = Hamburguesa {
    precioBase = 10,
    ingredientes = [Pan, Cheddar, Panceta]
}

hamburguesaConCarneYPollo :: Hamburguesa
hamburguesaConCarneYPollo = Hamburguesa {
    precioBase = 15,
    ingredientes = [Pan, Pollo, Carne, Cheddar, Pan]
}

hamburguesaVeggie :: Hamburguesa
hamburguesaVeggie = Hamburguesa {
    precioBase = 15,
    ingredientes = [Pan, PatiVegano, Pan]
}

hamburguesaSinPan :: Hamburguesa
hamburguesaSinPan = Hamburguesa {
    precioBase = 10,
    ingredientes = [Carne, Cheddar]
}


correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4

        describe "agrandar" $ do
            it "Una hamburguesa que tiene carne, se le agrega carne. " $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Carne]
                }
            it "Una hamburguesa que tiene pollo, se le agrega pollo. " $ do
                agrandar hamburguesaConPollo `shouldBe` Hamburguesa {
                    precioBase = 15,
                    ingredientes = [Pan, Pollo, Cheddar, Pan, Pollo]
                }
            it "Una hamburguesa que tiene carne y pollo, se le agrega cualquiera." $ do
                agrandar hamburguesaConCarneYPollo `shouldBe` Hamburguesa {
                    precioBase = 15,
                    ingredientes = [Pan, Pollo, Carne, Cheddar, Pan, Carne]
                }
            it "Una hamburguesa sin carne, carne vegana y sin pollo, no se le agrega nada." $ do
                agrandar hamburguesaSinElementoBase `shouldBe` hamburguesaSinElementoBase

        describe "agrandarIngrediente" $ do 
            -- esta funcion sabemos que anda, ya que se uso para la funcion agrandar, pero hacemos un test mas con algo que no sea ni carne ni pollo
            it "Si se le agrega un ingrediente a una hamburguesa, devuelve la hamburguesa con ese ingrediente agregado." $ do
                agregarIngrediente cuartoDeLibra [Cheddar] `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Cheddar]
                }

        describe "pdepBurger" $ do 
            it "La pdepBurger es una hamburguesa con los ingredientes del cuarto de libra pero con cheddar y panceta extra y precio base con 20% off " $ do
                pdepBurger `shouldBe` Hamburguesa {
                    precioBase = 16,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Carne, Carne, Panceta, Cheddar]
                }
            it "El precio final de la pdepBurger es de 110. " $ do
                precioFinal pdepBurger `shouldBe` 110

        describe "dobleCuarto" $ do 
            it "El dobleCuarto es una cuarto de libra con carne y cheddar." $ do
                dobleCuarto `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Carne, Cheddar]
                }
            it "El precio final de la dobleCuarto es de 84. " $ do
                precioFinal dobleCuarto `shouldBe` 84

        describe "bigPdep" $ do 
            it "La bigPdep es una doble cuarto de libra con curry." $ do
                bigPdep `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Carne, Cheddar, Curry]
                }
            it "El precio final de la bigPdep es de 89. " $ do
                precioFinal bigPdep `shouldBe` 89

        describe "delDia" $ do
            it "una hamburguesa con la promo del dia se le agrega papas y un descuento del 30% al precio base." $ do
                delDia bigPdep `shouldBe` Hamburguesa {
                    precioBase = 14,
                    ingredientes = [Pan, Carne, Cheddar, Pan, Carne, Cheddar, Curry, Papas]
                }
            it "un doble cuarto del dia vale 88." $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88

        describe "hacerVeggie" $ do
            it "Una hamburguesa no vegana se transforma en vegana reemplazando carne o pollo por PatiVegano y queso por QuesoDeAlmendras." $ do
                hacerVeggie cuartoDeLibra `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [Pan, PatiVegano, QuesoDeAlmendras, Pan]
                }
            it "Una hamburguesa ya vegana no se modifica." $ do
                hacerVeggie hamburguesaVeggie `shouldBe` hamburguesaVeggie
        
        describe "cambiarPanDePati" $ do
            it "Reemplaza el pan de una hamburguesa por pan integral." $ do
                cambiarPanDePati cuartoDeLibra `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [PanIntegral, Carne, Cheddar, PanIntegral]
                }
            it "Una hamburguesa sin pan no se modifica." $ do
                cambiarPanDePati hamburguesaSinPan `shouldBe` hamburguesaSinPan

        describe "dobleCuartoVegano" $ do
            it "El dobleCuartoVegano es un doble cuarto de libra transformado en vegano." $ do
                dobleCuartoVegano `shouldBe` Hamburguesa {
                    precioBase = 20,
                    ingredientes = [PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral, PatiVegano, QuesoDeAlmendras]
                }
            it "El precio final del dobleCuartoVegano es de 76." $ do
                precioFinal dobleCuartoVegano `shouldBe` 76