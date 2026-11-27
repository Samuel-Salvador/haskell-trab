module Main where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- 1. MODELAGEM DE DADOS (Tipos proprios e simples)

data Categoria = Livro | Alimento | Eletronico
  deriving (Show, Eq)

-- Um Item tem: Nome, Categoria, Preco unitario e Quantidade.
data Item = Item String Categoria Double Int
  deriving (Show)

-- 2. FUNCOES PURAS (Regras de negocio sem efeito colateral)

subtotalItem :: Item -> Double
subtotalItem (Item _ _ preco qtd) = preco * fromIntegral qtd

-- Imutabilidade: devolve um NOVO Item com o preco alterado.
aplicarDescontoItem :: Double -> Item -> Item
aplicarDescontoItem percentual (Item nome cat preco qtd) =
  Item nome cat novoPreco qtd
  where
    novoPreco = preco * (1 - percentual / 100)

-- Pattern Matching na Categoria:
-- Livros = 15% de desconto | Alimentos = 5% | Eletronicos = 0%
descontoPorCategoria :: Item -> Item
descontoPorCategoria item@(Item _ Livro _ _)      = aplicarDescontoItem 15 item
descontoPorCategoria item@(Item _ Alimento _ _)   = aplicarDescontoItem 5 item
descontoPorCategoria item@(Item _ Eletronico _ _) = item

aplicarDescontosNoCarrinho :: [Item] -> [Item]
aplicarDescontosNoCarrinho = map descontoPorCategoria

totalCarrinho :: [Item] -> Double
totalCarrinho itens = sum (map subtotalItem itens)

filtrarPorCategoria :: Categoria -> [Item] -> [Item]
filtrarPorCategoria alvo = filter (\(Item _ cat _ _) -> cat == alvo)

removerPorNome :: String -> [Item] -> [Item]
removerPorNome nomeAlvo = filter (\(Item nome _ _ _) -> nome /= nomeAlvo)

-- Recursao simples para somar quantidades
totalUnidades :: [Item] -> Int
totalUnidades [] = 0
totalUnidades (Item _ _ _ qtd : resto) = qtd + totalUnidades resto

-- Guards para desconto progressivo no pedido
percentualDescontoProgressivo :: Double -> Double
percentualDescontoProgressivo total
  | total > 500 = 15
  | total > 200 = 10
  | otherwise   = 0

calcularTotalFinal :: Double -> (Double, Double, Double)
calcularTotalFinal subtotal = (percentual, valorDesconto, totalFinal)
  where
    percentual    = percentualDescontoProgressivo subtotal
    valorDesconto = subtotal * (percentual / 100)
    totalFinal    = subtotal - valorDesconto

-- ============================================================================
-- 3. FUNCOES AUXILIARES DE ENTRADA E SAIDA (IO)
-- ============================================================================

formatarItem :: Item -> String
formatarItem item@(Item nome cat preco qtd) =
  printf "  - %-22s [%-10s] %dx R$ %6.2f = R$ %7.2f"
    nome (show cat) qtd preco (subtotalItem item)

imprimirLista :: [Item] -> IO ()
imprimirLista []    = putStrLn "  (Nenhum item)"
imprimirLista itens = mapM_ (putStrLn . formatarItem) itens

lerTexto :: String -> IO String
lerTexto pergunta = do
  putStr pergunta
  getLine

-- Le um numero com seguranca usando readMaybe (nao trava se digitar texto invalido)
lerDoublePositivo :: String -> IO Double
lerDoublePositivo pergunta = do
  entrada <- lerTexto pergunta
  case readMaybe entrada of
    Just valor | valor > 0 -> return valor
    _ -> do
      putStrLn "Valor invalido! Digite um numero maior que zero (ex: 49.90)."
      lerDoublePositivo pergunta

lerIntPositivo :: String -> IO Int
lerIntPositivo pergunta = do
  entrada <- lerTexto pergunta
  case readMaybe entrada of
    Just valor | valor > 0 -> return valor
    _ -> do
      putStrLn "Quantidade invalida! Digite um numero inteiro maior que zero."
      lerIntPositivo pergunta

lerCategoria :: IO Categoria
lerCategoria = do
  putStrLn "Escolha a categoria:"
  putStrLn "  1 - Livro (15% de desconto)"
  putStrLn "  2 - Alimento (5% de desconto)"
  putStrLn "  3 - Eletronico (sem desconto por item)"
  opcao <- lerTexto "Opcao: "
  case opcao of
    "1" -> return Livro
    "2" -> return Alimento
    "3" -> return Eletronico
    _   -> do
      putStrLn "Opcao invalida! Digite 1, 2 ou 3."
      lerCategoria

-- ============================================================================
-- 4. ACOES DO MENU INTERATIVO
-- ============================================================================

mostrarResumo :: [Item] -> IO ()
mostrarResumo itens = do
  let comDesconto    = aplicarDescontosNoCarrinho itens
  let subtotalBruto  = totalCarrinho itens
  let subtotalComCat = totalCarrinho comDesconto
  let (percExtra, valDesc, totalFinal) = calcularTotalFinal subtotalComCat

  putStrLn "\n--- CARRINHO ORIGINAL (IMUTAVEL) ---"
  imprimirLista itens
  printf "Total de unidades: %d\n" (totalUnidades itens)
  printf "Subtotal bruto:    R$ %.2f\n" subtotalBruto

  putStrLn "\n--- APOS DESCONTOS POR CATEGORIA ---"
  imprimirLista comDesconto
  printf "Subtotal com descontos: R$ %.2f\n" subtotalComCat

  putStrLn "\n--- TOTAL FINAL (COM DESCONTO PROGRESSIVO) ---"
  printf "Desconto extra no pedido (%.0f%%): -R$ %.2f\n" percExtra valDesc
  printf "TOTAL A PAGAR:                   R$ %.2f\n" totalFinal

adicionarItem :: [Item] -> IO [Item]
adicionarItem itens = do
  putStrLn "\n--- ADICIONAR ITEM ---"
  nome  <- lerTexto "Nome do produto: "
  cat   <- lerCategoria
  preco <- lerDoublePositivo "Preco unitario (use ponto, ex: 59.90): "
  qtd   <- lerIntPositivo "Quantidade: "
  let novoItem = Item nome cat preco qtd
  putStrLn "Item adicionado com sucesso!"
  -- Imutabilidade: retornamos uma NOVA lista com o item adicionado
  return (itens ++ [novoItem])

removerItem :: [Item] -> IO [Item]
removerItem itens = do
  putStrLn "\n--- REMOVER ITEM ---"
  nome <- lerTexto "Nome exato do produto para remover: "
  let novaLista = removerPorNome nome itens
  if length novaLista == length itens
    then putStrLn "Nenhum item encontrado com esse nome."
    else putStrLn "Item removido!"
  return novaLista

filtrarMenu :: [Item] -> IO ()
filtrarMenu itens = do
  putStrLn "\n--- FILTRAR POR CATEGORIA ---"
  cat <- lerCategoria
  let comDesconto = aplicarDescontosNoCarrinho itens
  let filtrados   = filtrarPorCategoria cat comDesconto
  putStrLn ("\nItens na categoria " ++ show cat ++ " (ja com desconto):")
  imprimirLista filtrados

-- Loop principal usando RECURSAO:
-- Em vez de alterar uma variavel global, passamos a nova lista para a proxima chamada!
menu :: [Item] -> IO ()
menu itens = do
  putStrLn "\n========================================"
  putStrLn "  CARRINHO DE COMPRAS INTERATIVO"
  putStrLn "========================================"
  putStrLn "1 - Ver carrinho e calcular total"
  putStrLn "2 - Adicionar item"
  putStrLn "3 - Filtrar por categoria"
  putStrLn "4 - Remover item pelo nome"
  putStrLn "0 - Sair"
  opcao <- lerTexto "Escolha uma opcao: "

  case opcao of
    "1" -> do
      mostrarResumo itens
      menu itens
    "2" -> do
      novosItens <- adicionarItem itens
      menu novosItens
    "3" -> do
      filtrarMenu itens
      menu itens
    "4" -> do
      novosItens <- removerItem itens
      menu novosItens
    "0" -> putStrLn "Encerrando. Ate mais!"
    _   -> do
      putStrLn "Opcao invalida! Tente novamente."
      menu itens

-- Carrinho comeca vazio
carrinhoInicial :: [Item]
carrinhoInicial = []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  menu carrinhoInicial
