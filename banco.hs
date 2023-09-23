-- Algumas observacoes
--	Alguns interpretadores podem nao conseguir mostrar o resultado de uma funcao,
--	no entanto a logica esta correta e tudo compila normalmente

-- Bibliotecas
import Data.List

-- Banco de Dados
type Nome = String
type Sobrenome = String
type Sexo = Char
type CPF = String
type Dinheiro = Float
type Emprestimo = Bool -- Elegivel para emprestimo ou não

type Conta = (Nome, Sobrenome, Sexo, CPF, Dinheiro, Emprestimo)
type Contas = [Conta]

conta :: Contas
conta = [ ("Barbara", "Rodrigues",'F', "12345678901", 20000.99, True)
        , ("Joao", "Silva", 'M',"23456789012", 5000.00, False)
        , ("Ana", "Pereira", 'F',"34567890123", 7500.50, True)
        , ("Carlos", "Medeiros", 'M',"45678901234", 300.25, False)
        , ("Luisa", "Fernandez", 'F',"56789012345", 15000.75, True)
        , ("Pedro", "Alvares", 'M', "67890123456", 10.00, False)
        , ("Camila", "Mendes", 'F',"78901234567", 4500.30, True)
        , ("Roberto", "Castro", 'M',"89012345678", 1000.00, False)
        , ("Mariana", "Santos", 'F',"90123456789", 800.99, True)
        , ("Diego", "Brito", 'M',"01234567890", 9999.99, True)
        ]
        
-- Funcoes auxiliares
auxNome :: Conta->Nome
auxNome (nome, _, _, _, _, _) = nome

auxSobrenome :: Conta->Sobrenome
auxSobrenome (_, sobrenome, _, _, _, _) = sobrenome 

auxSexo :: Conta->Sexo
auxSexo (_, _, sexo, _, _, _) = sexo

auxCPF :: Conta->CPF
auxCPF (_, _, _, cpf, _, _) = cpf

auxDinheiro :: Conta->Dinheiro
auxDinheiro (_, _, _, _, dinheiro, _) = dinheiro

auxEmprestimo :: Conta->Emprestimo
auxEmprestimo (_, _, _, _, _, emprestimo) = emprestimo

maior :: Float->Float->Float
maior a b
 | a < b		= b
 | otherwise		= a
 
menor :: Float->Float->Float
menor a b
 | a < b		= a
 | otherwise		= b
 
 -- Funcoes principais
 
 -- Acha as contas que possuem mais dinheiro que o dinheiro passado por parametro 
maisRicos :: Dinheiro->Contas->[(Nome, CPF, Dinheiro)]
maisRicos dinheiro conta = [(n, c, d) |(n, _, _, c, d, _) <-conta, dinheiro <= d]

-- Acha quem pode fazer emprestimo ou nao (de acordo com o input do usuario)
podeEmprestimo :: Bool->Contas->Contas
podeEmprestimo verif [] = []
podeEmprestimo verif (x:xs)
 | verif == auxEmprestimo x		= x : podeEmprestimo verif xs
 | otherwise				= podeEmprestimo verif xs

-- Comparador para ordenar pelo nome
compararPorNome :: Conta->Conta->Ordering
compararPorNome (nome1, _, _, _, _, _) (nome2, _, _, _, _, _) = compare nome1 nome2

-- Ordena as contas pelo nome
ordenaPorNome :: Contas->Contas
ordenaPorNome = sortBy compararPorNome

-- Acha o mais rico
maisRico :: Contas->Conta
maisRico [x] = x
maisRico (x:y:xs)
 | auxDinheiro x >= auxDinheiro y 	= maisRico (x:xs)
 | otherwise 				= maisRico (y:xs)

-- Remove a primeira ocorrência de uma conta em uma lista baseado no CPF
removerContaPorCPF :: CPF->Contas->Contas
removerContaPorCPF _ [] = []
removerContaPorCPF cpf (x:xs)
 | cpf == auxCPF x 			= xs
 | otherwise 				= x : removerContaPorCPF cpf xs			

-- Ordena as contas usando Selection Sort
ordenaPorRiqueza :: Contas->Contas
ordenaPorRiqueza [] = []
ordenaPorRiqueza xs = 
    let rico = maisRico xs
    in rico : ordenaPorRiqueza (removerContaPorCPF (auxCPF rico) xs)

-- Lista todos os clientes de um determinado sexo
clientesPorSexo :: Sexo->Contas->Contas
clientesPorSexo s = filter (\conta -> auxSexo conta == s)

-- Calcula o saldo total mantido no banco
saldoTotal :: Contas->Dinheiro
saldoTotal = sum . map auxDinheiro

-- Calcula a media de saldo de clientes de um determinado sexo
mediaSaldoPorSexo :: Sexo->Contas->Float
mediaSaldoPorSexo s contas = 
    let clientes = clientesPorSexo s contas
    in saldoTotal clientes / fromIntegral (length clientes)
 
-- Lista clientes que tem um saldo abaixo da media de todos os clientes
clientesAbaixoMedia :: Contas->Contas
clientesAbaixoMedia contas = 
    let media = saldoTotal contas / fromIntegral (length contas)
    in filter (\conta -> auxDinheiro conta < media) contas
