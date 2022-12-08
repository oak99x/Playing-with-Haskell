----------------------------------------------------------------------------------------------------
-- Objetivo
-- O objetivo desta tarefa é construir um programa Haskell capaz de simular um computador hipotético
-- de arquitetura simplificada.
----------------------------------------------------------------------------------------------------

-- Define a memoria como uma colecao de tuplas (endereco,valor) => [(Int,Int)]
type Reg = (Int, Int)

data Instruction = LOD Int |
                   STO Int |
                   JMP Int |
                   JMZ Int |
                   CPL Int |
                   AND Int |
                   ADD Int |
                   SUB Int |
                   NOP |
                   HLT deriving Show

buscar :: [Reg] -> Instruction
buscar (pc:memo)
    | snd instPos == 2 = LOD content
    | snd instPos == 4 = STO content
    | snd instPos == 6 = JMP content
    | snd instPos == 8 = JMZ content
    | snd instPos == 10 = CPL content
    | snd instPos == 12 = AND content
    | snd instPos == 14 = ADD content
    | snd instPos == 16 = SUB content
    | snd instPos == 18 = NOP
    | snd instPos == 20 = HLT
    | otherwise = NOP
        where
            instPos =  head $ dropWhile ((/= snd pc) . fst) memo
            content = snd $ memo !! (fst instPos + 1)
                   

evalInstruction :: Instruction -> [Reg] -> [Reg]
evalInstruction (LOD e) memo = writeMem 999 (readMem memo e) memo
evalInstruction (STO e) memo = writeMem e (readMem memo 999) memo
evalInstruction (JMP e) memo = writeMem 1000 e memo
evalInstruction (JMZ e) memo = if readMem memo 999 == 0 then writeMem 1000 e memo else memo
evalInstruction (CPL e) memo = if readMem memo e < 0 then writeMem 999 1 memo else writeMem 999 0 memo
evalInstruction (AND e) memo = if readMem memo e == 0 && readMem memo 999 == 0 then writeMem 999 1 memo else writeMem 999 0 memo
evalInstruction (ADD e) memo = writeMem 999 (readMem memo e + readMem memo 999) memo
evalInstruction (SUB e) memo = writeMem 999 (readMem memo 999 - readMem memo e) memo
evalInstruction NOP memo = memo
evalInstruction HLT memo = memo

cpu :: [Reg] -> [Reg]
cpu (x:memo) = let inst = buscar (x:memo) in
                 case inst of
                     HLT -> memo
                     NOP -> memo
                     _ -> cpu $ evalInstruction inst ((1000, snd x + 2):memo)

-- Ler a memoria
-- Retornar o conteudo do endereço de memoria
-- readMem(memoria,endereco)=conteudo
readMem :: [Reg] -> Int -> Int
readMem (x:xs) e
    | e == fst x = snd x
    | e /= fst x = readMem xs e

-- Escrever na memória
-- Armazenar o conteúdo em um endereço de memória
-- Capaz de adicionar um registrador que apareça em uma instrução, mas não no input.
-- Depende de que readMem encontre o dado que será escrito nesse novo registrador.
-- writeMem(memoria,endereço,conteudo)=memoria
-- writeMem :: Int -> Int -> [Reg] -> [Reg]
-- writeMem e c = map (\reg -> if fst reg == e then (fst reg, c) else reg)
writeMem :: Int -> Int -> [Reg] -> [Reg]
writeMem end con [] = [(end, con)]
writeMem end con (reg:memo)
    | end /= 1000 && fst reg == 1000 = reg : writeMem end con memo
    | fst reg == end = (fst reg, con) : memo
    | end < fst reg = (end,con):reg:memo
    | otherwise = reg : writeMem end con memo


----------------------------------------------------------------------------
-- Simulador ---------------------------------------------------------------

simular :: [Reg] -> [Reg]
simular memo = cpu ((1000, 0) : memo)

----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Programas teste ---------------------------------------------------------

-- Simula-se o exemplo e os  3 programas pedidos no enunciado

-- Exemplo: Resp = A + B
-- progTest
-- 0 LOD 240
-- 2 ADD 241
-- 4 STO 251
-- 6 HLT NOP
-- Observação: a posição 999 é o ACC
-- Observação 2: a posição 1000 é o PC
progTest :: [(Int,Int)]
progTest = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,0),(241,1),(251,0),(999,0)]

----------------------------------------------------------------------------

--Prog1 Resp = A + B –2;
prog1 :: [(Int, Int)]
prog1 = [
        -- Prog
        (0,2),(1,240),  -- LOD A
        (2,14),(3,241), -- ADD B
        (4,16),(5,242), -- SUB (A+B)
        (6,4),(7,251),  -- STO RESP
        (8,20),(9,18),  -- HLT NOP

        -- Data
        (240,2),        -- A
        (241,4),        -- B
        (242,2),        -- -2
        (251,0),        -- RESP
        (999,0)         --ACC
    ]

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- Prog2 Resp = A * B;
prog2 :: [Reg]
prog2 = [
    -- Prog
    (0,2),(1,251),      -- LOD RESP
    (2,14),(3,240),     -- ADD A
    (4,4),(5,251),      -- STO RESP
    (6,2),(7,241),      -- LOD B
    (8,16),(9,242),     -- SUB 1
    (10,4),(11,241),    -- STO B
    (12,16),(13,242),   -- SUB 1
    (14,10),(15,999),   -- CPL 999
    (16,8),(17,0),      -- JMZ 0
    (18,20),(19,18),    -- HLT NOP

    -- Data
    (240,50),            -- A
    (241,10),           -- B
    (242,1),            -- 1
    (251,0),            -- Resposta
    (999,0)]            -- ACC

----------------------------------------------------------------------------
----------------------------------------------------------------------------

--Prog3 A = 0; Resp = 1; while(A < 10) { A = A + 1; Resp = Resp + 2; }
prog3 :: [Reg]
prog3 = [
    -- Prog
    (0,2),(1,240),     -- LOD A
    (2,16),(3,200),    -- SUB 10
    (4,4),(5,180),     -- STO T
    (6,10),(7,180),    -- CPL 180
    (8,8),(9,40),      -- JMZ 40
    (10,2),(11,240),   -- LOD A
    (12,14),(13,201),  -- ADD 1
    (14,4),(15,240),   -- STO A
    (16,2),(17,251),   -- LOD RESP
    (18,14),(19,202),  -- ADD 2
    (20,4),(21,251),   -- STO RESP
    (22,6),(23,0),     -- JMP 0
    (40,20),(5,16),    -- HLT NOP

    -- Data
    (180,0),           -- T
    (200,10),          -- 10
    (201,1),           -- 1
    (202,2),           -- 2
    (240,0),           -- A
    (251,1),           -- RESP
    (999,0)]           -- ACC