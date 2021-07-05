{-# LANGUAGE FlexibleContexts #-}
module Graphics.JPEG where

import Data.Char(chr,ord)
import Data.Word
import Data.Int
import Data.List(transpose)
import Data.Bits(testBit)
-- import Control.Monad(sequence, replicateM, liftM, liftM2)
import Debug.Trace
import System.IO
import Control.Monad.State -- (State(..), evalState, runState, get, put)

----------------------------------------------
-- Auxiliary functions:
----------------------------------------------

infixr 9 `o`
o :: (c->d) -> (a->b->c) -> (a->b->d)
(g `o` f) x y = g (f x y)

type Table a  =  Int -> a

subst :: Eq a => a -> b -> (a->b) -> (a->b)
subst i e t j  | i==j      =  e
               | otherwise =  t j

multi  :: Int -> [a] -> [a]
multi n = concatMap (replicate n)

ceilDiv    :: Int -> Int -> Int
ceilDiv n d = (n+d-1)`div`d


----------------------------------------------
-- Pixels
----------------------------------------------

data PixelRGB = PixelRGB { red, green, blue :: {-#UNPACK#-} !Word8 }

sane :: Int -> Int
sane x = (0 `max` x) `min` 255

yCbCr2RGB :: [Int8] -> PixelRGB
yCbCr2RGB [y,cb,cr]
  = let yi  :: Int
        yi  = fromIntegral y
        cbi :: Int
        cbi = fromIntegral cb
        cri :: Int
        cri = fromIntegral cr
        r  :: Word8
        r = fromIntegral (sane (128 + yi              + 8*cri `div` 5))
        g :: Word8
        g = fromIntegral (sane (128 + yi - cbi `div`3 - 4*cri `div` 5))
        b :: Word8
        b = fromIntegral (sane (128 + yi + cbi+cbi))
    in  PixelRGB r g b       
yCbCr2RGB _ = error "yCbCr2RGB needs 3 elements"

----------------------------------------------
-- Matrix manipulation
----------------------------------------------

type Dim   = (Int,Int)
type Mat a = [[a]]

matapply    :: Num a  =>  Mat a -> [a] -> [a]
matapply m v = map (inprod v) m

inprod :: Num a  =>  [a] -> [a] -> a
inprod  = sum `o` zipWith (*)

matmap :: (a->b) -> Mat a -> Mat b
matmap  = map . map

matconcat :: Mat (Mat a) -> Mat a
matconcat  = concatMap (map concat . transpose)

matzip :: [Mat a] -> Mat [a]
matzip  = map transpose . transpose


----------------------------------------------
-- Bit Streams
----------------------------------------------

type Bits = [Bool]

byte2bits  :: Int -> Bits
byte2bits x = map (testBit x) [7,6..0]

{-
byte2bits x = zipWith (>=) (map (rem x) powers) (tail powers)
      where powers = [256,128,64,32,16,8,4,2,1]
-}

string2bits :: String -> Bits
string2bits  = concatMap (byte2bits . ord)

byte2nibs  :: Int -> (Int,Int)
byte2nibs x = x `divMod` 16


----------------------------------------------
-- Binary Trees
----------------------------------------------

data Tree a  =  Nil
             |  Tip a
             |  Bin (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Nil       =  Nil
    fmap f (Tip a)   =  Tip (f a)
    fmap f (Bin x y) =  Bin (fmap f x) (fmap f y)


----------------------------------------------
-- Primitive State Functions
----------------------------------------------

empty  ::  State [a] Bool
empty = liftM null get

item   ::  State [a] a
item = do (x:xs) <- get
          put xs
          return x

peekitem   ::  State [a] a
peekitem = liftM head get


entropy :: State String String
entropy = do ys <- get
             case ys of
               ('\xFF':'\x00':xs) -> do put xs
                                        liftM ('\xFF':) entropy
               ('\xFF': _    :xs) -> do put xs
                                        entropy -- continue after restart marker
               ( x           :xs) -> do put xs
                                        liftM (x:) entropy
               []                 -> return []

----------------------------------------------
-- Auxiliary State Functions
----------------------------------------------

byte :: State String Int
byte = liftM ord item

word :: State String Int
word  =  do a<-byte
            b<-byte
            return $ a*256+b

-- word = liftM2 ((+) .(256*)) byte byte

nibbles :: State String (Int,Int)
nibbles  = liftM byte2nibs byte


----------------------------------------------
-- State Function Combinators
----------------------------------------------

matrix      :: Monad m => Dim -> m a -> m (Mat a)
matrix (y,x) = replicateM y . replicateM x

many   :: Monad (State [a]) => State [a] b -> State [a] [b]
many f  = do b  <- empty
             if b
              then return []
              else liftM2 (:) f (many f)

                
{-  
-- definition using State constructor
sf_uncur f = State h
  where h (a,b) = (c, (a2,b2))
            where State g      = f b
                  ((b2,c),a2)  = g a
-}

-- alternative definition using State interface
sf_uncur  :: (b -> State a (b,c)) -> State (a,b) c
sf_uncur f = do (a,b) <- get
                let g = f b
                let ((b2,c),a2) = runState g a
                put (a2,b2)
                return c

-- definition using State constructor
sf_curry :: State (a,b) c -> b -> State a (b,c)
sf_curry s = f
          where f b = state g
                 where g a = ((b2,c),a2) 
                        where (c,(a2,b2)) = runState s (a,b)

{-
-- uses non existent constructors
sf_curry :: State (a,b) c -> b -> State a (b,c)
sf_curry (State h) = f
          where f b = State g
                 where g a = ((b2,c),a2) 
                        where (c,(a2,b2)) = h (a,b)
-}
{-
-- unfinished alternative definition using State interface
sf_curry       :: State (a,b) c -> b -> State a (b,c)
sf_curry sh = f
          where f b = do let (c,(a2,b2)) = runState sh (a,b)
                         (b2,c) <- get
                         ????
-}


----------------------------------------------
-- Huffman Trees
----------------------------------------------

build :: Monad (State [(a,Int)]) => Int -> State [(a,Int)] (Tree a)
build n = do b     <- empty
             (_,s) <- peekitem
             t     <- if   n==s
                      then do (v,_) <- item
                              return $  Tip v
                      else do x <- build (n+1)
                              y <- build (n+1)
                              return $ Bin x y
             return $ if b then Nil else t
             

huffmanTree ::  Monad (State [(a,Int)]) => [[a]] -> Tree a
huffmanTree  =  evalState (build 0) . concat . zipWith f [1..16]
         where  f s = map (\v->(v,s))


treeLookup              :: Tree a -> State Bits a
treeLookup (Tip x)       = return x
treeLookup (Bin lef rit) = do b <- item
                              treeLookup (if b then rit else lef)
treeLookup Nil           = error "treeLookup needs nonempty tree"


receive    :: Int -> State Bits Int
receive 0  = return 0
receive k  = do n <- receive (k-1)
                b <- item
                return $ 2*n + (if b then 1 else 0)

dcdecode  :: Tree Int -> State Bits Int
dcdecode t = do s <- treeLookup t
                v <- receive s
                return $ extend v s

extend :: Int -> Int -> Int
extend v t | t==0      =  0
           | v>=vt     =  v
           | otherwise =  v + 1 - 2*vt
                   where  vt = 2^(t-1)

acdecode :: Tree (Int,Int) -> Int -> State Bits [Int]
acdecode t k 
  = do (r,s) <- treeLookup t
       let  k2 =  k + r + 1
       if   r==0&&s==0 
        then return (replicate (64-k) 0)
        else do x <-  receive s
                xs <- if k2>=64 then return [] else acdecode t k2
                return $  replicate r 0 ++ (extend x s:xs)


----------------------------------------------
-- Discrete Cosine Transform
----------------------------------------------

idct1 :: [Float] -> [Float]
idct1  = matapply cosinuses

idct2 :: Mat Float -> Mat Float
idct2  = transpose . map idct1 . transpose . map idct1

cosinuses :: Mat Float
cosinuses  = map f [1,3..15]
     where f :: Int -> [Float]
           f x = map g [0..7]
             where g :: Int -> Float
                   g 0 = 0.5 / sqrt 2.0
                   g u = 0.5 * cos(fromIntegral(x*u)*(pi/16.0))


----------------------------------------------
-- Dequantization and Upsampling
----------------------------------------------

type QuaTab = [Int]

dequant :: QuaTab -> [Int] -> Mat Int8
dequant  =  matmap truncate `o` idct2 `o` zigzag `o` map fromIntegral `o` zipWith (*) 

upsamp      :: Dim -> Mat a -> Mat a
upsamp (1,1) = id
upsamp (x,y) = multi y . map (multi x)

zigzag :: [a] -> Mat a
zigzag xs = matmap (xs!!) [[ 0, 1, 5, 6,14,15,27,28]
                          ,[ 2, 4, 7,13,16,26,29,42]
                          ,[ 3, 8,12,17,25,30,41,43]
                          ,[ 9,11,18,24,31,40,44,53]
                          ,[10,19,23,32,39,45,52,54]
                          ,[20,22,33,38,46,51,55,60]
                          ,[21,34,37,47,50,56,59,61]
                          ,[35,36,48,49,57,58,62,63]
                          ]


-- alternative definition, more intensional but not necessarily clearer

zigzag2 :: [a] -> Mat a
zigzag2 cs =  (transpose . map concat . transpose . fst . foldr f e) [1..15]
      where e = ([],reverse cs)
            f n (rss,xs) = (bs:rss, ys)
              where (as,ys) = splitAt (min n (16-n)) xs
                    rev = if even n then id else reverse
                    bs =    replicate (max (n-8) 0) [] 
                         ++ map (:[]) (rev as) 
                         ++ replicate (max (8-n) 0) []

----------------------------------------------
-- Data decoding
----------------------------------------------

type DataUnit =  Mat Int8
type Picture  =  Mat PixelRGB

type DataSpec =  (Dim, QuaTab, Tree Int, Tree (Int,Int))
type MCUSpec  =  [(Dim, DataSpec)]

dataunit ::  DataSpec -> Int -> State Bits (Int,DataUnit)
dataunit (u,q,dc,ac) x = do dx <- dcdecode dc
                            xs <- acdecode ac 1
                            let y=x+dx 
                            return (y, upsamp u (dequant q (y:xs))) 

units    :: Dim -> DataSpec -> State (Bits,Int) DataUnit
units dim = fmap matconcat . matrix dim . sf_uncur . dataunit

units2  :: (Dim,DataSpec) -> Int -> State Bits (Int,DataUnit)
units2   =  sf_curry . uncurry units

mcu     :: MCUSpec -> [ Int -> State Bits (Int,DataUnit) ]
mcu      = map units2

mcu2    :: MCUSpec -> [Int] -> [ State Bits (Int,DataUnit) ]
mcu2     = zipWith ($) . mcu

mcu3    :: MCUSpec -> [Int] -> State Bits ([Int],[DataUnit])
mcu3     = fmap unzip `o` sequence `o` mcu2

mcu4    :: MCUSpec -> State (Bits,[Int]) Picture
mcu4     = fmap (matmap yCbCr2RGB . matzip) . sf_uncur . mcu3

picture :: Dim -> MCUSpec -> State (Bits,[Int]) Picture
picture dim  = fmap matconcat . matrix dim . mcu4

-- if you prefer one-liners over auxiliary definitions:
{-
picture2 dim =     fmap matconcat 
                .  matrix dim 
                .  fmap (matmap yCbCr2RGB . matzip)
                .  sf_uncur 
                .  fmap unzip
               `o` sequence
               `o` zipWith ($)
                .  map (sf_curry . uncurry units)
-}


----------------------------------------------
-- JPEG Header structure
----------------------------------------------

type FrameCompo = (Int,Dim,Int)
type ScanCompo  = (Int,Int,Int)
type QtabCompo  = (Int,[Int])

type SOF = (Dim,[FrameCompo])
type DHT = (Int,Int,Tree Int)
type SOS = ([ScanCompo],Bits)
type DQT = [QtabCompo]
type XXX = (Char,String)

frameCompo :: State String (Int, (Int,Int), Int)
frameCompo = do c <- byte
                dim <- nibbles
                tq <- byte
                return $ (c,dim,tq) 

scanCompo :: State String (Int,Int,Int)
scanCompo  = do cs <- byte
                (td,ta) <- nibbles
                return $ (cs,td,ta)

qtabCompo :: State String (Int, [Int])
qtabCompo  = do (p,ident) <- nibbles
                qt <- replicateM 64 (if p==0 then byte else word)
                return $ (ident,qt)


sofSeg :: State String ( (Int,Int), [(Int, (Int,Int), Int)] )
sofSeg = do _ <- word  
            _ <- byte
            y <- word
            x <- word
            n <- byte
            fcs <- replicateM n frameCompo
            return $ ((y,x), fcs)

dhtSeg :: State String (Int, Int, Tree Int)
dhtSeg =  do _ <- word
             (tc,th) <- nibbles
             ns <- replicateM 16 byte
             v <- sequence (map (flip replicateM byte) ns)
             return $  (tc, th, huffmanTree v)

dqtSeg :: State String [(Int, [Int])]
dqtSeg = do len <- word
            replicateM ((len-2)`rem`64) qtabCompo

sosSeg :: State String ( [(Int,Int,Int)], Bits)
sosSeg = do _ <- word
            n <- byte
            scs <- replicateM n scanCompo
            _ <- byte
            _ <- byte
            _   <- nibbles
            ent <- entropy
            return $ (scs, string2bits ent)
            
segment :: (SOF->a, DHT->a, DQT->a, SOS->a, XXX->a) -> State String a
segment (sof,dht,dqt,sos,xxx) =
  do _ <- item
     c <- item
     -- () <- trace ("segment: " ++ show (ord c)) (return ())
     s <- case c of
           '\xC0' -> fmap sof sofSeg
           '\xC4' -> fmap dht dhtSeg
           '\xDB' -> fmap dqt dqtSeg
           '\xDA' -> fmap sos sosSeg
           '\xD8' -> return $ xxx (c,[])
           '\xD9' -> return $ xxx (c,[])
           _      -> do n <- word
                        s <- replicateM (n-2) item
                        return $ xxx (c,s)
     return s

----------------------------------------------
-- JPEG Decoder
----------------------------------------------

type Huf   =  (Table(Tree Int), Table(Tree (Int,Int)))
type Sof   =  (Dim, Table(Dim,QuaTab))
type Qua   =  Table QuaTab
type State2 =  (Sof,Huf,Qua,Picture)

segments :: State String [State2->State2]
segments = many (segment (sof,dht,dqt,sos,xxx))
     where sof x s@(_,b,c,d) = (evalSOF x s, b, c, d)
           dht x s@(a,_,c,d) = (a, evalDHT x s, c, d)
           dqt x s@(a,b,_,d) = (a, b, evalDQT x s, d)
           sos x s@(a,b,c,_) = (a, b, c, evalSOS x s)
           xxx x s           = trace ("extra data: " ++ show x) s

errRes  :: State2
errRes   = (error "SOF", error "DHT", error "DQT", error "SOS")

evalSOF :: SOF -> State2 -> Sof
evalSOF (dim,xs) (~(_,sof),_,qua,_)  =  (dim, foldr f sof xs)
                                 where  f (i,d,q) = subst i (d,qua q)

evalDHT :: DHT -> State2 -> Huf
evalDHT (0,i,tree) (_,~(hdc,hac),_,_) = (subst i tree hdc, hac)
evalDHT (1,i,tree) (_,~(hdc,hac),_,_) = (hdc, subst i (fmap byte2nibs tree) hac)
evalDHT _          _                  = error "evalDHT: unexpected case"

evalDQT :: DQT -> State2 -> Qua
evalDQT xs (_,_,qua,_) =  foldr f qua xs
                   where  f (i,q) = subst i q 

evalSOS :: SOS -> State2 -> Picture
evalSOS (cs,xs) (((y,x),sof),(h0,h1),_,_) 
                                 =  map (take x) (take y (evalState thePicture (xs,[0,0,0])))
            where thePicture     =  picture repCount mcuSpec
                  mcuSpec        =  map f cs
                  f (ident,dc,ac)=  (d, (upsCount d, qt, h0 dc, h1 ac))
                             where  (d,qt) = sof ident
                  repCount       =  ( ceilDiv y (8*maxy), ceilDiv x (8*maxx) ) 
                  upsCount (h,w) =  ( maxy `div` h, maxx `div` w )
                  maxy           =  maximum ( map (fst.fst) mcuSpec )
                  maxx           =  maximum ( map (snd.fst) mcuSpec )

jpegDecode :: String -> Picture
jpegDecode  = pi4 . foldl (flip ($)) errRes . evalState segments 
        where pi4 (_,_,_,x) = x


----------------------------------------------
-- PPM encoding
----------------------------------------------

ppmEncode :: Mat PixelRGB -> String
ppmEncode xss  
   =  "P6\n# Creator: Haskell JPEG decoder\n" 
      ++ w ++ " " ++ h ++ "\n255\n"
      ++ (concat . map rgbPixel2ppmChars . concat) xss
   where  w = show (length (head xss))
          h = show (length xss)


rgbPixel2ppmChars :: PixelRGB -> String
rgbPixel2ppmChars (PixelRGB r g b)
 = [ chr (fromIntegral r) , chr (fromIntegral g), chr (fromIntegral b) ]

----------------------------------------------
-- BMP Encoding
----------------------------------------------

bmpEncode :: Mat PixelRGB -> String
bmpEncode xss 
  = bmphead xss
    ++ concat (map bmpline (reverse xss))

bmphead :: [[a]] -> String
bmphead xss = (concat . map wor )
              ([ 19778, len, len `div` 65536, 0, 0 ,54, 0, 40
               , 0    , w  , 0, h, 0 , 1, 24, 0 ] ++ replicate 11 0)
        where w = length (head xss)
              h = length xss
              a = w*3
              p = bmpPad a
              len = 54 + (a+p)*h

bmpline :: [PixelRGB] -> String
bmpline xs 
   = let  as = concatMap rgbPixel2bmpChars xs
          n  = bmpPad (length as)
     in   if n==0
          then as
          else as ++ replicate n '\0'

bmpPad :: Int -> Int
bmpPad w
   = let m = w `mod` 4
     in  if m==0 then 0 else 4-m

rgbPixel2bmpChars :: PixelRGB -> String
rgbPixel2bmpChars (PixelRGB r g b)
 = [chr (fromIntegral b), chr (fromIntegral g), chr (fromIntegral r)]

wor :: Int -> String
wor x = [chr (x`rem`256), chr (x`div`256) ]


----------------------------------------------
-- Wrappers
----------------------------------------------

readBinFile :: String -> IO String
readBinFile f = do h <- openBinaryFile f ReadMode
                   hGetContents h

writeBinFile :: String -> String -> IO ()
writeBinFile f s = do h <- openBinaryFile f WriteMode
                      hPutStr h s
                      hClose h


jpgFile2bmpFile :: String -> String -> IO ()                                                  
jpgFile2bmpFile src dst
  =  do input <- readBinFile src 
        let output = (bmpEncode . jpegDecode) input
        writeBinFile dst output

jpgFile2ppmFile :: String -> String -> IO ()                                                  
jpgFile2ppmFile src dst
  =  do input <- readBinFile src 
        let output = (ppmEncode . jpegDecode) input
        writeBinFile dst output
