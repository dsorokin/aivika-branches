
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Branch.Generator
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is defined a random number generator,
-- where 'BrIO' is an instance of 'MonadGenerator'.
--
module Simulation.Aivika.Branch.Generator () where

import Control.Monad
import Control.Monad.Trans

import System.Random

import Data.IORef

import Simulation.Aivika.Trans
import Simulation.Aivika.Branch.Internal.Br

instance MonadGenerator BrIO where

  data Generator BrIO =
    Generator { generator01 :: BrIO Double,
                -- ^ the generator of uniform numbers from 0 to 1
                generatorNormal01 :: BrIO Double
                -- ^ the generator of normal numbers with mean 0 and variance 1
              }

  generateUniform = generateUniform01 . generator01

  generateUniformInt = generateUniformInt01 . generator01

  generateNormal = generateNormal01 . generatorNormal01

  generateExponential = generateExponential01 . generator01

  generateErlang = generateErlang01 . generator01

  generatePoisson = generatePoisson01 . generator01

  generateBinomial = generateBinomial01 . generator01

  newGenerator tp =
    case tp of
      SimpleGenerator ->
        liftIO newStdGen >>= newRandomGenerator
      SimpleGeneratorWithSeed x ->
        error "Unsupported generator type SimpleGeneratorWithSeed: newGenerator"
      CustomGenerator g ->
        g
      CustomGenerator01 g ->
        newRandomGenerator01 g

  newRandomGenerator g = 
    do r <- liftIO $ newIORef g
       let g01 = do g <- liftIO $ readIORef r
                    let (x, g') = random g
                    liftIO $ writeIORef r g'
                    return x
       newRandomGenerator01 g01

  newRandomGenerator01 g01 =
    do gNormal01 <- newNormalGenerator01 g01
       return Generator { generator01 = g01,
                          generatorNormal01 = gNormal01 }

-- | Generate an uniform random number with the specified minimum and maximum.
generateUniform01 :: BrIO Double
                     -- ^ the generator
                     -> Double
                     -- ^ minimum
                     -> Double
                     -- ^ maximum
                     -> BrIO Double
generateUniform01 g min max =
  do x <- g
     return $ min + x * (max - min)

-- | Generate an uniform random number with the specified minimum and maximum.
generateUniformInt01 :: BrIO Double
                        -- ^ the generator
                        -> Int
                        -- ^ minimum
                        -> Int
                        -- ^ maximum
                        -> BrIO Int
generateUniformInt01 g min max =
  do x <- g
     let min' = fromIntegral min
         max' = fromIntegral max
     return $ round (min' + x * (max' - min'))

-- | Generate a normal random number by the specified generator, mean and variance.
generateNormal01 :: BrIO Double
                    -- ^ normal random numbers with mean 0 and variance 1
                    -> Double
                    -- ^ mean
                    -> Double
                    -- ^ variance
                    -> BrIO Double
generateNormal01 g mu nu =
  do x <- g
     return $ mu + nu * x

-- | Create a normal random number generator with mean 0 and variance 1
-- by the specified generator of uniform random numbers from 0 to 1.
newNormalGenerator01 :: BrIO Double
                        -- ^ the generator
                        -> BrIO (BrIO Double)
newNormalGenerator01 g =
  do nextRef <- liftIO $ newIORef 0.0
     flagRef <- liftIO $ newIORef False
     xi1Ref  <- liftIO $ newIORef 0.0
     xi2Ref  <- liftIO $ newIORef 0.0
     psiRef  <- liftIO $ newIORef 0.0
     let loop =
           do psi <- liftIO $ readIORef psiRef
              if (psi >= 1.0) || (psi == 0.0)
                then do g1 <- g
                        g2 <- g
                        let xi1 = 2.0 * g1 - 1.0
                            xi2 = 2.0 * g2 - 1.0
                            psi = xi1 * xi1 + xi2 * xi2
                        liftIO $ writeIORef xi1Ref xi1
                        liftIO $ writeIORef xi2Ref xi2
                        liftIO $ writeIORef psiRef psi
                        loop
                else liftIO $ writeIORef psiRef $ sqrt (- 2.0 * log psi / psi)
     return $
       do flag <- liftIO $ readIORef flagRef
          if flag
            then do liftIO $ writeIORef flagRef False
                    liftIO $ readIORef nextRef
            else do liftIO $ writeIORef xi1Ref 0.0
                    liftIO $ writeIORef xi2Ref 0.0
                    liftIO $ writeIORef psiRef 0.0
                    loop
                    xi1 <- liftIO $ readIORef xi1Ref
                    xi2 <- liftIO $ readIORef xi2Ref
                    psi <- liftIO $ readIORef psiRef
                    liftIO $ writeIORef flagRef True
                    liftIO $ writeIORef nextRef $ xi2 * psi
                    return $ xi1 * psi

-- | Return the exponential random number with the specified mean.
generateExponential01 :: BrIO Double
                         -- ^ the generator
                         -> Double
                         -- ^ the mean
                         -> BrIO Double
generateExponential01 g mu =
  do x <- g
     return (- log x * mu)

-- | Return the Erlang random number.
generateErlang01 :: BrIO Double
                    -- ^ the generator
                    -> Double
                    -- ^ the scale
                    -> Int
                    -- ^ the shape
                    -> BrIO Double
generateErlang01 g beta m =
  do x <- loop m 1
     return (- log x * beta)
       where loop m acc
               | m < 0     = error "Negative shape: generateErlang."
               | m == 0    = return acc
               | otherwise = do x <- g
                                loop (m - 1) (x * acc)

-- | Generate the Poisson random number with the specified mean.
generatePoisson01 :: BrIO Double
                     -- ^ the generator
                     -> Double
                     -- ^ the mean
                     -> BrIO Int
generatePoisson01 g mu =
  do prob0 <- g
     let loop prob prod acc
           | prob <= prod = return acc
           | otherwise    = loop
                            (prob - prod)
                            (prod * mu / fromIntegral (acc + 1))
                            (acc + 1)
     loop prob0 (exp (- mu)) 0

-- | Generate a binomial random number with the specified probability and number of trials. 
generateBinomial01 :: BrIO Double
                      -- ^ the generator
                      -> Double 
                      -- ^ the probability
                      -> Int
                      -- ^ the number of trials
                      -> BrIO Int
generateBinomial01 g prob trials = loop trials 0 where
  loop n acc
    | n < 0     = error "Negative number of trials: generateBinomial."
    | n == 0    = return acc
    | otherwise = do x <- g
                     if x <= prob
                       then loop (n - 1) (acc + 1)
                       else loop (n - 1) acc
