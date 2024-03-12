-- | Fully-connected neural network
module NeuralNetwork
  ( NeuralNetwork (..),
    Layer (..),
    Activation (..),
    genWeights,
    genNetwork,

    -- * Training
    optimize,
    AdamParameters (..),
    adamParams,
    optimizeAdam,

    -- * Inference
    inferBinary,
    accuracy,
  )
where

import Numeric.LinearAlgebra as LA
import Numeric.Morpheus.Activation
  ( relu,
    reluGradient,
    sigmoid,
    sigmoidGradient,
    tanhGradient,
    tanh_,
  )

-- Activation function:

-- * Rectified linear unit (ReLU)

-- * Sigmoid

-- * Identity (no activation)

data Activation = Relu | Sigmoid | Tanh | Id

-- Neural network layer: weights, biases, and activation
data Layer a = Layer (Matrix a) (Matrix a) Activation

type NeuralNetwork a = [Layer a]

-- | Weight and bias gradients
data Gradients a = Gradients (Matrix a) (Matrix a)

-- | Lookup activation function by a symbol
getActivation :: Activation -> (Matrix Double -> Matrix Double)
getActivation Id = id
getActivation Sigmoid = sigmoid
getActivation Relu = relu
getActivation Tanh = tanh_

-- | Lookup activation function derivative by a symbol
getActivation' ::
  Activation ->
  (Matrix Double -> Matrix Double -> Matrix Double)
getActivation' Id = flip const
getActivation' Sigmoid = \x dY -> sigmoidGradient x * dY
getActivation' Relu = \x dY -> reluGradient x * dY
getActivation' Tanh = \x dY -> tanhGradient x * dY

-- | Forward pass in a neural network:
-- exploit Haskell lazyness to never compute the
-- gradients.
forward ::
  NeuralNetwork Double -> Matrix Double -> Matrix Double
forward net dta = fst $ pass net (dta, undefined)

-- | Both forward and backward neural network passes
pass ::
  -- | `NeuralNetwork` `Layer`s: weights and activations
  NeuralNetwork Double ->
  -- | Data set
  (Matrix Double, Matrix Double) ->
  -- | NN computation from forward pass and weights gradients
  (Matrix Double, [Gradients Double])
pass net (x, tgt) = (pred, grads)
  where
    (_, pred, grads) = _pass x net

    _pass ::
      -- \| Input
      Matrix Double ->
      -- \| Layers
      [Layer Double] ->
      -- \| Output, prediction, and gradients
      (Matrix Double, Matrix Double, [Gradients Double])
    _pass input [] = (loss', pred, [])
      where
        pred = sigmoid input
        -- Gradient of cross-entropy loss
        -- after sigmoid activation.
        loss' = pred - tgt
    _pass input (Layer w b sact : layers) = (dX, pred, Gradients dW dB : t)
      where
        linearCombination = (input LA.<> w) + b
        y = getActivation sact linearCombination

        (dZ, pred, t) = _pass y layers

        dY = getActivation' sact linearCombination dZ
        dW = linearW' input dY
        dB = bias' dY
        dX = linearX' w dY

-- | Bias gradient
bias' :: Matrix Double -> Matrix Double
bias' dY = cmap (/ m) r
  where
    -- Sum elements in each row and return a new matrix
    r = matrix (cols dY) $ map sumElements (toColumns dY)
    m = fromIntegral $ rows dY

-- | Linear layer weights gradient
linearW' x dy = cmap (/ m) (tr' x LA.<> dy)
  where
    m = fromIntegral $ rows x

-- | Linear layer inputs gradient
linearX' w dy = dy LA.<> tr' w

-- | Gradient descent optimization
optimize ::
  -- | Learning rate
  Double ->
  -- | No of iterations
  Int ->
  -- | Neural network
  NeuralNetwork Double ->
  -- | Dataset
  (Matrix Double, Matrix Double) ->
  -- | Updated neural network
  NeuralNetwork Double
optimize lr iterN net0 dataSet = last $ take iterN (iterate step net0)
  where
    step net =
      let (_, dW) = pass net dataSet
       in zipWith f net dW

    f ::
      Layer Double ->
      Gradients Double ->
      Layer Double
    f (Layer w b act) (Gradients dW dB) =
      Layer (w - lr `scale` dW) (b - lr `scale` dB) act

data AdamParameters = AdamParameters
  { _beta1 :: Double,
    _beta2 :: Double,
    _epsilon :: Double,
    _lr :: Double
  }

-- | Adam optimizer parameters
adamParams =
  AdamParameters
    { _beta1 = 0.9,
      _beta2 = 0.999,
      _epsilon = 1e-8,
      _lr = 0.001
    }

-- \^ Learning rate

-- | Adam optimization
optimizeAdam ::
  -- | Adam parameters
  AdamParameters ->
  -- | No of iterations
  Int ->
  -- | Neural network layers
  NeuralNetwork Double ->
  -- | Dataset
  (Matrix Double, Matrix Double) ->
  NeuralNetwork Double
optimizeAdam p iterN w0 dataSet = w
  where
    s0 = map zf w0
    v0 = map zf w0
    zf (Layer a b _) = (zerosLike a, zerosLike b)
    zerosLike m = matrix c (replicate (r * c) 0.0)
      where
        r = rows m
        c = cols m
    (w, _, _) = _adam p iterN (w0, s0, v0) dataSet

_adam ::
  AdamParameters ->
  Int ->
  ([Layer Double], [(Matrix Double, Matrix Double)], [(Matrix Double, Matrix Double)]) ->
  (Matrix Double, Matrix Double) ->
  ([Layer Double], [(Matrix Double, Matrix Double)], [(Matrix Double, Matrix Double)])
_adam
  p@AdamParameters
    { _lr = lr,
      _beta1 = beta1,
      _beta2 = beta2,
      _epsilon = epsilon
    }
  iterN
  (w0, s0, v0)
  dataSet = last $ take iterN (iterate step (w0, s0, v0))
    where
      step (w, s, v) = (wN, sN, vN)
        where
          (_, dW) = pass w dataSet

          sN = zipWith f2 s dW
          vN = zipWith f3 v dW
          wN = zipWith3 f w vN sN

          f ::
            Layer Double ->
            (Matrix Double, Matrix Double) ->
            (Matrix Double, Matrix Double) ->
            Layer Double
          f (Layer w_ b_ sf) (vW, vB) (sW, sB) =
            Layer
              (w_ - lr `scale` vW / ((sqrt sW) `addC` epsilon))
              (b_ - lr `scale` vB / ((sqrt sB) `addC` epsilon))
              sf

          addC m c = cmap (+ c) m

          f2 ::
            (Matrix Double, Matrix Double) ->
            Gradients Double ->
            (Matrix Double, Matrix Double)
          f2 (sW, sB) (Gradients dW dB) =
            ( beta2 `scale` sW + (1 - beta2) `scale` (dW ^ 2),
              beta2 `scale` sB + (1 - beta2) `scale` (dB ^ 2)
            )

          f3 ::
            (Matrix Double, Matrix Double) ->
            Gradients Double ->
            (Matrix Double, Matrix Double)
          f3 (vW, vB) (Gradients dW dB) =
            ( beta1 `scale` vW + (1 - beta1) `scale` dW,
              beta1 `scale` vB + (1 - beta1) `scale` dB
            )

-- | Perform a binary classification
inferBinary ::
  NeuralNetwork Double -> Matrix Double -> Matrix Double
inferBinary net dta =
  let pred = forward net dta
   in -- Thresholding the NN output
      cmap (\a -> if a < 0.5 then 0 else 1) pred

-- | Generate random weights and biases
genWeights :: (Int, Int) -> IO (Matrix Double, Matrix Double)
genWeights (nin, nout) = do
  w <- _genWeights (nin, nout)
  b <- _genWeights (1, nout)
  return (w, b)
  where
    _genWeights (nin, nout) = do
      let k = sqrt (1.0 / fromIntegral nin)
      w <- randn nin nout
      return (k `scale` w)

-- | Generate a neural network with random weights
genNetwork ::
  [Int] -> [Activation] -> IO (NeuralNetwork Double)
genNetwork nodes activations = do
  weights <- mapM genWeights nodes'
  return (zipWith (\(w, b) a -> Layer w b a) weights activations)
  where
    nodes' = zip nodes (tail nodes)

-- | Binary classification accuracy in percent
accuracy ::
  -- | Neural network
  [Layer Double] ->
  -- | Dataset
  (Matrix Double, Matrix Double) ->
  Double
accuracy net (dta, tgt) = 100 * (1 - e / m)
  where
    pred = net `inferBinary` dta
    e = sumElements $ abs (tgt - pred)
    m = fromIntegral $ rows tgt