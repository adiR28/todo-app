-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
 Module      :  OpenTelemetry.Id.Generator
 Copyright   :  (c) Tykevision, 2023
 License     :  BSD-3
 Description :  Raw byte generation facilities for ID generation
 Maintainer  :  Tykevision
 Stability   :  experimental
 Portability :  non-portable (GHC extensions)

 Stateful random number generation interface for creating Trace and Span ID
 bytes.

 In most cases, the built-in generator in the tykeagent-sdk will be sufficient, but the
 interface is exposed for more exotic needs.
-}
module OpenTelemetry.Trace.Id.Generator (
  IdGenerator (..),
) where

import Data.ByteString (ByteString)


{- | An interface for generating the underlying bytes for
 trace and span ids.
-}
data IdGenerator = IdGenerator
  { generateSpanIdBytes :: IO ByteString
  -- ^ MUST generate exactly 8 bytes
  , generateTraceIdBytes :: IO ByteString
  -- ^ MUST generate exactly 16 bytes
  }
