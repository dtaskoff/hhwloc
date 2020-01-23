-----------------------------------------------------------------------------
-- |
-- Module      :  System.Libhwloc
-- Copyright   :  (c) Daniel Taskoff, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- Bindings to https://www.open-mpi.org/projects/hwloc
--
-- Currently implemented:
--
-- * initialising and loading a 'Topology'
-----------------------------------------------------------------------------

module System.Libhwloc
  (  -- * 'Topology'
    Topology, initialise, load
  ) where

import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)


-- | A topology context.
data Topology = Topology (ForeignPtr ())

-- | Allocate a topology context.
initialise :: IO (Maybe Topology)
initialise =
  alloca \ptr_ptr ->
    c_hwloc_topology_init ptr_ptr >>= \case
      -1 -> pure Nothing
      _ -> Just . Topology <$> do
        newForeignPtr c_hwloc_topology_destroy =<< peek ptr_ptr

foreign import ccall "hwloc_topology_init"
  c_hwloc_topology_init :: Ptr (Ptr hwloc_topology) -> IO CInt

foreign import ccall "&hwloc_topology_destroy"
  c_hwloc_topology_destroy :: FinalizerPtr hwloc_topology

-- | Load a topology. The return value is 'True' if the loading was successful, otherwise 'False'.
load :: Topology -> IO Bool
load (Topology ptr) = (== 0) <$> withForeignPtr ptr c_hwloc_topology_load

foreign import ccall "hwloc_topology_load"
  c_hwloc_topology_load :: Ptr hwloc_topology -> IO CInt
