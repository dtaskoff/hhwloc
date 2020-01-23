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
-- * getting the depth of an object type
-- * getting the number of objects of an object type, or at a given depth
-----------------------------------------------------------------------------

module System.Libhwloc
  ( -- * Topology
    Topology, initialise, load
    -- * Object types
  , ObjectType(..), getTypeDepth, getNumberOfObjectsByDepth, getNumberOfObjectsByType
  ) where

import Foreign.C.Types (CInt (..), CUInt(..))
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

-- | A type of a topology object.
data ObjectType =
    Machine
  | Package
  | Core
  | PU
  | L1Cache
  | L2Cache
  | L3Cache
  | L4Cache
  | L5Cache
  | L1ICache
  | L2ICache
  | L3ICache
  | Group
  | NUMANode
  | Bridge
  | PCIDevice
  | OSDevice
  | Miscellaneous
  | MemoryCache
  | Die

-- | Get the depth of the objects of a given type.
getTypeDepth :: Topology -> ObjectType -> IO Int
getTypeDepth (Topology ptr) objectType =
  fromIntegral <$> withForeignPtr ptr (`c_hwloc_get_type_depth` toCInt objectType)

foreign import ccall "hwloc_get_type_depth"
  c_hwloc_get_type_depth :: Ptr hwloc_topology -> CInt -> IO CInt

#include "hwloc.h"

toCInt :: ObjectType -> CInt
toCInt = \case
  Machine -> #{const HWLOC_OBJ_MACHINE}
  Package -> #{const HWLOC_OBJ_PACKAGE}
  Core -> #{const HWLOC_OBJ_CORE}
  PU -> #{const HWLOC_OBJ_PU}
  L1Cache -> #{const HWLOC_OBJ_L1CACHE}
  L2Cache -> #{const HWLOC_OBJ_L2CACHE}
  L3Cache -> #{const HWLOC_OBJ_L3CACHE}
  L4Cache -> #{const HWLOC_OBJ_L4CACHE}
  L5Cache -> #{const HWLOC_OBJ_L5CACHE}
  L1ICache -> #{const HWLOC_OBJ_L1ICACHE}
  L2ICache -> #{const HWLOC_OBJ_L2ICACHE}
  L3ICache -> #{const HWLOC_OBJ_L3ICACHE}
  Group -> #{const HWLOC_OBJ_GROUP}
  NUMANode -> #{const HWLOC_OBJ_NUMANODE}
  Bridge -> #{const HWLOC_OBJ_BRIDGE}
  PCIDevice -> #{const HWLOC_OBJ_PCI_DEVICE}
  OSDevice -> #{const HWLOC_OBJ_OS_DEVICE}
  Miscellaneous -> #{const HWLOC_OBJ_MISC}
  MemoryCache -> #{const HWLOC_OBJ_MEMCACHE}
  Die -> #{const HWLOC_OBJ_DIE}

-- | Get the number of objects at a given depth.
getNumberOfObjectsByDepth :: Topology -> Int -> IO Word
getNumberOfObjectsByDepth (Topology ptr) depth =
  fromIntegral <$>
    withForeignPtr ptr (`c_hwloc_get_nbobjs_by_depth` fromIntegral depth)

foreign import ccall "hwloc_get_nbobjs_by_depth"
  c_hwloc_get_nbobjs_by_depth :: Ptr hwloc_topology -> CInt -> IO CUInt

-- | Get the number of objects of a given type.
getNumberOfObjectsByType :: Topology -> ObjectType -> IO Word
getNumberOfObjectsByType topology objectType =
  getNumberOfObjectsByDepth topology =<< getTypeDepth topology objectType
