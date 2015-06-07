-- Copyright(c) 2012-2015, ETH Zurich. All rights reserved.
--
-- Released under a dual BSD 3-clause/GPL 2 license. When using or
-- redistributing this file, you may do so under either license.
--
-- See LICENCE.Dragonet for details.

module MachineDetails (
    burrataIP,
    gruyereIP,
    ziger2IP,
    sbrinz2IP,
    asiagoIP_E10K,
    asiagoIP_SF
) where


import Data.Word

-------------------------------------------
-- Move this to single file
burrataIP :: Word32
burrataIP =   175178848

gruyereIP :: Word32
gruyereIP =  175178772

ziger2IP :: Word32
ziger2IP =   175178809

sbrinz2IP :: Word32
sbrinz2IP =   175178781

asiagoIP_E10K :: Word32
asiagoIP_E10K = 175178847  -- Intel

asiagoIP_SF :: Word32
asiagoIP_SF =  175178947  -- solarflare


{-
# 10.113.4.96  # Burrata # 175178848
# 10.113.4.20  # gruyere # 175178772
# 10.113.4.57  # ziger2  # 175178809
# 10.113.4.29  # sbrinz2 # 175178781
# 10.113.4.95  # Asiago  # 175178847  # Intel
# 10.113.4.195 # Asiago  # 175178947  # solarflare
-}



