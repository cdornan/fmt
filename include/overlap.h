#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#  define _OVERLAPPING_
#  define _OVERLAPPABLE_
#  define _OVERLAPS_
#else
#  define _OVERLAPPING_ {-# OVERLAPPING #-}
#  define _OVERLAPPABLE_ {-# OVERLAPPABLE #-}
#  define _OVERLAPS_ {-# OVERLAPS #-}
#endif
