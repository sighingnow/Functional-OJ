module IPv4 where
import Data.Int  (Int32)
import Data.List
import Data.Bits

type IPString = String

int32ToIP :: Int32 -> IPString
int32ToIP int32 = intercalate "." . map show . reverse . take 4 . (++ repeat 0) . decompose $ int32 where
    decompose = unfoldr helper where
        helper 0 = Nothing
        helper x = Just ((.&.) x 0xff, shiftR x 0x8)
