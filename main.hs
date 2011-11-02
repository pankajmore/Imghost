import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withImgHost)

main :: IO ()
main = defaultMain fromArgs withImgHost

