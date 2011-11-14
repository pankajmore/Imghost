module Settings
    ( widgetFile
    , PersistConfig
    ) where
import Yesod.Default.Util
import Database.Persist.Postgresql (PostgresConf)

widgetFile = Yesod.Default.Util.widgetFileProduction
type PersistConfig = PostgresConf
