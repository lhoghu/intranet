import Dispatch ()
import Foundation
import Web.Notebook.App
import Web.YahooPortfolioManager.App
import Yesod

main :: IO ()
main = warp 3000 $ Intranet YahooPortfolioManagerSite Notebook
