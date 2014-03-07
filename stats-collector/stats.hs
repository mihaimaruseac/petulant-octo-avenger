{-
 - Extract statistics about the Artemis universe of Pardus.
 -}
import Network.HTTP.Conduit

main = simpleHttp "http://artemis.pardus.at/statistics.php?display=onlinelist" >>= print
