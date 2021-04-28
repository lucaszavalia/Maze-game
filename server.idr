import Control.App.Console
--import Network.Socket
import Control.Linear.Network


--Define ConsoleIO
{--data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
>>= = Do--}


--Define Sockets
{--data SocketState = Ready | Bound | Listening | Open | Closed

interface Sockets (m : Type -> Type) where
  Sock : SocketState -> Type
  --}

--Define server functions
echoServer : (Console m, Socket m) => (sock : Var) -> ST m () [remove sock (Sock {m} Listening)]
echoServer =
  do Right new <- accept sock | Left err => do close sock; remove sock
     Right msg <- recv new | Left err => do close sock; remove sock; remove new
     Right ok <- send new ("You said" ++ msg) | Left err => do remove new; close sock; remove sock
     close new; remove new; echoServer sock

startServer : (Console m, Socket m) => ST m () []
startServer =
   do Right sock <- socket Stream        | Left err => pure ()
      Right ok <- bind sock Nothing 9442 | Left err => remove sock
      Right ok <- list sock              | Left err => remove sock
      echoServer sock
