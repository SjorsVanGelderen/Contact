(*
   Basic peer-to-peer networking example using Lidgren.Network
   Uses a simple 'mailbox' system
*)

open System.Net
open Lidgren.Network

//Mode of operation
type Mode =
    | Server = 0
    | Client = 1

//Enumeration for message types
type MessageType =
    | PeerInformation = 1
    | Text            = 2
    
type Incoming = NetIncomingMessageType

[<EntryPoint>]
let main args =
    //Specify the mode of operation
    let rec getMode () =
        printfn "0 - Server, 1 - Client"
        match System.Console.ReadLine () with
        | "0" ->
            printfn "Starting as server"
            Mode.Server
        | "1" ->
            printfn  "Starting as client"
            Mode.Client
        | _   ->
            printfn "Invalid input!"
            getMode ()
    
    let mode = getMode ()

    //This application's port number
    let listenPort =
        match mode with
        | Mode.Server -> 10000
        | Mode.Client -> 10001
        | _           -> failwith "Invalid mode!"
    
    let clientPort =
        match mode with
        | Mode.Server -> 10001
        | Mode.Client -> 10000
        | _           -> failwith "Invalid mode!"
    
    //Generate a random nickname
    let random = System.Random ()
    let nickname =
        match args |> List.ofArray with
        | x :: xs -> x
        | []      -> "Unnamed_" + (hash (random.Next ())).ToString ()
    
    printfn "Nickname set to %s" nickname
    
    //Set up network configuration
    let config = new NetPeerConfiguration "Networking Example"
    do config.Port                      <- listenPort
    do config.MaximumConnections        <- 128
    do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //NetUtility.Resolve("localhost")
    do config.AcceptIncomingConnections <- true
    
    let messageTypes =
        [ Incoming.DiscoveryRequest
          Incoming.DiscoveryResponse
          Incoming.ConnectionApproval
          Incoming.StatusChanged
          Incoming.UnconnectedData
          Incoming.Data
          Incoming.VerboseDebugMessage
          Incoming.DebugMessage
          Incoming.WarningMessage
          Incoming.ErrorMessage        ]
    
    let rec enableMessageTypes list =
        match list with
        | x :: xs ->
            do config.EnableMessageType x
            enableMessageTypes xs
        | []      -> ()
    
    enableMessageTypes messageTypes

    printfn "Starting peer"
    let peer = new NetPeer (config)
    peer.Start ()

    //Look for peers
    peer.DiscoverKnownPeer ("localhost", clientPort) |> ignore
    peer.DiscoverLocalPeers (clientPort)

    let handleDebugMessage (message : NetIncomingMessage) =
        printfn "Debug: %s" (message.ReadString ())
    
    let sendPeerInfo (ip : IPAddress) (port : int) =
        let peerMessage  = peer.CreateMessage ()
        peerMessage.Write ((int)MessageType.PeerInformation)
        
        let ipBytes = ip.GetAddressBytes ()
        peerMessage.Write ((int)MessageType.PeerInformation)
        peerMessage.Write (ipBytes.Length)
        peerMessage.Write (ipBytes)
        peerMessage.Write (port)

        if peer.Connections.Count > 0 then
            peer.SendMessage (peerMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
        else
            printfn "Couldn't send peer info, no connections!"
    
    //The main send and receive loop
    let rec loop times =
        if times > 0 then
            //Process received messages
            let rec processIncomingMessages () =
                let message = peer.ReadMessage ()
                match message with
                | null -> ()
                | _    ->
                    //printfn "Message type: %A" message.MessageType
                    match message.MessageType with
                    | Incoming.VerboseDebugMessage
                    | Incoming.DebugMessage
                    | Incoming.WarningMessage
                    | Incoming.ErrorMessage        -> handleDebugMessage message
                    | Incoming.DiscoveryRequest    ->
                        peer.SendDiscoveryResponse (null, message.SenderEndPoint)
                        
                    | Incoming.DiscoveryResponse   ->
                        peer.Connect (message.SenderEndPoint) |> ignore
                        
                    | Incoming.ConnectionApproval  ->
                        message.SenderConnection.Approve ()
                        printfn "Sending peer info"
                        sendPeerInfo message.SenderEndPoint.Address message.SenderEndPoint.Port
                        
                    | Incoming.StatusChanged       ->
                        let id     = message.SenderConnection.RemoteUniqueIdentifier.ToString ()
                        let status = enum<NetConnectionStatus> (message.ReadInt32 ()) //(message.ReadByte ())
                        if status = NetConnectionStatus.Connected then
                            let reason = message.SenderConnection.RemoteHailMessage.ReadString ()
                            printfn "%s reports: %A - %s" id status reason
                        
                    | Incoming.UnconnectedData    ->
                        printfn "Unconnected data: %s" (message.ReadString ())
                        
                    | Incoming.Data               ->
                        printfn "Incoming data!"
                        let messageType = message.ReadInt32 ()
                        match (enum<MessageType> messageType) with
                        | MessageType.PeerInformation ->
                            let byteLength = message.ReadInt32 ()
                            let ip         = new IPAddress (message.ReadBytes (byteLength))
                            let port       = message.ReadInt32 ()
                            let endPoint   = new IPEndPoint (ip, port)
                            
                            match peer.GetConnection (endPoint) with
                            | null ->
                                let localHash  = peer.Configuration.LocalAddress.GetHashCode ()
                                let localPort  = peer.Configuration.Port.GetHashCode ()
                                let remoteHash = endPoint.Address.GetHashCode ()
                                let remotePort = endPoint.Port.GetHashCode ()
                                if  (localHash <> remoteHash) || (localPort <> remotePort)  then
                                    printfn "Initiating new connection to %s:%s"
                                        (endPoint.Address.ToString()) (endPoint.Port.ToString ())
                                    peer.Connect (endPoint) |> ignore
                            | _    -> ()
                        | MessageType.Text         ->
                            printfn "Received message: %s" (message.ReadString ())
                        | _                           -> printfn "Unhandled message type: %A!" messageType
                    | _                            ->
                        printfn "Unhandled message type: %A! %s" message.MessageType (message.ReadString ())
                    
                    peer.Recycle message
                    processIncomingMessages ()
            
            processIncomingMessages ()

            match peer.Connections with
            | null -> ()
            | _    ->
                if peer.Connections.Count > 0 then
                    //Send a message
                    let reportMessage = peer.CreateMessage ()
                    reportMessage.Write ((int)MessageType.Text)
                    reportMessage.Write (nickname + " reporting in!")
                    peer.SendMessage (reportMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
            
            System.Threading.Thread.Sleep 100 //Pause for 0.1 seconds
            loop (times - 1)
        
    loop 600

    //Stop the server and exit the program
    peer.Shutdown ("Finished program")
    printfn "Finished networking sample"
    0
