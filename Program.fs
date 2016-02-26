(*
   Basic peer-to-peer networking example using Lidgren.Network
   Uses a simple 'mailbox' system
*)

open System.Net
open Lidgren.Network

//Mode of operation
type Mode =
    | Create = 0
    | Join   = 1

//Enumeration for message types
type MessageType =
    | PeerInformation = 1
    | Text            = 2
    
type Incoming = NetIncomingMessageType

[<EntryPoint>]
let main args =
    //Specify the mode of operation
    let rec getMode () =
        printfn "0 - Create, 1 - Join"
        match System.Console.ReadLine () with
        | "0" ->
            printfn "Creating network"
            Mode.Create
        | "1" ->
            printfn  "Joining network"
            Mode.Join
        | _   ->
            printfn "Invalid input!"
            getMode ()
    
    let mode = getMode ()
    
    //This application's port number
    let localPort =
        printfn "Type a local port number to use"
        System.Int32.Parse(System.Console.ReadLine ())

    printfn "Using local port: %A" localPort
    
    //The client's port number
    let remotePort =
        printfn "Type a remote port number to use"
        System.Int32.Parse(System.Console.ReadLine ())

    printfn "Using remote port: %A" remotePort
    
    //Generate a random nickname
    let random = System.Random ()
    let nickname =
        match args |> List.ofArray with
        | x :: xs -> x
        | []      -> "Unnamed_" + (hash (random.Next ())).ToString ()
    
    printfn "Nickname set to %s" nickname
    
    //Set up network configuration
    let config = new NetPeerConfiguration "Networking Example"
    do config.Port                      <- localPort
    do config.MaximumConnections        <- 128
    do config.LocalAddress              <- new IPAddress ((int64)0x0100007f) //The hex value for 127.0.0.1(localhost) //NetUtility.Resolve("localhost")
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
    peer.DiscoverKnownPeer ("localhost", remotePort) |> ignore
    peer.DiscoverLocalPeers (remotePort)

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
            let rec processIncomingMessages mailbox =
                let message = peer.ReadMessage ()
                let mail =
                    match message with
                    | null -> None        
                    | _    ->
                        match message.MessageType with
                        | Incoming.VerboseDebugMessage
                        | Incoming.DebugMessage
                        | Incoming.WarningMessage
                        | Incoming.ErrorMessage        ->
                            handleDebugMessage message
                            None
                            
                        | Incoming.DiscoveryRequest    ->
                            peer.SendDiscoveryResponse (null, message.SenderEndPoint)
                            None
                        
                        | Incoming.DiscoveryResponse   ->
                            peer.Connect (message.SenderEndPoint) |> ignore
                            None
                        
                        | Incoming.ConnectionApproval  ->
                            message.SenderConnection.Approve ()
                            printfn "Sending peer info"
                            sendPeerInfo message.SenderEndPoint.Address message.SenderEndPoint.Port
                            None
                        
                        | Incoming.StatusChanged       ->
                            let id     = message.SenderConnection.RemoteUniqueIdentifier.ToString ()
                            let status = enum<NetConnectionStatus> (message.ReadInt32 ()) //(message.ReadByte ())
                            if status = NetConnectionStatus.Connected then
                                let reason = message.SenderConnection.RemoteHailMessage.ReadString ()
                                printfn "%s reports: %A - %s" id status reason
                            None
                        
                        | Incoming.UnconnectedData    ->
                            printfn "Unconnected data: %s" (message.ReadString ())
                            None

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
                                None
                            | MessageType.Text         ->
                                Some (message.ReadString ()) //Add message to the mailbox
                            | _                           ->
                                printfn "Unhandled message type: %A!" messageType
                                None
                                
                        | _                            ->
                            printfn "Unhandled message type: %A! %s" message.MessageType (message.ReadString ())
                            None
                
                if message <> null then
                    peer.Recycle message
                    match mail with
                    | Some m -> processIncomingMessages (mail :: mailbox)
                    | None   -> processIncomingMessages mailbox
                else
                    mailbox
            
            let mailbox = (processIncomingMessages [])
            printfn "Mailbox:\n %A" mailbox

            match peer.Connections with
            | null -> ()
            | _    ->
                if peer.Connections.Count > 0 then
                    //Send a message
                    let reportMessage = peer.CreateMessage ()
                    reportMessage.Write ((int)MessageType.Text)
                    reportMessage.Write (nickname + " says: I'll take that for " + ((random.Next ()).ToString ()) + " dollars!")
                    peer.SendMessage (reportMessage, peer.Connections, NetDeliveryMethod.ReliableOrdered, 0)
            
            System.Threading.Thread.Sleep 100 //Pause for 0.1 seconds
            loop (times - 1)
        
    loop 600 //Loop for one minute

    //Stop the create and exit the program
    peer.Shutdown ("Finished program")
    printfn "Finished networking sample"
    0
