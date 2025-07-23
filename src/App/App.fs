module App

open Sutil
open type Feliz.length

/// Definition of the protocol between client and server over WebSocket
/// We will expect every exchanged message to be JSON text matching this 
/// schema:

type WsMessage =

    /// Possible values (so far): 
    /// - cmd (client -> server)
    /// - log, color (server -> client)
    abstract mtype : string with get,set

    /// Value depends on mtype
    abstract mdata : obj with get,set

/// State for the web application
type Model = 
    {
        Color : string
        LogMessages : string[]
    }

/// Local messages to drive the web application state
type Message = 
    | Log of string   // Add this text to the log console
    | ReceivedWsMsg of WsMessage  // Handler for messages from server


/// Helper function for creating WsMessages
[<AutoOpen>]
module WsMessageExt =
    type WsMessage with
        static member Create( mtype : string, mdata : obj ) : WsMessage =
            unbox {|mtype = mtype; mdata = mdata |}

/// Helpers for establishing connection and dispatch messages 
/// between client and server
module WsMessage =

    open Browser.Dom
    open Browser.WebSocket

    let wsUrl = $"ws://{window.location.host}/api/ws"

    let close (ws : Browser.Types.WebSocket) =
        ws.close()

    /// Connect to server, and use supplied dispatch function to
    /// notify web application of incoming messages and to log activity
    let connect( dispatch ) =

        let log (s: string) = 
            Fable.Core.JS.console.log("WebSocket: ", s)
            dispatch (Log s)

        let ws = Browser.WebSocket.WebSocket.Create(wsUrl)

        ws.onopen <- fun e -> 
            log "WebSocket connected"

        ws.onclose <- fun e -> 
            log "WebSocket disconnected"

        ws.onerror <- fun e -> 
            log "WebSocket error"
            Fable.Core.JS.console.error("WebSocket error", e)

        ws.onmessage <- fun (e : Browser.Types.MessageEvent) ->
            // FIXME: You ought to check  e.``type`` = text

            try
                let msg : WsMessage = unbox (Fable.Core.JS.JSON.parse (string e.data))

                dispatch (Log (sprintf "Received '%A'" e.data))
                dispatch (ReceivedWsMsg msg)
            with
            | x -> 
                let s = "WebSocket error: parsing message"
                dispatch (Log s)
                Fable.Core.JS.console.error(s, x)
        ws

    /// Send message to server
    let dispatch (ws : Browser.Types.WebSocket) (msg : WsMessage) =
        ws.send( Fable.Core.JS.JSON.stringify msg )

    /// Send command to server
    let dispatchCmd (ws : Browser.Types.WebSocket) (cmd : string) =
        dispatch ws (WsMessage.Create("cmd",cmd))

/// ---------------------------------------------------------------------------
/// Manage web application state
/// 

let init() = { Color = "white"; LogMessages = [||] }, Cmd.none

let update msg model = 
    match msg with

    // This is where we turn incoming server messages (from the WebSocket) into
    // model updates that Sutil can react to.
    | ReceivedWsMsg wmsg ->
        match wmsg.mtype with
        | "log" ->
            model, Cmd.ofMsg (Log (sprintf "Server: %A" wmsg.mdata))
        | "color" ->
            { model with Color = string (wmsg.mdata) }, Cmd.none
        | _ ->
            model, Cmd.ofMsg (Log (sprintf "Unknown message: %s" wmsg.mtype))

    | Log s ->
        { model with LogMessages = [|s|] |> Array.append model.LogMessages }, Cmd.none

let view() = 

    // Initialise web application state
    let model, dispatch = 
        () |> Store.makeElmish init update ignore

    dispatch (Log "Connecting...")

    // Establish connection to server
    let ws = WsMessage.connect dispatch

    /// Helper function for sending commands to the server and logging to our console
    let sendCmd (cmd : string) =
        dispatch (Log ("Sending command: " + cmd))
        WsMessage.dispatchCmd ws cmd

    Html.div [ 

        Attr.style [
            Css.marginTop (rem 3)
            Css.displayFlex
            Css.flexDirectionColumn
            Css.alignItemsCenter
            Css.gap (rem 1)
        ]

        Html.h3 "Sutil Web Socket Demo"

        // React to activity from the server (via WebSocket)
        // In this example, we change the color of the box and show
        // the color name
        Bind.el( model .> _.Color, fun color ->
            Html.divc "colour-box" [
                Attr.style [
                    Css.displayFlex
                    Css.flexDirectionColumn
                    Css.alignItemsCenter
                    Css.justifyContentCenter
                    Css.width (px 200)
                    Css.height (px 200)
                    Css.border (px 1, Feliz.borderStyle.solid, "gray")
                    Css.backgroundColor color
                ]
                Html.h3 color
            ]
        )

        Html.divc "buttons" [
            Attr.style [
                Css.displayFlex
                Css.flexDirectionRow
                Css.gap (rem 0.5)
            ]

            // Start sending colors
            Html.button [ text "Start"; Ev.onClick (fun _ -> sendCmd "start")]

            // Stop sending colors
            Html.button [text "Stop"; Ev.onClick (fun _ -> sendCmd "stop")]
        ]

        // Log console
        Html.pre [
            Attr.className "log-window"
            Attr.style [
                Css.padding (px 3)
                Css.width (px 600)
                Css.height (px 150)
                Css.border (px 1, Feliz.borderStyle.solid, "gray")
                Css.overflowAuto
            ]
            BindArray.each( 
                model .> _.LogMessages, 
                fun m ->    Html.span [ 
                                text (m + "\n" ) 

                                // Autoscroll the console
                                Ev.onMount (fun e -> 
                                    let el = e.target :?> Browser.Types.HTMLElement
                                    if el.nextSibling = null then
                                        el.scrollIntoView()
                                )
                            ] 
            )
        ]

    ]

view() |> Program.mount