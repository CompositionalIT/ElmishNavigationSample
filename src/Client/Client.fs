module Client

open Elmish
open Elmish.React
open Fable.Core
open Fable
open Elmish
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Elmish.Navigation
open Elmish.Cmd.OfPromise

open Shared

// TODO: Record for each page

type Page =
    | PageOne
    | PageTwo
    | PageThree
    | PageFour

type PageOneModel = { Text : string }

type PageTwoModel = { Text : string }

type PageThreeModel = { Text : string }

type PageFourModel = { Text : string }

type SubModel =
    | PageOneModel of PageOneModel
    | PageTwoModel of PageTwoModel

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = 
    { Counter: Counter option
      CurrentPage: Page
      SubModel: SubModel }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | NavigateTo of Page
    | Increment
    | Decrement
    | InitialCountLoaded of Counter

let initialCounter () = Fetch.fetchAs<Counter> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None; CurrentPage = PageOne; SubModel = PageOneModel { Text = "You are currently in page 1!" } }
    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd

// The update functison computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | model, InitialCountLoaded initialCount->
        let nextModel = { model with Counter = Some initialCount }
        nextModel, Cmd.none
    | model, NavigateTo page -> 
        let nextModel = { model with CurrentPage = page }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none

module Navigation = 
    open Elmish.UrlParser

    let pageParser  =
        oneOf [
            map PageOne (s "pageone")
            map PageTwo (s "pagetwo")
            map PageTwo (s "pagethree")
            map PageTwo (s "pagefour") ]

    let urlUpdate (result: Page option) model =
        match result with
        | Some page ->
            { model with CurrentPage = page }, Cmd.none
        | None ->
            failwith ("Error passing url")


let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

let show = function
    | { Counter = Some counter } -> string counter.Value
    | { Counter = None   } -> "Loading..."

let button txt onClick colour =
    Button.button
        [ Button.IsFullWidth
          Button.Color colour
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h1 [ ]
                    [ str "ALLYJAN" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [ ] [ str (sprintf "You are in %A" model.CurrentPage) ] ]
                Columns.columns []
                    [ Column.column [] [ button "PAGE 1" (fun _ -> dispatch (NavigateTo PageOne) ) IsSuccess ]
                      Column.column [] [ button "PAGE 2" (fun _ -> dispatch (NavigateTo PageTwo) ) IsPrimary ]
                      Column.column [] [ button "PAGE 3" (fun _ -> dispatch (NavigateTo PageThree) ) IsPrimary ]
                      Column.column [] [ button "PAGE 4" (fun _ -> dispatch (NavigateTo PageFour) ) IsPrimary ] ] ] ]                       

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable (UrlParser.parseHash Navigation.pageParser) Navigation.urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
