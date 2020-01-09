module Client

open Fulma
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fable.Core
open Elmish.Navigation
open Browser
open Browser.Types
open System
open Shared

let button txt href options =
    Button.a
        [ Button.Color IsPrimary
          Button.Props [ Href href ]
          yield! options ]
        [ str txt ]

type Msg = PersonNameChanged of string
    
module PersonPage =
    type Model =
        { Name : string }

    let init fullName =
        { Name = fullName }

    let view model dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [ 
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Person Details" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Full Name: %s" model.Name) ]
        ]       

let onKeyDown keyCode action =
    OnKeyDown (fun ev ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev )

module HomePage = 
    type Model = { Name : string }

    let init () = { Name = "" }

    let view (model: Model) dispatch =
        div [] [
            Content.content [
                Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
            ] [ 
                Heading.h1 [] [ str "Welcome! You're in the Home Page." ]
                Content.content [] [
                    Heading.h3 [] [ str "Enter Name to Navigate to Person Page" ]
                    Label.label [] [ str "Name" ]
                    Columns.columns [ Columns.IsCentered ] [
                        Column.column [ Column.Width (Screen.All, Column.Is3) ] [
                            Control.div [] [
                                Input.text [
                                    Input.Value model.Name
                                    Input.Placeholder "Enter Name Here..."
                                    Input.Props [ OnChange (fun ev -> dispatch (PersonNameChanged !!ev.target?value)) ]
                                ]
                            ]
                            Control.div [ Control.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                                let href = sprintf "#person/%s" model.Name
                                button (sprintf "Set URL to '%s'" href) href [ ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

module AddressPage =
    type Model = 
        { BuildingNo : int
          Street : string
          City : string 
          Postcode : string }

    let init () =
      { BuildingNo = 41
        City = "London"
        Postcode = "P21 1XX"
        Street = "Liverpool St" }

    let view model dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [ 
            Heading.h1 [ Heading.Props [ Style [ Margin "2rem" ] ] ] [ str "Address" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Building No: %d" model.BuildingNo) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Street: %s" model.Street) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "City: %s" model.City) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Postcode: %s" model.Postcode) ]
        ]

type Page = HomePage | AddressPage | PersonPage of string

type SubModel =
    | HomePageModel of HomePage.Model
    | AddressPageModel of AddressPage.Model
    | PersonPageModel of PersonPage.Model

type Model = 
    { CurrentPage: Page
      SubModel: SubModel }

let init page : Model * Cmd<Msg> =
    let page = page |> Option.defaultValue HomePage

    let subModel =
        match page with
        | HomePage -> HomePageModel (HomePage.init())
        | AddressPage -> AddressPageModel (AddressPage.init())
        | PersonPage name -> PersonPageModel (name |> PersonPage.init)

    { CurrentPage = page
      SubModel = subModel }, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | PersonNameChanged name -> { model with SubModel = HomePageModel { Name = name } }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let navigationButton text href page model =
        button text href [
            Button.IsFullWidth
            if model.CurrentPage = page then Button.Color IsSuccess
        ]

    div [] [ 
        Navbar.navbar [ Navbar.Color IsPrimary ] [ 
            Navbar.Item.div [ ] [ 
                Heading.h1 [ ] [ str "Elmish navigation example" ] 
            ] 
        ]

        Container.container [] [
            Columns.columns [ Columns.Option.Props [ Style  [ Margin "3em" ] ] ] [ 
                Column.column [] [ navigationButton "HOME PAGE" "#home" HomePage model ]
                Column.column [] [ navigationButton "ADDRESS PAGE" "#address" AddressPage model ]
            ]
            Columns.columns [] [ 
                Column.column [] [ 
                    match model.SubModel with
                    | HomePageModel m -> HomePage.view m dispatch 
                    | AddressPageModel m -> AddressPage.view m dispatch
                    | PersonPageModel m -> PersonPage.view m dispatch
                ]
            ]
        ] 
    ]                       

module Navigation = 
    open Elmish.UrlParser

    let pageParser : Parser<_,_> =
        oneOf [
            map HomePage (s "home")
            map AddressPage (s "address")
            map PersonPage (s "person" </> str)
        ]

    let urlUpdate (page: Page option) _ =
        let page = page |> Option.defaultValue HomePage
        
        let model, _ = Some page |> init

        { model with 
            CurrentPage = page; 
            SubModel = model.SubModel }, Cmd.none

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
