module Client

open Fulma
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Elmish.Navigation

module HomePage =
    type Model = { Title : string }

    let init () = { Title = "Welcome! You're in the Home Page." }

    let view (model: Model) dispatch =
        div [] [
            Content.content [
                Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
            ] [
                Heading.h1 [] [ str model.Title ]
            ]
        ]

module PersonPage =
    type Model = { Name : string }

    let init fullName = { Name = fullName }

    let view model dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Person Details" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Full Name: %s" model.Name) ]
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

type Page =
    | HomePage
    | AddressPage
    | PersonPage of string

type SubModel =
    | HomePageModel of HomePage.Model
    | AddressPageModel of AddressPage.Model
    | PersonPageModel of PersonPage.Model

type Model =
    { NameEntry: string
      CurrentPage: Page
      SubModel: SubModel }

type Msg = PersonNameChanged of string

let init page : Model * Cmd<Msg> =
    printfn "IN INIT %A" page
    let page = page |> Option.defaultValue HomePage

    let subModel =
        match page with
        | HomePage -> HomePageModel (HomePage.init())
        | AddressPage -> AddressPageModel (AddressPage.init())
        | PersonPage name -> PersonPageModel (PersonPage.init name)

    { CurrentPage = page
      SubModel = subModel
      NameEntry = "" }, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | PersonNameChanged name -> { model with NameEntry = name }, Cmd.none

let button txt href options =
    Button.a
        [ Button.Color IsPrimary
          Button.Props [ Href href ]
          yield! options ]
        [ str txt ]

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

        Content.content [] [
            Columns.columns [ Columns.IsCentered ] [
                Column.column [ Column.Width (Screen.All, Column.Is3) ] [
                    Control.div [ Control.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                        br []
                        Label.label [] [ str "Name" ]
                        Input.text [
                            Input.Value model.NameEntry
                            Input.Placeholder "Enter Name Here..."
                            Input.Props [ OnChange (fun ev -> dispatch (PersonNameChanged !!ev.target?value)) ]
                        ]
                    ]
                    Control.div [ Control.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                        br []
                        let href = sprintf "#person/%s" model.NameEntry
                        button (sprintf "Set URL to '%s'" href) href [ ]
                    ]
                ]
            ]
        ]

        Container.container [ ] [
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
            s "home" |> map HomePage
            s "address" |> map AddressPage
            s "person" </> str |> map PersonPage
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
