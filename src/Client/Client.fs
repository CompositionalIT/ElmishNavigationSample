module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Elmish.Navigation
open System
open Shared

let button txt href colour =
    Button.a
        [ Button.IsFullWidth
          Button.Color colour
          Button.Props [ Href href ] ]
        [ str txt ]

module HomePage = 
    type Model = { FirstName : string option }

    let init () = { FirstName = None }
    
    let view (model: Model) dispatch =
        div [] [
            Content.content [
                Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
            ] [ 
                Heading.h1 [ ] [ str "Welcome! You're in the Home Page. " ]
                br []; br []
                Content.content [ ] [
                    Heading.h3 [ ] [ str "Get person details by first name." ]
                    Label.label [ ] [ str "First Name:"]
                    Control.div [ ] [
                        Input.text [Input.Placeholder "Enter Here..." ]
                    ]
                    Control.div [ ] [
                        button "Submit" "#person/" IsPrimary
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
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Address" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Building No: %d" model.BuildingNo) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Street: %s" model.Street) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "City: %s" model.City) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Postcode: %s" model.Postcode) ]
        ]

module PersonPage =
    type Model =
        { FirstName : string
          Surname : string
          BirthDate : DateTime }

    let alican =
        { FirstName = "Alican"
          Surname = "Demirtas"
          BirthDate = DateTime(1998,07,27) }
    let prash =
        { FirstName = "Prashant"
          Surname = "Pathak"
          BirthDate = DateTime.UtcNow }            

    let init () =
        alican

    let view model dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [ 
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Person Details" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "First Name: %s" model.FirstName) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Surname: %s" model.Surname) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Birth date: %s" (model.BirthDate.ToShortDateString())) ]
        ]       

type Page = HomePage | AddressPage | PersonPage

type SubModel =
    | HomePageModel of HomePage.Model
    | AddressPageModel of AddressPage.Model
    | PersonPageModel of PersonPage.Model

type Model = 
    { CurrentPage: Page
      SubModel: SubModel }

type Msg =
    | NoOp

let init page : Model * Cmd<Msg> =
    let page = page |> Option.defaultValue HomePage

    let subModel =
        match page with
        | HomePage -> HomePageModel (HomePage.init())
        | AddressPage -> AddressPageModel (AddressPage.init())
        | PersonPage -> PersonPageModel (PersonPage.init())

    { CurrentPage = page
      SubModel = subModel }, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with NoOp -> model, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let getButtonColor page model =
        if model.CurrentPage = page then
            IsSuccess
        else IsPrimary

    div [] [ 
        Navbar.navbar [ Navbar.Color IsPrimary ] [ 
            Navbar.Item.div [ ] [ 
                Heading.h1 [ ] [ str "Elmish navigation example" ] 
            ] 
        ]

        Container.container [] [
            Columns.columns [ Columns.Option.Props [ Style  [ Margin "3em" ] ] ] [ 
                Column.column [] [ button "HOME PAGE" "#home" (model |> getButtonColor HomePage) ]
                Column.column [] [ button "ADDRESS PAGE" "#address" (model |> getButtonColor AddressPage) ]
                // Column.column [] [ button "PERSON PAGE" "#person" (model |> getButtonColor PersonPage) ]
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
            map PersonPage (s "person") 
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
