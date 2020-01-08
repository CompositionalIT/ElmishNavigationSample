module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Elmish.Navigation
open System
open Shared

module HomePage = 
    type Model = { Title : string }

    let init () = { Title = "Welcome! You're in HOME PAGE. " }
    
    let view (model: Model) dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [ 
            Heading.h3 [ ] [ str model.Title ]
        ] 

module PageOne =
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
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Address Page" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Building No: %d" model.BuildingNo) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Street: %s" model.Street) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "City: %s" model.City) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Postcode: %s" model.Postcode) ]
        ]

module PageTwo =
    type Model =
        { FirstName : string
          Surname : string
          BirthDate : DateTime }

    let init () =
        { FirstName = "Alican"
          Surname = "Demirtas"
          BirthDate = DateTime(1998,07,27) }

    let view model dispatch =
        Content.content [
            Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
        ] [ 
            Heading.h1 [ Heading.Option.Props [ Style [ Margin "2rem" ] ] ] [ str "Person Page" ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "First Name: %s" model.FirstName) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Surname: %s" model.Surname) ]
            Heading.h3 [ Heading.IsSubtitle ] [ str (sprintf "Birth date: %s" (model.BirthDate.ToShortDateString())) ]
        ]       

type Page = HomePage | PageOne | PageTwo

type SubModel =
    | HomePageModel of HomePage.Model
    | PageOneModel of PageOne.Model
    | PageTwoModel of PageTwo.Model

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
        | PageOne -> PageOneModel (PageOne.init())
        | PageTwo -> PageTwoModel (PageTwo.init())

    { CurrentPage = page
      SubModel = subModel }, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with NoOp -> model, Cmd.none

let button txt href colour =
    Button.a
        [ Button.IsFullWidth
          Button.Color colour
          Button.Props [ Href href ] ]
        [ str txt ]

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
                Column.column [] [ button "HOME PAGE" "#homepage" (model |> getButtonColor HomePage) ]
                Column.column [] [ button "PAGE ONE" "#pageone" (model |> getButtonColor PageOne) ]
                Column.column [] [ button "PAGE TWO" "#pagetwo" (model |> getButtonColor PageTwo) ]
            ]
            Columns.columns [] [ 
                Column.column [] [ 
                    match model.SubModel with
                    | HomePageModel m -> HomePage.view m dispatch 
                    | PageOneModel m -> PageOne.view m dispatch
                    | PageTwoModel m -> PageTwo.view m dispatch
                ]
            ]
        ] 
    ]                       

module Navigation = 
    open Elmish.UrlParser

    let pageParser : Parser<_,_> =
        oneOf [
            map HomePage (s "homepage")
            map PageOne (s "pageone")
            map PageTwo (s "pagetwo") 
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
