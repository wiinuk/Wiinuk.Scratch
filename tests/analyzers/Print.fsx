open System
open Printf


type ColorModifer = {
    getColor: unit -> ConsoleColor
    setColor: ConsoleColor -> unit
    getBackgroundColor: unit -> ConsoleColor
    setBackgroundColor: ConsoleColor -> unit
}
let consoleColorModier = {
    getColor = Console.get_ForegroundColor
    setColor = Console.set_ForegroundColor
    getBackgroundColor = Console.get_BackgroundColor
    setBackgroundColor = Console.set_BackgroundColor
}
type ConsoleConfig = {
    fore: ConsoleColor
    back: ConsoleColor
    colorModifer: ColorModifer
}
let withConsoleConfig withConfig action =
    let m = consoleColorModier
    let { colorModifer = m } as config = withConfig { fore = m.getColor(); back = m.getBackgroundColor(); colorModifer = m }
    let c = m.getColor()
    let b = m.getBackgroundColor()
    m.setColor config.fore
    m.setBackgroundColor config.back
    let r = action()
    m.setColor c
    m.setBackgroundColor b
    r

let cprintfn withConfig format =
    kprintf (fun message ->
        withConsoleConfig withConfig <| fun _ -> Console.WriteLine message
    ) format

let cprintf withConfig format =
    kprintf (fun message ->
        withConsoleConfig withConfig <| fun _ -> Console.Write message
    ) format
