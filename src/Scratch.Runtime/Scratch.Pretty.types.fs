namespace Scratch
open Scratch.Primitives


[<NoEquality; NoComparison>]
type PrettyState<'S> = private {
    scope: 'S PrettyState option
    listOrVariables: string Set
    procedures: string Set
    parameters: string Set
    pluginState: 'S
}

type private Printer<'s,'a> = (struct('s PrettyState * 'a) -> Document)
type private Names<'s,'a> = (struct('s * 'a seq) -> string seq)
[<NoEquality; NoComparison>]
type PrettyPlugin<'State,'Script,'Variable,'List> = {
    prettyScript: Printer<'State,'Script>
    prettyVariable: Printer<'State,'Variable>
    prettyList: Printer<'State,'List>

    procedureNames: Names<'State,'Script>
    listNames: Names<'State,'List>
    variableNames: Names<'State,'Variable>

    makeState: unit -> 'State
}
[<Struct; NoEquality; NoComparison>]
type internal PrettyStateWithPlugin<'State,'Script,'Variable,'List> = {
    state: PrettyState<'State>
    plugins: PrettyPlugin<'State,'Script,'Variable,'List>
}
