#if INTERACTIVE
[<AutoOpen>]
#endif
module Shell
open System
open System.Diagnostics
open System.Globalization
open System.Text
open System.IO


exception ProcessException of ExitCode: int * ErrorLines: string seq * FileName: string * Arguments: string with
    override e.Message =
        let nl = Environment.NewLine
        let message = String.concat nl e.ErrorLines
        $"Process terminated with exit code {e.ExitCode}. `%s{e.FileName} %s{e.Arguments}`%s{nl}message:%s{message}%s{nl}"

type StartConfig = {
    priorityClass: ProcessPriorityClass
    /// allow null
    encoding: Encoding
    onStarting: struct {| command: string |} -> unit
    onCancel: struct {| killError: exn option |} -> unit
    onFailure: unit -> unit
    onOut: string -> unit
}
let private consoleGate = obj()

module StartConfig =
    let defaultConfig<'a> = {
        priorityClass = ProcessPriorityClass.Normal
        encoding =
#if INTERACTIVE
            Console.OutputEncoding
            // Encoding.Default
#else
            CultureInfo.CurrentCulture.TextInfo.OEMCodePage
            |> CodePagesEncodingProvider.Instance.GetEncoding
#endif

        onStarting = ignore
        onCancel = ignore
        onFailure = ignore
        onOut = fun data -> lock consoleGate <| fun _ -> stdout.WriteLine data
    }

let private startProcessAsync config (command: string) = async {
    let fileName, arguments =
        let i = command.IndexOf ' '
        if i < 0 then command, ""
        else command.[0..i-1], command.[i+1..]

    let i =
        ProcessStartInfo(
            FileName = fileName,
            Arguments = arguments,
            CreateNoWindow = true,
            WindowStyle = ProcessWindowStyle.Hidden,
            ErrorDialog = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        )
    match config.encoding with
    | null -> ()
    | e -> 
        i.StandardOutputEncoding <- e
        i.StandardErrorEncoding <- e

    use p = new Process(StartInfo = i, EnableRaisingEvents = true)
    p.OutputDataReceived.Add <| fun x ->
        match x.Data with
        | null -> ()
        | data -> config.onOut data

    let errorLines = ResizeArray()
    p.ErrorDataReceived.Add <| fun x ->
        match x.Data with
        | null -> ()
        | data -> lock consoleGate <| fun _ ->
            let c = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Red
            stderr.WriteLine data
            Console.ForegroundColor <- c
            errorLines.Add data

    let exited = p.Exited
    config.onStarting {| command = command |}
    if p.Start() then
        p.PriorityClass <- config.priorityClass
        let pid = p.Id
        use! __ = Async.OnCancel <| fun _ ->
            let e =
                try Process.GetProcessById(pid).Kill(); None
                with e -> Some e
            config.onCancel {| killError = e |}

        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        do! Async.AwaitEvent exited |> Async.Ignore
        p.WaitForExit()

        if p.ExitCode <> 0 then raise <| ProcessException(p.ExitCode, errorLines, fileName, arguments)
    else
        config.onFailure()
}
let kStartWithAsync continuation withConfig command =
    Printf.ksprintf (startProcessAsync (withConfig StartConfig.defaultConfig) >> continuation) command

let startWithAsync withConfig command = kStartWithAsync id withConfig command
let startAsync command = startWithAsync id command

let startWith withConfig command = kStartWithAsync Async.RunSynchronously withConfig command
let start command = startWith id command

let runWith withConfig command =
    let lines = ResizeArray()
    let withConfig c =
        let c = withConfig c
        { c with
            onStarting = fun x -> stdout.WriteLine("> " + x.command); c.onStarting x
            onOut = fun l -> lines.Add l; c.onOut l
        }

    kStartWithAsync (fun x -> Async.RunSynchronously x; Seq.toList lines) withConfig command

let run command = runWith id command

let (/) a b = Path.Combine(a, b)

type TreeConfig = {
    depth: int
}
module TreeConfig =
    let defaultValue = {
        depth = 8
    }

let treeWith withConfig directories =
    let withIsLast (xs: _ seq) = seq {
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let mutable next = true
            while next do
                let current = e.Current
                next <- e.MoveNext()
                yield current, not next
    }

    let config = withConfig TreeConfig.defaultValue
    let rec tree depth indent namePrefix brunchPrefix current =
        let name = if depth = 0 then current else Path.GetFileName(current + "")
        printfn "%s%s%s" indent namePrefix name

        if Directory.Exists current then
            let indent = indent + brunchPrefix
            if depth < config.depth then
                let es =
                    try Directory.EnumerateFileSystemEntries(current, "*")
                    with e ->
                        printfn "%s└─× %O" indent e
                        Seq.empty

                for child, last in withIsLast es do
                    tree (depth + 1) indent (if last then "└─" else "├─") (if last then "  " else "│ ") child
            else
                printfn "%s└─…" indent

    let ds = if List.isEmpty directories then [Environment.CurrentDirectory] else directories
    for d in ds do tree 0 "" "" "" d

let tree directories = treeWith id directories
