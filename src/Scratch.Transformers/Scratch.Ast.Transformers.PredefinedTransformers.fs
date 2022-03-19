module Scratch.Ast.Transformers.PredefinedTransformers
open Scratch
open Scratch.Ast
open Scratch.Ast.Transformers.PredefinedTransformer
open Scratch.Primitives
open Scratch.Reflection
open Scratch.Transformers
module R = Scratch.Transformers.TransformResult


// `1 + 2` (op は純粋, operand は定数)
// => `3`
let constantFolding expression _ =
    match expression with
    | Complex(ComplexExpression(operator = operator; operands = operands))
        when
            operatorCost operator = Pure &&
            operands |> List.forall (function Literal _ -> true | _ -> false) ->

        match evaluateExpressionWithGlobalContext expression with
        | Error _ -> R.noModify expression
        | Ok c -> Literal(Expression.state expression, c) |> R.modified

    | _ -> R.noModify expression

// `x <- $e; $es... ($e は定数様, x は局所単一代入); x + 2`
// => `$es...; x + 2`
let constantPropagation expressions env =
    match expressions with
    | SetVar_To_(varName, value)::es when isLocalSingleAssignment varName env ->
        match maxCost value with
        | LiteralLike ->
            let es = replaceVar varName value es

            // 代入式を削除
            es |> R.mapIfModified expressions id

        | _ -> R.noModify expressions
    | _ -> R.noModify expressions

// `x <- $e; $es... ($e は純粋, x は局所単一代入, x の読み取り回数 = 1); x + 2`
// => `$es...; $e + 2`
let purePropagation expressions env =
    match expressions with
    | SetVar_To_(varName, value)::es when isLocalSingleAssignment varName env ->
        match maxCost value with

        | Pure when List.sumBy (ComplexExpression.variableCount varName) es <= 1 ->
            let es = replaceVar varName value es

            // 代入式を削除
            es |> R.mapIfModified expressions id
        
        | _ -> R.noModify expressions
    | _ -> R.noModify expressions

// `x <- y; $es... (y は変数, x は局所単一代入, $es 内で y の変更なし); x + 2`
// => `$es...; y + 2`
let copyPropagation expressions env =
    match expressions with
    | SetVar_To_(varName, Complex(ReadVariable _) & value) as e::es
        when isLocalSingleAssignment varName env  ->
        let es = replaceVarToVarIfNotAssigned varName value es

        if R.isSkip es then

            // 代入が発見された ( isSkip = true ) ので、代入式を削除しない
            es
            |> R.withIsSkip false
            |> R.mapIfModified expressions (fun es -> e::es)
        else
            // 代入式を削除
            es |> R.mapIfModified expressions id

    | _ -> R.noModify expressions

// `x <- y; $es... (y は変数を除き純粋な式, yvs は y 内の変数, x は局所単一代入, $es 内で yvs の変更なし, x の読み取り回数 = 1); x + 2`
// => `$es...; y + 2`
let copyPurePropagation expressions env =
    match expressions with
    | SetVar_To_(varName, value) as e::es
        when
            isLocalSingleAssignment varName env &&
            maxCostWithoutReadVariable value <= Pure &&
            List.sumBy (ComplexExpression.variableCount varName) es <= 1 ->

        let es = replaceVarToVarIfNotAssigned varName value es
        if R.isSkip es then
            es
            |> R.withIsSkip false
            |> R.mapIfModified expressions (fun es -> e::es)
        else
            es |> R.mapIfModified expressions id

    | _ -> R.noModify expressions

[<Struct>]
type InlineExpansionConfig = {
    maxProcedureSize: int
}
let private defaultInlineExpansionConfig = { maxProcedureSize = 8 }

// `proc a b = a + b`, `proc x 20 (現在の Atomicity と proc の Atomicity が等しい, proc は自己呼び出しを含まない, (proc は指定サイズ以下))`
// => `x + 20`
let inlineExpansion withConfig =
    let { maxProcedureSize = maxSize } = withConfig defaultInlineExpansionConfig
    fun self env ->

    let (EntityEnvs.Entity entityEnv & EntityEnvs.Config config) = env
    let locals = ref Map.empty
    let config =
        { config with
            maxIterationCount = 1
            plugins =
            { Plugins.empty with
                transformComplexExpressions = inlineExpansionAtExpressions maxSize locals
            }
        }
    let memberEnv = { self = self }
    let env = config^^HUnit |> EntityEnvs.ofConfigs entityEnv |> MemberEnvs.ofEntityEnvs memberEnv
    Transformer.transformEntityScripts self.scripts env
    |> R.mapIfModified self (fun scripts ->
        let variables =
            if Map.isEmpty locals.contents then self.variables else

            let localVars = locals.contents |> Seq.map (fun kv -> kv.Value) |> Seq.toList
            self.variables @ localVars

        { self with
            scripts = scripts
            variables = variables
        }
    )

let joinWhenGreenFlags self _ =
    match joinWhenGreenFlags self with
    | ValueNone -> R.noModify self
    | ValueSome scripts -> R.modified { self with scripts = scripts }

let eliminateTopLevelExpression s _ =
    match s with
    | Expression _
    | Statements _ when not <| hasExportTag (Script.state s) -> R.removed s
    | _ -> R.noModify s

// ルート ( Listener, Expression, Statements, ExportTag が付いた Procedure ) から到達できないプロシージャを取り除く
// ルートから到達できないなら、相互再帰しているプロシージャも取り除く
let eliminateDeadProcedures self _ =
    let aliveSet =
        self.scripts
        |> List.fold (fun aliveSet { script = s } ->
            if isRootScript s then collectAliveProcedureReferences aliveSet self s
            else aliveSet
        ) Set.empty

    self.scripts
    |> R.mapList (fun s ->
        match s.script with
        | Procedure(ProcedureDefinition(name = name)) when Set.contains name aliveSet |> not -> R.removed s
        | _ -> R.noModify s
    )
    |> R.mapIfModified self (fun scripts -> { self with scripts = scripts })

let eliminateUnusedVariables self (EntityEnvs.Entity { EntityEnv.children = children }) =
    self.variables
    |> R.mapList (fun v ->
        if isExportVarData v || hasReferenceAtSelf v.name self || hasReferenceAtChildren v.name children then
            R.noModify v
        else
            R.removed v
    )
    |> R.mapIfModified self (fun variables -> { self with variables = variables })

// `x <- $e` ( x はエクスポートされていない, x は全てのスクリプトで読み取られていない, $e は観測されない )
// => ``
let eliminateDeadSetExpression e env =
    match e with
    | SetVar_To_(varName, value)::xs ->
        let BlockEnvs.Entity { parent = parent; children = children } & BlockEnvs.Member { self = self } = env
        let varDef, isDefinedAtSelf = resolveVariable varName env

        if Option.exists isExportVarData varDef then R.noModify e else

        match parent with

        // self で定義されていないときは parent を探す
        | Some parent when not isDefinedAtSelf && hasVariableReadAtSelf varName parent -> R.noModify e

        // self を探す
        | _ when hasVariableReadAtSelf varName self -> R.noModify e

        // children を探す
        | _ when hasVariableReadAtChildren varName children -> R.noModify e

        // NOTE: 右辺は、現在知られている全ての式で削除を観測できない
        | _ when false && HasSideEffect <= maxCost value -> R.noModify e

        // 変数の読み取りはなかったので代入式を削除
        | _ -> R.modified xs

    | _ -> R.noModify e

let eliminateSelfSetExpression es _ =
    match es with
    | SetVar_To_(varName, Complex(ReadVariable varName'))::xs when varName = varName' -> R.modified xs
    | _ -> R.noModify es

let eliminateDeadConditionExpression es env =
    match es with
    | [] -> R.noModify es
    | ComplexExpression(operator = operator; operands = operands)as e::es as ees ->
        let (BlockEnvs.Procedure { procedureScript = script }) = env
        let atomicity = Script.atomicity script

        match operator, operands with

        | O.doIf, [Literal(value = test); Block ifTrue] ->

            if SValue.toBool test

            // `doIf true { ...ifTrue }` => `..ifTrue`
            then R.modified (BlockExpression.body ifTrue @ es)

            // `doIf false { ...ifTrue }` => ``
            else R.modified es

        | O.doIfElse, [Literal(value = test); Block ifTrue; Block ifFalse] ->

            if SValue.toBool test

            // `doIfElse true { ...ifTrue } { ... }` => `...ifTrue`
            then R.modified (BlockExpression.body ifTrue @ es)

            // `doIfElse false { ... } { ...ifFalse }` => `...ifFalse`
            else R.modified (BlockExpression.body ifFalse @ es)

        //| O.doWaitUntil

        //| O.doForeverIf
        //| O.doWhile

        | O.doRepeat, [Literal(value = test); Block body] ->
            match atomicity with
            | NoAtomic -> R.noModify ees
            | Atomic ->

            let count = SValue.toNumber test

            // `doRepeat $n \{ ... }` ( atomic, $n <= 0 ) => ``
            if count <= 0. then R.modified es

            // `doRepeat 1 \{ ...body }` ( atomic ) => `...body`
            elif count = 1. then R.modified (BlockExpression.body body @ es)

            else R.noModify ees

        //| O.doReturn
        //| O.doForever

        | O.doUntil, [Literal(value = test); Block(BlockExpression(s, ifFalse))] ->
            match atomicity with
            | NoAtomic -> R.noModify ees
            | Atomic ->

            match SValue.toBool test, atomicity with

            // `doUntil { true } { ... }` ( atomic ) => ``
            | true, Atomic -> R.modified es

            // `doUntil { false } { ...ifFalse }` => `doForever { ...ifFalse }`
            | false, _ ->
                let s' = ComplexExpression.state e
                let e = ComplexExpression(s', O.doForever, [Block(BlockExpression(s, ifFalse))])
                R.modified (e::es)

            | _ -> R.noModify ees

        //| O.stopAll
        //| O.stopScripts
        //| O.deleteClone

        | _ -> R.noModify ees

[<AutoOpen>]
module private AlgebraicSimplificationPatterns =
    // Scratch では `NaN * x = 0`, `NaN + x = x`

    // `n * 0 = 0`
    [<return: Struct>]
    let (|NMul0|_|) = function
        | Mul(Number 0. & n, _)
        | Mul(_, Number 0. & n) -> ValueSome(R.modified n)
        | _ -> ValueNone

    // `n * 1 = n` ( n の型は NaN 以外の数値 )
    [<return: Struct>]
    let (|NMul1|_|) = function
        | Mul(Number 1., n)
        | Mul(n, Number 1.) when isNumberWithoutNaN n -> ValueSome(R.modified n)
        | _ -> ValueNone

    // `a * (b * n)` =
    // `a * (n * b)` =
    // `(a * n) * b` =
    // `(n * a) * b` = `(a * b) * n` ( a は定数, b は定数 )
    [<return: Struct>]
    let (|AMulBMulN|_|) = function
        | Mul(Literal _ & a, Mul(Literal _ & b, n))
        | Mul(Literal _ & a, Mul(n, Literal _ & b))
        | Mul(Mul(Literal _ & a, n), Literal _ & b)
        | Mul(Mul(n, Literal _ & a), Literal _ & b) as e ->
            let s = Expression.state e |> removeTags
            match Expressions.``*`` s a b |> evaluateExpressionWithGlobalContext with
            | Error _ -> R.noModify e |> ValueSome
            | Ok c -> Expressions.``*`` s (Literal(s, c)) n |> R.modified |> ValueSome
        | _ -> ValueNone

    // `a + (b + n)` =
    // `a + (n + b)` =
    // `(a + n) + b` =
    // `(n + a) + b` = `(a + b) + n` ( a は定数, b は定数 )
    [<return: Struct>]
    let (|AAddBAddN|_|) = function
        | Add(Literal _ & a, Add(Literal _ & b, n))
        | Add(Literal _ & a, Add(n, Literal _ & b))
        | Add(Add(Literal _ & a, n), Literal _ & b)
        | Add(Add(n, Literal _ & a), Literal _ & b) as e ->
            let s = Expression.state a |> removeTags
            match Expressions.``+`` s a b |> evaluateExpressionWithGlobalContext with
            | Error _ -> R.noModify e |> ValueSome
            | Ok c -> Expressions.``+`` s (Literal(s, c)) n |> R.modified |> ValueSome
        | _ -> ValueNone

    // `(x - x) = 0`
    [<return: Struct>]
    let (|XSubX|_|) = function
        | Sub(x, x') as e when Expression.map ignore x = Expression.map ignore x' ->
            Expression.eNumber (Expression.state e |> removeTags) 0. |> R.modified |> ValueSome
        | _ -> ValueNone

    // `(x + -y) = (x - y)`
    [<return: Struct>]
    let (|XAddNegY|_|) = function
        | Add(x, Mul(Number -1., y))
        | Add(x, Mul(y, Number( -1.))) as e ->
            Expressions.``-`` (Expression.state e) x y |> R.modified |> ValueSome
        | _ -> ValueNone

    // `x * 2 = x + x` (x は定数様)
    [<return: Struct>]
    let (|XMul2|_|) = function
        | Mul(Number 2., x)
        | Mul(x, Number 2.) as e when maxCost x <= LiteralLike ->
            Expressions.``+`` (Expression.state e |> removeTags) x x |> R.modified |> ValueSome
        | _ -> ValueNone

    // `(x / a) = (x * (1 / a))` (a は定数)
    [<return: Struct>]
    let (|XDivA|_|) = function
        | Div(x, Number a) as e ->
            let s = Expression.state e |> removeTags
            Expressions.``*`` s x (Expression.eNumber s (1. / a)) |> R.modified |> ValueSome
        | _ -> ValueNone

let algebraicSimplification e _ =
    match e with
    | NMul0 r
    | NMul1 r
    | AMulBMulN r
    | AAddBAddN r
    | XSubX r
    | XAddNegY r
    | XMul2 r
    | XDivA r
        -> r

    | e -> R.noModify e

let strengthReduction es _ =

    // `a <- a + v` = `a += v`
    match es with
    | SetVar_To_(varName, Add(Complex(ReadVariable varName'), value)) as e::es when varName = varName' ->
        let e = Expressions.``changeVar:by:`` (ComplexExpression.state e |> removeTags) varName value
        R.modified (e::es)

    | _ -> R.noModify es
