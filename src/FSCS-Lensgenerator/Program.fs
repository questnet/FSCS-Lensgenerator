open System
open System.IO
open System.Text
open System.Xml.Linq

open Argu

open LensGenerator

type CLIArguments = 
    | [<ExactlyOnce>] ProjectName of name:string
    | [<ExactlyOnce>] ProjectDir of path:string
    | [<ExactlyOnce>] ProjectFileName of name:string
    | [<Unique>] OutFileName of path:string
    | [<Unique>] OutputDir of path:string
    | [<Unique>] OutputNamespace of nmspc:string
    | [<Unique>] OutputProjectFile of outfsproj:string
    | [<Unique>] ErrorFileName of path:string
    | SysLib of name:string
    | AbsLib of fullpath:string
    | ProjLib of relpath:string
    | LibDir of fullpath:string
    | BinLib of relpath:string
    | WaitBeforeExit
    | Silent
    | RequestDebugger
    | WaitForDebugger
    | ShimReferenceAssemblies
    | [<Unique>] LensLibrary of LensLibrary
with
    interface IArgParserTemplate with
        member s.Usage = 
            match s with    
            | ProjectName _ -> "specify the input project name"
            | ProjectDir _ -> "specify the input project directory"
            | ProjectFileName _ -> "specify the name of the input project file (located in the project dir)"
            | OutFileName _ -> "SingleFile: specify the full name of the output file"
            | OutputNamespace _ -> "speficy the output namespace"
            | OutputDir _ -> "MultiFile: specify the output directory for multi file output"
            | OutputProjectFile _ -> "if MultiFile: update this fsproj file located in OutputDir (removes all .fs except AssemblyInfo and adds lens files)"
            | ErrorFileName _ -> "log errors to this file if given, to console otherwise"
            | SysLib _ -> "specify system libaries by name (mscorlib, System, System.Core are linked by default)"
            | AbsLib _ -> "specify libraries (binary/dll) as absolute path"
            | ProjLib _ -> "specify libraries (binary/dll) relative to the project directory"
            | LibDir _ -> "specify base directory for binlib"
            | BinLib _ -> "specify libraries (binary/dll) relative to the libraries directory"
            | WaitBeforeExit _ -> "wait for <Enter> before exiting"
            | Silent _ -> "reduce verbosity"
            | RequestDebugger _ -> "start with debugger attached and break instantly"
            | WaitForDebugger _ -> "wait for (remote) debugger to attach and break instantly"
            | LensLibrary _ -> "LensLibrary target, defaults to Aether (currently only Aether supported)"
            | ShimReferenceAssemblies _ -> "use filesystem shim for FSharp.Compiler.Service if you don't have the requierd reference assemblies"

let rec ensureDirectoryExists path = 
    let dir = new DirectoryInfo(path)
    if not dir.Exists then
        ensureDirectoryExists dir.Parent.FullName
        dir.Create()

let rec ensureParentDirectoryExists path = 
    let file = new FileInfo(path)
    let parentDirectry = file.Directory
    if not parentDirectry.Exists then
        ensureDirectoryExists parentDirectry.FullName

let wait100ms = fun _ -> System.Threading.Thread.Sleep(100)
let breakDebugger = fun _ -> System.Diagnostics.Debugger.Break()
let requestDebugger = fun _ -> (System.Diagnostics.Debugger.Launch() |> ignore)
let waitForDebugger = fun _ -> 
    Console.WriteLine("Waiting for debugger");
    while (not System.Diagnostics.Debugger.IsAttached) do wait100ms()
    Console.WriteLine("Debugger attached");

let breakIfDebuggerRequested (parsed:ParseResults<CLIArguments>) =
    let debuggerRequested = parsed.Contains <@ RequestDebugger @>
    let remoteDebugger = parsed.Contains <@ WaitForDebugger @>
    match (debuggerRequested,remoteDebugger) with 
    | (true,false) ->
        requestDebugger()
        breakDebugger()
    | (_,true) ->
        waitForDebugger()
        breakDebugger()
    | _ -> ()

let removeAndWarnAboutDuplicates (errorWriter : TextWriter) (lensFiles : string list) =
    let exactlyOne = function
        | x :: [] -> true
        | _ -> false

    let grouped = 
        lensFiles 
        |> List.map (fun name -> name, name.ToLowerInvariant()) 
        |> List.groupBy snd

    let nonUniques =
        grouped |> List.filter (fun e -> not (exactlyOne (snd e)))
    if not (nonUniques |> List.isEmpty) then
        let caseCount = nonUniques |> List.length
        let caseMessage = sprintf "Error: found %i cases of non-unique filenames" caseCount
        errorWriter.WriteLine(caseMessage)
        nonUniques
        |> List.sortBy (fun nu -> -(nu |> snd |> List.length))
        |> List.iter (fun nu -> 
                                let canonicalName = fst nu
                                let cnMessage = sprintf "    CanonicalName '%s'" canonicalName
                                errorWriter.WriteLine(cnMessage)
                                snd nu // alternatives by name
                                |> List.groupBy fst
                                |> List.map (fun (n,l) -> n, List.length l)
                                |> List.sortBy (fun (n,i) -> -i)
                                |> List.iter (fun (n,i) -> 
                                                            let nameMessage = sprintf "        %i x '%s'" i n
                                                            errorWriter.WriteLine(nameMessage)))

    let uniques = 
        grouped
        |> List.filter (fun e -> exactlyOne (snd e))
        |> List.map (fun t -> t |> snd |> List.head |> fst)
    uniques

let updateProjectFile (projectFileName:string) (errorWriter : TextWriter) (lensFiles : string list) =
    let originalProject = XDocument.Load projectFileName

    // the plan:
    // step 1: remove all `<Compile Include="$Name.fs" />` except our referenceFile (AssemblyInfo.fs)
    // step 2: for all items in lensFiles add filename as compile include
    // -> would require to .gitignore *.fs except the AssemblyInfo.fs in this dir -> potentially dangerous move...
    let xn ns s = XName.Get(s, ns)
    let xml = originalProject
    let xns = xn (xml.Root.Attribute(XName.Get("xmlns")).Value)

    // there should be only one ItemGroup with Compile tags
    let getCompileItemGroup (fsproj:XDocument) = 
        fsproj.Root.Elements(xns "ItemGroup")//.Where(ig=>ig.Elements().Any(e=>e.Name.Equals(xns "Compile")))
        |> Seq.filter(fun ig -> ig.Elements() |> Seq.exists (fun e -> e.Name.Equals(xns "Compile")))
        |> Seq.exactlyOne
    let removeNonAssemblyInfo (compileItemGroup:XElement) =
        let compiles = compileItemGroup.Elements(xns "Compile")
        let nonAssemblyInfos = compiles |> Seq.filter (fun e -> not (e.Attribute(xn "" "Include").Value.Equals("AssemblyInfo.fs")))
        let toBeDeleted = nonAssemblyInfos |> Seq.toList
        toBeDeleted.Remove()
        compileItemGroup    
    let addCompilesAfterAssemblyInfo (compileItemGroup : XElement) (fileNames : string list) =
        // find assembly info node (we could expect only the AssemblyInfo.fs)
        let compiles = compileItemGroup.Elements(xns "Compile")
        let assemblyInfo = compiles |> Seq.filter (fun e -> (e.Attribute(xn "" "Include").Value.Equals("AssemblyInfo.fs"))) |> Seq.exactlyOne
        let addcompiles = fileNames |> Seq.map (fun n -> XElement(xns "Compile", XAttribute(xn "" "Include", n)))
        assemblyInfo.AddAfterSelf(addcompiles)

    let cig = getCompileItemGroup originalProject
    let withouCompiles = removeNonAssemblyInfo cig   
    let uniqueFiles = removeAndWarnAboutDuplicates errorWriter lensFiles
    addCompilesAfterAssemblyInfo cig uniqueFiles
    originalProject.Save(projectFileName)
    ()

type NullWriter() = 
    inherit TextWriter()
    override __.Encoding = new UTF8Encoding(false) :> Encoding
    // just to make it explicit that we do nothing
    // see http://referencesource.microsoft.com/#mscorlib/system/io/textwriter.cs,193
    override __.Write (c:char) = ()
    // return earlier instead of calling Write(char) all the time
    // a good optimizer would probably get this under the hood too
    override __.Write (s:string) = ()
    override __.WriteLine (s:string) = ()
    override __.WriteLine () = ()
    interface IDisposable with member __.Dispose() = ()

let runOn (parsed:ParseResults<CLIArguments>) = 
    breakIfDebuggerRequested parsed
    let shimRequested = parsed.Contains <@ ShimReferenceAssemblies @>
    let projectName = parsed.GetResult(<@ ProjectName @>)
    let projectDir = parsed.GetResult(<@ ProjectDir @>)
    let projectFileName = parsed.GetResult(<@ ProjectFileName @>)

    let outputNamespace = parsed.GetResult(<@ OutputNamespace @>, defaultValue = "Lenses" )
    let wait = parsed.Contains <@ WaitBeforeExit @>
    let silent = parsed.Contains <@ Silent @>
        
    let sysLibs = parsed.GetResults <@ SysLib @>
    let absLibs = parsed.GetResults <@ AbsLib @>
    let projLibs = parsed.GetResults <@ ProjLib @>
    let libDir = parsed.GetResult(<@ LibDir @>, defaultValue = Environment.CurrentDirectory)
    let binLibs = parsed.GetResults <@ BinLib @>

    let sysAssemblies = sysLibs |> List.map FrontEnd.sysLib
    let projAssemblies = projLibs |> List.map (fun l -> Path.Combine(projectDir, l))
    let binAssemblies = binLibs |> List.map (fun l -> Path.Combine(libDir, l))

    let referencedAssemblies= sysAssemblies @ projAssemblies @ binAssemblies @ absLibs

    let outFileNameOpt = parsed.GetResults(<@ OutFileName @>) |> Seq.tryHead
    let outputDirOpt = parsed.GetResults(<@ OutputDir @>)  |> Seq.tryHead
    let outputProjectFileNameOpt = parsed.GetResults(<@ OutputProjectFile @>) |> Seq.tryHead
    let errorFileName = parsed.GetResults(<@ ErrorFileName @>) |> Seq.tryHead
    let lensLib = parsed.GetResult(<@ LensLibrary @>, defaultValue = Aether)

    let outConfig : OutputConfig =
        let ignore2 _ _ = ()
        let errorWriterFactory = function
            | None -> 
                (fun () -> 
                    match silent with  
                    | true -> new NullWriter() :> TextWriter
                    | false -> Console.Out)
            | Some(fileName) -> (fun () -> new StreamWriter(fileName, false, new UTF8Encoding(false)) :> TextWriter)

        match (outFileNameOpt,outputDirOpt,outputProjectFileNameOpt) with
        | Some(outFileName),None,None -> 
            ensureParentDirectoryExists outFileName
            let outputConfig = {
                FileConfig = CreateLensFile(fun () -> new StreamWriter(outFileName, false, new UTF8Encoding(false)) :> TextWriter)
                Namespace = outputNamespace
                LensLibrary = lensLib
                CreateErrorWriter = errorWriterFactory errorFileName
                UpdateProjectFile = ignore2
            }
            outputConfig
        | None, Some(outDir), None ->
            // multi file, don't touch output project file (if it even exists)
            ensureDirectoryExists outDir
            let outputConfig = {
                FileConfig = CreateLensFileFor(fun name ->
                    new StreamWriter(Path.Combine(outDir, name), false, new UTF8Encoding(false)) :> TextWriter)
                Namespace = outputNamespace
                LensLibrary = lensLib
                CreateErrorWriter = errorWriterFactory errorFileName
                UpdateProjectFile = ignore2
            }
            outputConfig
        | None, Some(outDir), Some(outProject) ->
            // todo: multi file, update output project file
            let outputConfig = {
                FileConfig = CreateLensFileFor(fun name ->
                    new StreamWriter(Path.Combine(outDir, name), false, new UTF8Encoding(false)) :> TextWriter)
                Namespace = outputNamespace
                LensLibrary = lensLib
                CreateErrorWriter = errorWriterFactory errorFileName
                UpdateProjectFile = updateProjectFile outProject
            }
            outputConfig
        | _ -> failwith ("either set outfilename for single file output" + Environment.NewLine + "or outputdir (and optionally outputprojectfile) for multi file output")

    

    let run() = 
        let input : FrontEnd.InputProject = { // lets hope we dont need type annotation that long
            projectName = projectName
            projectDir = projectDir
            projectFileName = projectFileName
            referencedAssemblies = referencedAssemblies
            shimRequested = shimRequested
        }
        let lensGenerationParameters : LensGenerationParameters = { // lets hope we dont need type annotation that long
            input = input
            output = outConfig
        }
        generate(lensGenerationParameters)
        ()

    run()
    if not silent then
        match (outFileNameOpt,outputDirOpt,errorFileName) with
        | (Some(singleFile),_,None) -> printfn "output written to %s" singleFile
        | (None,Some(dir),None) -> printfn "output written to %s" dir
        | (Some(singleFile),_,Some(err)) -> printfn "output written to %s, errors in %s" singleFile err
        | (None,Some(dir),Some(err)) -> printfn "output written to %s, errors in %s" dir err
        | _ -> ()
        
    
    if wait then
        printfn "press enter to quit"
        let key = Console.ReadKey()
        ()

let parseAndRun argv =
    let parser = ArgumentParser.Create<CLIArguments>()
    try
        let results = parser.Parse(argv)
        runOn results
    with ex ->
#if DEBUG
        printfn "%s" (ex.ToString())
#endif
        let usage = parser.PrintUsage()
        printfn "%s" usage
        ()

[<EntryPoint>]
let main argv =
    parseAndRun argv
    0 // return an integer exit code
