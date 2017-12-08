module LensGenerator

open System
open System.IO
open System.Linq
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.SourceCodeServices

module FrontEnd = 
    type InputProject = {
        projectName : string
        projectDir : string
        projectFileName : string    
        referencedAssemblies : string list
        shimRequested : bool
    }
    
    let getFsFilesIn projectDirectory =
        let fsprojFile = 
            Directory.GetFiles projectDirectory
            |> Seq.tryFind(fun file -> (FileInfo file).Extension = ".fsproj")
        if fsprojFile.IsSome then
            let content = File.ReadAllText fsprojFile.Value
            // todo: use xml document parsing as it should be less error prone
            let matches = Regex.Matches(content,@"<Compile Include=""(\w+\.fs+)\"" />")
            seq { for m in matches -> m.Groups.[1].Value }
            |> Seq.map(fun filename -> sprintf @"%s\%s" projectDirectory filename)
        else failwith ".fsproj file couldn't find !!!"

    let sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\" + nm + ".dll"
        else
            // untested
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let (++) a b = System.IO.Path.Combine(a,b)
            sysDir ++ nm + ".dll"

    let analyze (inputProject : InputProject) =
        let checker = FSharpChecker.Create()

        let projectFullFileName = Path.Combine(inputProject.projectDir, inputProject.projectFileName)

        let projectOptions = 
            let fsCoreRef() = 
                if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
                    // file references only valid on Windows
                    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
                    // have fun with versions
                    // BUG#2801
                    //@"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"  
                    @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
                else 
                    sysLib "FSharp.Core"

            let staticSwitches = seq {
                yield "--simpleresolution"
                yield "--noframework"
                yield "--debug:full"
                yield "--define:DEBUG"
                yield "--optimize-"
                //yield "--out:" + Inputs.dllName
                yield "--doc:test.xml"
                yield "--warn:3"
                yield "--fullpaths"
                yield "--flaterrors"
                yield "--target:library"
            }
            let inputFiles = getFsFilesIn inputProject.projectDir
            let references = seq{
                    yield sysLib "mscorlib"
                    yield sysLib "System"
                    yield sysLib "System.Core"
                    yield fsCoreRef() 
                    yield! inputProject.referencedAssemblies
                }
            let referencedReferences = seq {
                for r in references do 
                    yield "-r:" + r 
            }
            let switches = 
                [staticSwitches;inputFiles;referencedReferences] 
                |> List.map Seq.toArray
                |> Array.concat

            checker.GetProjectOptionsFromCommandLineArgs(projectFullFileName, switches)

        // compile/analyze
        let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
        wholeProjectResults

    let getTypeNames (entity : FSharpEntity) (errorWriter : TextWriter) = 
        let inTypeName = entity.DisplayName
        let inTypeFullName = 
            try
                match entity.TryFullName with
                | Some(name) -> name
                | None ->
                    errorWriter.WriteLine("//warning, no FullName for " + entity.DisplayName)
                    entity.DisplayName
            with ex -> 
                errorWriter.WriteLine("//warning, TryFullName threw for " + entity.DisplayName + ": "+ ex.ToString())
                entity.DisplayName
        (inTypeName, inTypeFullName)

    let realFileSystem = Shim.FileSystem

    type MappedFileSystem(mappings,logDebug)=
        let underlyingFileSystem = realFileSystem
        let mappings = mappings // from virtual/fake location to real location
        let logDebug = logDebug // todo: strip when integrating

        let getRealPosition virtualFileName =
            match Map.tryFind virtualFileName mappings with
            | Some(realFileName) -> realFileName
            | None -> virtualFileName

        interface IFileSystem with
            member __.FileStreamReadShim(virtualFileName) = 
                getRealPosition virtualFileName
                |> underlyingFileSystem.FileStreamReadShim

            member __.FileStreamCreateShim(virtualFileName) =
                underlyingFileSystem.FileStreamCreateShim(virtualFileName)
            member __.FileStreamWriteExistingShim(virtualFileName) =
                // we don't want anyone to fiddle around with our mapped files
                underlyingFileSystem.FileStreamWriteExistingShim(virtualFileName)

            member __.ReadAllBytesShim(virtualFileName) =
                getRealPosition virtualFileName
                |> underlyingFileSystem.ReadAllBytesShim
            
            member __.GetTempPathShim() =
                underlyingFileSystem.GetTempPathShim()
            member __.GetLastWriteTimeShim(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.GetLastWriteTimeShim
            member __.GetFullPathShim(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.GetFullPathShim
            member __.IsInvalidPathShim(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.IsInvalidPathShim
            member __.IsPathRootedShim(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.IsPathRootedShim
            
            member __.SafeExists(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.SafeExists
            member __.FileDelete(virtualFileName)=
                match Map.tryFind virtualFileName mappings with
                | Some(realFileName) -> () // nope! we want to keep our mapped files
                | None -> underlyingFileSystem.FileDelete(virtualFileName)
            
            member __.AssemblyLoadFrom(virtualFileName)=
                getRealPosition virtualFileName
                |> underlyingFileSystem.AssemblyLoadFrom
            member __.AssemblyLoad(assemblyName)=
                // lets hope this works even for our mapped reference libs
                logDebug(sprintf "debug: load assembly direct from underlying filesystem: '%s'" (assemblyName.ToString()))
                underlyingFileSystem.AssemblyLoad assemblyName                

    let setUpShim (isRequested : bool) (errorWriter : TextWriter) =
        match isRequested with
        | false -> ()
        | _ ->
            let thisAssembly = typedefof<InputProject>.Assembly
            let binDir = thisAssembly.GetFiles() |> Seq.head |> (fun f -> ((new FileInfo(f.Name)).Directory))
            let shimFileName = "filesystemshim.txt"
            errorWriter.WriteLine(sprintf "debug: searching for '%s' in '%s'" shimFileName (binDir.FullName))
            let mappingOpt = binDir.EnumerateFiles(shimFileName) |> Seq.tryHead
            match mappingOpt with
            | None -> ()
            | Some(mappingFile) -> 
                errorWriter.WriteLine(sprintf "debug: found '%s'" mappingFile.FullName)
                let mappings = // fake location, real location
                    File.ReadLines(mappingFile.FullName)
                    |> Seq.filter (fun line -> (not (line.StartsWith("#"))))
                    |> Seq.map (fun line -> FileInfo(line))
                    |> Seq.map (fun f -> f.FullName, Path.Combine(mappingFile.DirectoryName, f.Name))
                    |> Map.ofSeq
                let mappedFileSystem = MappedFileSystem(mappings, (fun msg -> errorWriter.WriteLine(msg)))
                Shim.FileSystem <- mappedFileSystem
                ()

type LensLibrary = 
    | Aether

type OutputFileCreation =
    | CreateLensFile of (unit -> TextWriter)
    | CreateLensFileFor of (string -> TextWriter)

type OutputConfig = {
    FileConfig : OutputFileCreation
    Namespace : string
    LensLibrary : LensLibrary
    CreateErrorWriter : (unit -> TextWriter)
    UpdateProjectFile : (TextWriter -> string list -> unit)
}

type LensGenerationParameters = {
    input : FrontEnd.InputProject
    output : OutputConfig
}

type IFormatLenses =
    abstract member FormatLensFileHeader: unit -> string
    abstract member FormatLensHeader: string -> string -> string
    abstract member FormatLens: string -> string -> string -> string -> string -> string
    abstract member FormatLensFooter: unit -> string
    abstract member FormatLensFileFooter: unit -> string

type AetherLensFormatter (outputNamespace : string) =
    let outputNamespace = outputNamespace
    let lensNamePrefix = ""
    let lensNameSuffix = "_" //"L"

    let formatLens inTypeFullName typeshorty propTypeName propName propshorty = 
        let memberDef = "    let " + lensNamePrefix + propName + lensNameSuffix + " : Lens<" + inTypeFullName + "," + propTypeName + "> =";
        let getterDef = "        let getterDef = ( fun (" + typeshorty + ":" + inTypeFullName + ") -> " + typeshorty + "." + propName + " )"
        let setterDef = "        let setterDef = ( fun (" + propshorty + ":" + propTypeName + ") (" + typeshorty + ":" + inTypeFullName + ") -> {" + typeshorty + " with " + propName + " = " + propshorty + " } )"
        let returnDef = "        (getterDef,setterDef)"
        let formatted = String.Join(Environment.NewLine, [|memberDef;getterDef;setterDef;returnDef;String.Empty|])
        formatted        

    let formatLensFileHeader (outputNamespace : string) =
        let nmspc = "namespace " + outputNamespace
        let comment = "// autogenerated with FSCSLensGenerator"
        let includes = "open Aether"
        let formatted = String.Join(Environment.NewLine, [|nmspc;Environment.NewLine;comment;includes;Environment.NewLine|])
        formatted        

    let formatLensHeader (inTypeName : string) (inTypeFullName : string) =
        let comment = "// autogenerated Aether.Lens-es for " + inTypeFullName
        let definition = "module " + inTypeName + " = "
        // we don't need to import the module/namespaces if we use fully qualyfied type names
        // this also reduces the amount of possible clashes
        (comment + Environment.NewLine + definition + Environment.NewLine)
        
    let formatLensFooter() = Environment.NewLine
    let formatLensFileFooter() = String.Empty

    interface IFormatLenses with
        member __.FormatLensFileHeader() = 
            formatLensFileHeader outputNamespace
        member __.FormatLensHeader inTypeName inTypeFullName = 
            formatLensHeader inTypeName inTypeFullName
        member __.FormatLens inTypeFullName typeshorty propTypeName propName propshorty = 
            formatLens inTypeFullName typeshorty propTypeName propName propshorty
        member __.FormatLensFooter() =
            formatLensFooter()
        member __.FormatLensFileFooter() =
            formatLensFileFooter()


type IWriteLenses =
    abstract member WriteLensFileHeader: unit -> unit
    abstract member WriteLensHeader: string -> string -> unit
    abstract member WriteLens: string -> string -> string -> string -> string -> unit
    abstract member WriteLensFooter: unit -> unit
    abstract member WriteLensFileFooter: unit -> unit

type IWriteMultipleFiles =
    abstract member WriteLensFileFor : FSharpEntity -> string option

type IWriteASingleFile =
    abstract member WriteLensesFor : FSharpEntity list -> unit

module LensHelper = // I know...
    let breakAndReturn x =
        x // just so we have a break point    
    
    let bestname (n:string) = 
        let first (n:string) = n.ToLower().[0] |> string
        let allUpper (n:string) = 
            n.ToCharArray() 
            |> Array.filter Char.IsUpper 
            |> (fun ca -> (new String(ca)).ToLower())
        let rec lastOrEmpty l =
            match l with
            | [] -> ""
            | c :: [] -> new String([|c|])
            | h :: t -> lastOrEmpty t
        let lastUpper (n:string) = 
            n.ToCharArray() 
            |> Array.filter Char.IsUpper 
            |> Array.toList
            |> lastOrEmpty
            |> (fun s -> s.ToLower())
        let nameIsAllUpper (n:string) = n.ToUpper() = n
        let nameIsAllLower (n:string) = n.ToLower() = n
        // choose best fitting strategy, first should be good enough though
        let returnValue = 
            match (nameIsAllUpper(n), nameIsAllLower(n), first(n), lastUpper(n), allUpper(n)) with
            | false,false,_,l,_ -> l // RoutingContext=>c, In
            | _,_,f,_,_ -> f // MSN=>m, IVR=>i string=>s
        let realReturnValue = 
            if returnValue = "" then breakAndReturn "x"
            else returnValue
        realReturnValue

    let rec getTypeDisplayName (t:FSharpType) =
        let guessGenericCode (e:FSharpEntity) (fst:FSharpType) = 
            let genericArguments = fst.GenericArguments |> Seq.map (fun ga -> getTypeDisplayName ga)
            let genArgArray = genericArguments |> Seq.toArray
            // prevent stupid "A unique overload for method 'Join' ..." compiler error
            let fuckingStringJoin (sep:string) (arr:string[]) = String.Join(sep, arr) 
            let genArgString = fuckingStringJoin "," genArgArray
            e.DisplayName + "<" + genArgString + ">"        
        
        let guessName (e:FSharpEntity) (fst:FSharpType) =
            try
                if e.GenericParameters.Count > 0 then guessGenericCode e t
                else                 
                    if fst.IsAbbreviation then                    
                        // abbreviated types usually have no FullName
                        fst.TypeDefinition.AccessPath + "." + fst.TypeDefinition.DisplayName
                    else e.FullName
            with ex ->
                e.ToString()
        if t.HasTypeDefinition then
            try
                let def = t.TypeDefinition
                guessName def t
            with ex ->
                breakAndReturn "fuuuuuuuuuuuuuuuuuuuuu" //todo: handle
        else
            let getTupleCode (e:FSharpType) =
                String.Join(" * ", 
                    e.GenericArguments
                    |> Seq.map getTypeDisplayName
                )
            let getFunctionCode (e:FSharpType) =
                e.Format FSharpDisplayContext.Empty
            if t.IsTupleType then
                getTupleCode t
            else
                if t.IsFunctionType then
                    getFunctionCode t
                else
                    breakAndReturn "alternatives????" // todo: handle

    let getCandidatesForPropertyShorty (propName:string) (propTypeName:string) = 
        let bestPropName = bestname propName // current implementation
        let bestTypeName = bestname propTypeName
        let altName1 = bestPropName + bestPropName
        let altName2 = bestTypeName + bestTypeName
        let candidates = [bestPropName;altName1;bestTypeName;altName2]
        candidates
    
    let propertyShorty (propName:string) (propTypeName:string) (clashes:string list) = 
        let candidates = getCandidatesForPropertyShorty propName propTypeName
        let remainingCandidates = candidates.Except(clashes).ToList()
        remainingCandidates.First()

    let writeLens(lensWriter:IWriteLenses)(inTypeName:string)(inTypeFullName:string)(field:FSharpField)=
        let propName = field.DisplayName
        let fieldType = field.FieldType;
        let propTypeName = getTypeDisplayName fieldType
        let typeshorty = bestname inTypeName
        let propshorty = propertyShorty propName propTypeName [typeshorty;propName;propTypeName]
        lensWriter.WriteLens inTypeFullName typeshorty propTypeName propName propshorty
        ()

    let writeLensMembers(lensWriter:IWriteLenses)(inTypeName:string)(inTypeFullName:string)(fields:IList<FSharpField>)=
        let createLensFor = writeLens lensWriter inTypeName inTypeFullName
        fields
        |> Seq.iter createLensFor
        ()
            
    let lensGenerationFunc (errorWriter:TextWriter) (e:FSharpEntity) =
        let (inTypeName, inTypeFullName) = FrontEnd.getTypeNames e errorWriter
        if e.IsFSharpRecord then
            if e.FSharpFields.Count>0 then
                let generateLensForRecord (lensWriter:IWriteLenses) =
                    lensWriter.WriteLensHeader inTypeName inTypeFullName
                    writeLensMembers lensWriter inTypeName inTypeFullName e.FSharpFields
                    lensWriter.WriteLensFooter()
                    ()
                Some(generateLensForRecord)
            else None // no fields, no lenses
        else if e.IsFSharpUnion then
            // todo: we should support discriminated unions too
            None
        else
            None

    let sanitizeFileName name = 
        // todo: make sure we get legal file names and not some ``blörp :-\ 😄💩``
        name
        
    let WriteLensFileIfApplicable (errorWriter:TextWriter) (entity:FSharpEntity) (generateFile:string->TextWriter) (formatter : IFormatLenses) writerFunc =
        let lensGenerationFunc = lensGenerationFunc errorWriter entity
        match lensGenerationFunc with
        | None -> None
        | Some(lensGenerationFunc) ->
            let (inTypeName, inTypeFullName) = FrontEnd.getTypeNames entity errorWriter
            let lensFileName = (sanitizeFileName inTypeName) + ".fs"
            use lensFile = generateFile lensFileName
            let writer = writerFunc(errorWriter, lensFile, formatter) :> IWriteLenses // why is the cast required?!
            writer.WriteLensFileHeader()
            lensGenerationFunc writer
            writer.WriteLensFileFooter()
            Some(lensFileName)

type SingleFileLensWriter(errorWriter:TextWriter, outputWriter:TextWriter, formatter : IFormatLenses) = 
    let formatter = formatter
    let outputWriter = outputWriter

    let generateLensFor (lensWriter:IWriteLenses) (entity:FSharpEntity) =
        let lensGenerationFunc = LensHelper.lensGenerationFunc errorWriter entity
        match lensGenerationFunc with
        | Some(generate) -> generate lensWriter
        | None -> ()

    interface IWriteLenses with
        member __.WriteLensFileHeader() = 
            let fileHeader = formatter.FormatLensFileHeader()
            outputWriter.Write(fileHeader)
        member __.WriteLensHeader inTypeName inTypeFullName = 
            let lensHeader = formatter.FormatLensHeader inTypeName inTypeFullName
            outputWriter.Write(lensHeader)
        member __.WriteLens inTypeFullName typeshorty propTypeName propName propshorty = 
            let lens = formatter.FormatLens inTypeFullName typeshorty propTypeName propName propshorty
            outputWriter.Write(lens)
        member __.WriteLensFooter() =
            let lensFooter = formatter.FormatLensFooter()
            outputWriter.Write(lensFooter)
        member __.WriteLensFileFooter() =
            let fileFooter = formatter.FormatLensFileFooter()
            outputWriter.Write(fileFooter)

    interface IWriteASingleFile with
        member self.WriteLensesFor (entities : FSharpEntity list) =
            let me = (self :> IWriteLenses)
            me.WriteLensFileHeader()
            entities |> Seq.iter (generateLensFor me)
            me.WriteLensFileFooter()
            ()

type MultiFileLensWriter (errorWriter:TextWriter, generateFile:string->TextWriter, formatter : IFormatLenses) =
    let errorWriter = errorWriter
    let generateFile = generateFile
    let formatter = formatter

    let writeLensFileFor (entity:FSharpEntity) =
        // todo: reorder arguments for better reuse
        // formatter generateFile errorWriter entity 
        let writerFunc = (fun (ew,ow,fm) -> new SingleFileLensWriter(ew,ow,fm) :> IWriteLenses)
        LensHelper.WriteLensFileIfApplicable errorWriter entity generateFile formatter writerFunc

    interface IWriteMultipleFiles with
        member __.WriteLensFileFor entity =
            writeLensFileFor entity

let severityToString (s:FSharpErrorSeverity) =
    match s with
    | FSharpErrorSeverity.Error -> "Error"
    | FSharpErrorSeverity.Warning -> "Warning"

let commentLines (message:string) =
    let lines = message.Split([|"\n"|], StringSplitOptions.None)
    let remainingCommented = String.Join("\n// ", lines)
    "// " + remainingCommented

let writeCompileError (errorWriter:TextWriter) (i:int) (e:FSharpErrorInfo) =
    let writeCommented (m:string) = errorWriter.WriteLine (commentLines m)
    writeCommented (sprintf "%i %s: %s" i (severityToString(e.Severity)) e.Subcategory)
    writeCommented (sprintf "in %s (%i,%i) to (%i,%i)" e.FileName e.StartLineAlternate e.StartColumn e.EndLineAlternate e.EndColumn)
    writeCommented (sprintf "%s" e.Message)
    writeCommented (sprintf "")
    ()

let rec allEntities (entities: IList<FSharpEntity>) = 
    [ for e in entities do 
        yield e
        yield! allEntities e.NestedEntities
        ]

let getLensFormatter (library : LensLibrary) (outputNamespace : string) =
    match library with
    | Aether -> new AetherLensFormatter(outputNamespace)
    | unsupportedLibrary -> failwithf "lens library `%s` not yet supported" (unsupportedLibrary.ToString())

let generate(parameters : LensGenerationParameters) =
    let inputProject = parameters.input
    let output = parameters.output

    use errorWriter = output.CreateErrorWriter()
    // frontend
    FrontEnd.setUpShim inputProject.shimRequested errorWriter
    let wholeProjectResults = FrontEnd.analyze inputProject
    wholeProjectResults.Errors
    |> Array.iteri (writeCompileError errorWriter)

    // backend
    let formatter = getLensFormatter output.LensLibrary output.Namespace :> IFormatLenses
    match output.FileConfig with
    | CreateLensFile(f) -> 
        let entities = allEntities wholeProjectResults.AssemblySignature.Entities
        use lensFile = f()
        let singleFileWriter = new SingleFileLensWriter(errorWriter, lensFile, formatter) :> IWriteASingleFile
        singleFileWriter.WriteLensesFor entities
    | CreateLensFileFor(fn) ->
        let multiFileWriter = new MultiFileLensWriter(errorWriter, fn, formatter) :> IWriteMultipleFiles
        let filesWritten =
            allEntities wholeProjectResults.AssemblySignature.Entities
            |> Seq.map (multiFileWriter.WriteLensFileFor)
            |> Seq.filter (fun n -> Option.isSome n)
            |> Seq.map (fun nameopt -> nameopt.Value)
            |> Seq.toList
        output.UpdateProjectFile errorWriter filesWritten