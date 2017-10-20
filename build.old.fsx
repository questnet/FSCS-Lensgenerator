#r @"packages/FAKE/tools/FakeLib.dll"
open System
open System.IO
open System.Text
open Fake
open Fake.NuGet
open Fake.Git
open Fake.GitVersionHelper
open Fake.Paket
open Fake.Testing
open Fake.SCPHelper
open Fake.SSHHelper

// todo: port TFS specific code to AppVeyor/local build
let qnugetPublishKey = "geheim"// not really, we push via TFS dedicated task 
let artifactDir = "./artifacts"
let packagingDir = "./artifacts/nuget"
let nugetToolPath = "packages/build/NuGet.CommandLine/tools/NuGet.exe"
let gitVersionToolPath = " packages/build/GitVersion.CommandLine/tools/GitVersion.exe"
let buildDir = "FSCS-LensGenerator/bin/Release"
let redistDir = "FSCS-LensGenerator/redist"

let authors = ["QuestNet GmbH"]// also used for owner and copyright
let projectName = "FSCS-LensGenerator"
let projectSummary = "FSCS-LensGenerator: generate lenses/optics for F# projects"
let projectDescription = "FSCS-LensGenerator is a code generator that generates lenses/optics for your F# projects. Currently only Aether output is supported but we're open accept other output providers."

let localVersion() =
    let gitver = GitVersion (fun p -> {p with ToolPath = gitVersionToolPath })
    gitver.NuGetVersionV2

let updateTfsBuildNumber v = 
    let message = sprintf "Set build number to %s" v
    trace message
    // TFS magic: see https://github.com/Microsoft/vsts-tasks/blob/master/docs/authoring/commands.md
    let updateCommand = sprintf "##vso[build.updatebuildnumber] %s" v
    log updateCommand

let nugetPackageVersion =
    match buildServer with
    | TeamFoundation -> 
        let nugetVersion2 = localVersion() // NOT buildVersion
        updateTfsBuildNumber(nugetVersion2)// if we publish this package, our build number should match the packages build number
        nugetVersion2
    | LocalBuild -> localVersion()
    | _ -> "0.0.0-local"

let binaryPackagePatterns = ["*.exe";"*.dll";"*.config";(*"*.pdb";"*.txt"*)]
let redistPatterns = ["*.dll";"*.optdata";"*.sigdata";"*.txt"]
let copyWithPatterns (patterns:string list) (toDir:string) (fromDir:string) = 
    let allMatchings =
        patterns
        |> Seq.collect ( fun p -> (directoryInfo fromDir).EnumerateFiles(p) )
        |> Seq.distinct
        |> Seq.map ( fun fi -> fi.FullName )
    allMatchings |> CopyFiles toDir
let createPackage() =
    let nugetToolsDir = packagingDir @@ "tools"
    let nugetToolsRedistDir = nugetToolsDir // nuget spec is picky, easier without extra sub folder
    let copyBinariesToNugetToolsDir = copyWithPatterns binaryPackagePatterns nugetToolsDir
    let copyRedistToNugetToolsDirRedist = copyWithPatterns redistPatterns nugetToolsRedistDir

    CleanDir packagingDir
    CleanDir nugetToolsDir

    copyBinariesToNugetToolsDir buildDir
    copyRedistToNugetToolsDirRedist redistDir

    let setParams p = 
        {p with
            Authors = authors
            Project = projectName
            OutputPath = packagingDir
            Summary = projectSummary
            Description = projectDescription
            WorkingDir = packagingDir
            Version = nugetPackageVersion// as detect from build server, "LocalBuild" for local builds...
            //AccessKey = qnugetPublishKey
            Publish = false // we don't want our private packages ending up in the nuget gallery
            ToolPath = nugetToolPath
        }
    NuGet setParams "FSCS-LensGenerator.nuspec"

let pushPackage() =
    let setParams p =
        {p with 
            AccessKey = qnugetPublishKey
            Publish = true
            ToolPath = nugetToolPath
        }
    // TODO: NuGetPublish setParams
    ()

let displayHelp() =

    let listCommentsOnTargets()=
        trace "Comment on Targets:"
        trace "    CreatePackage        create NuGet package from build output"
        trace "    CreatePackage        create NuGet package from build output and push it to our internal feed"

    let listExampleUsages() =
        trace "exaple usages:"
        trace @"    .\build.bat CreatePackage"

    listTargets()
    listCommentsOnTargets()
    listExampleUsages()

Target "CreatePackage" createPackage
Target "PushPackage" pushPackage

Target "Help" displayHelp

// dependencies
//"Bootstrap" ==> "Clean"
//"Clean" ==> "Build"
//"Build" ==> "Test"
//"Test" ==> "CreatePackage"
"CreatePackage" ==> "PushPackage"

// default target
RunTargetOrDefault "Help"
