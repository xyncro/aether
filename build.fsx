#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

// Dirs

let outDir = "./output"
let srcDir = outDir + "/src"

// Clean

Target "Clean" (fun _ ->
    CleanDirs [ outDir ])

// Restore Packages

Target "Restore" (fun _ ->
    RestorePackages ())

// Build

Target "Build" (fun _ ->
    !! "src/**/*.fsproj"
    |> MSBuildRelease srcDir "Build" 
    |> Log "Build Source: ")

// Publish

Target "Publish" (fun _ ->
    NuGet (fun p ->
        { p with
              Authors = [ "Andrew Cherry" ]
              Project = "Aether"
              OutputPath = outDir
              WorkingDir = srcDir
              Version = "0.0.2-pre001"
              AccessKey = getBuildParamOrDefault "nuget_key" ""
              Publish = hasBuildParam "nuget_key"
              Dependencies = []
              Files = [ "Aether.dll", Some "lib/net45", None ] })
              "./nuget/Aether.nuspec")

// Dependencies

"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "Publish"

RunTargetOrDefault "Publish"
