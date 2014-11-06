#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

// Dirs

let tempDir = "./temp"
let srcDir = tempDir + "/src"

// Clean

Target "Clean" (fun _ ->
    CleanDirs [ tempDir ])

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
              OutputPath = tempDir
              WorkingDir = srcDir
              Version = "4.1.1"
              AccessKey = getBuildParamOrDefault "nuget_key" ""
              Publish = hasBuildParam "nuget_key"
              Dependencies = []
              Files = [ "Aether.dll", Some "lib/net40", None ] })
              "./nuget/Aether.nuspec")

// Dependencies

"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "Publish"

RunTargetOrDefault "Publish"
