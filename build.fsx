#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

open Fake

// Dirs

let tempDir = "temp"

// Clean

Target "Clean" (fun _ ->
    CleanDirs [ tempDir ])

// Build

Target "Build" (fun _ ->
    build (fun x ->
        { x with
            Properties =
                [ "Optimize",      environVarOrDefault "Build.Optimize"      "True"
                  "DebugSymbols",  environVarOrDefault "Build.DebugSymbols"  "True"
                  "Configuration", environVarOrDefault "Build.Configuration" "Release" ]
            Targets =
                [ "Build" ]
            Verbosity = Some Quiet }) "Aether.sln")

// Package

Target "Pack" (fun _ ->
    Paket.Pack (fun p ->
        { p with
            OutputPath = tempDir }))

Target "Push" (fun _ ->
    Paket.Push (fun p ->
        { p with
            WorkingDir = tempDir }))

// Dependencies

Target "Default" DoNothing

"Clean"
    ==> "Build"
    ==> "Pack"
    =?> ("Push", Option.isSome (environVarOrNone "nugetkey"))
    ==> "Default"

RunTargetOrDefault "Default"
