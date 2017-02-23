$ErrorActionPreference = "Stop"

function checkExitCode {
    if ($LastExitCode -ne 0) {
        throw "Non-zero exit code: $LastExitCode"
    }
}

dotnet clean
checkExitCode

dotnet restore
checkExitCode

dotnet build -c Release --version-suffix "dev"
checkExitCode

dotnet test -c Release tests/Aether.Tests/Aether.Tests.fsproj
checkExitCode

dotnet pack -c Release --include-symbols --include-source --version-suffix "dev"
