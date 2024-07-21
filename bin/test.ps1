function verify_exercise ($practiceExerciseDir) {
    $dir=[System.IO.Path]::GetFullPath($practiceExerciseDir)
    Write-Output "MH dir -> $dir"
    $slug=Split-Path $dir -Leaf
    Write-Output "MH slug -> $slug"
    $implementationFile="$dir/.meta/proof.ci.cob"
    Write-Output "MH implementationFile -> $implementationFile"
    $stubFile="$dir/src/$slug.CBL"
    Write-Output "MH stubFile -> $stubFile"
    $stubBackupFile="$stubFile.bak"
    Copy-Item $stubFile -Destination $stubBackupFile
    Copy-Item $implementationFile -Destination $stubFile

    Invoke-Expression "$dir/test.ps1"

    if ($Lastexitcode -ne 0) {
        Write-Output $slug": proof solution did not pass the tests"
        exit 1
    }

    Copy-Item $stubBackupFile -Destination $stubFile
}

Get-ChildItem -Directory ./exercises/practice/ |
    ForEach-Object {
        Write-Output "Checking $_ exercise..."
        verify_exercise $_
    }
