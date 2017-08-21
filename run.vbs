Randomize
CreateObject("Wscript.Shell").Run "R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runPubmed.R" & " " & "" & " ", 0, False
