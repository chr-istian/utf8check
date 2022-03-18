# utf8check
Windows commandline tool to check UTF-8 encoding

Intention
---------

This program is intended to check source code files for proper UTF-8 encoding with a quality assuring build process.
If source code is merged or cherry picked from a version that was not yet UTF-8 encoded, strange mixed states can occur.
Encoding errors can cause build aborts or worse unrecognized akward characters in user interface and therefore frantic searching for the file and location causing this .

Usage
-----

utf8check.exe [Options] \<pathname\>
  
  Parameters
  pathname: file- or directoryname
  
  Options
  /brief /b        - only log errors to stdout
  /ignore-bom /ib  - ignore missing BOM
  /e:<extensions>  - semicolon separated list of file extensions
                     e.g. /e:pas;cpp;java;txt
  /s               - recurse subdirectories
  
  Return value     - ExitCode 0, if no encoding errors were detected
                     Exitcode 1, if one or more files have encoding errors