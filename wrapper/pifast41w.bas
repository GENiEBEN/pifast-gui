'================================================================================
' Name        : PiFast 4.1 Wrapper v0.1 *10/2012*
' Author      : El Genieben
' Description : Preventing fake scores in PiFast 4.1 HEXUS edition
'================================================================================
#COMPILE EXE
#INCLUDE "SendInput.Inc"
$INCLUDE "WIN32API.INC"

GLOBAL hPBCC&

SUB pause
    DIM an$
WHILE INSTAT=0
WEND
an$ = INKEY$
        IF an$ = CHR$(27) THEN ' ESCape key pressed.
            END
        ELSE                   ' any other key pressed.
            CLS
            RunPIFAST
            PrintAppHeader
            PrintEnd
        END IF
END SUB
SUB CenterWindow (BYVAL hAnyWnd AS LONG)
  DIM WndRect AS RECT
  DIM x       AS LONG
  DIM y       AS LONG
  GetWindowRect hAnyWnd, WndRect
  x = (GetSystemMetrics(%SM_CXSCREEN)-(WndRect.nRight-WndRect.nLeft))\2
  y = (GetSystemMetrics(%SM_CYSCREEN)-(WndRect.nBottom-WndRect.nTop+GetSystemMetrics(%SM_CYCAPTION)))\2
  SetWindowPos hAnyWnd, %NULL, x, y, 0, 0, %SWP_NOSIZE OR %SWP_NOZORDER
END SUB
SUB PrintAppHeader
        COLOR 14, 9
        STDOUT CHR$(32, 201) + STRING$(47, 205) + CHR$(187, 32)
        STDOUT CHR$(32, 186) + " PIFAST41 Wrapper v0.1 by GENiEBEN             " + CHR$(186, 32)
        STDOUT CHR$(32, 200) + STRING$(47, 205) + CHR$(188, 32)
        COLOR 1, 7
END SUB
SUB RunPIFAST
        pid??? = SHELL("pifast41.exe",1)
        pid??? = SHELL(ENVIRON$("COMSPEC") + " /C > pifast41.txt")
        SendString "0{ENTER}"
        SLEEP 500
        CLS
        SendString "0{ENTER}"
        SLEEP 500
        CLS
        SendString "10000000{ENTER}"
        SLEEP 500
        CLS
        SendString "1024{ENTER}"
        SLEEP 500
        CLS
        SendString "0{ENTER}"
        CLS
END SUB
SUB PrintEnd
        COLOR 15, 0
        STDOUT
        pause
END SUB
FUNCTION PBMAIN () AS LONG
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   hPBCC = CONSHNDL
   DeleteMenu GetSystemMenu (hPBCC,0), %SC_RESTORE,  %MF_BYCOMMAND OR %MF_DISABLED
   'DeleteMenu GetSystemMenu (hPBCC,0), %SC_CLOSE,    %MF_BYCOMMAND OR %MF_DISABLED
   DeleteMenu GetSystemMenu (hPBCC,0), %SC_MINIMIZE, %MF_BYCOMMAND OR %MF_DISABLED
   DeleteMenu GetSystemMenu (hPBCC,0), %SC_MAXIMIZE, %MF_BYCOMMAND OR %MF_DISABLED
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        CONSOLE NAME "PIFAST41 Wrapper"
        CenterWindow hPBCC
        PrintAppHeader
        COLOR 15, 0
        STDOUT
        STDOUT "Press any key to start PIFAST..."
        WAITKEY$
        RunPIFAST
        PrintAppHeader
        PrintEnd
END FUNCTION
