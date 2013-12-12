'===============================================================================
' SendInput.inc
' 2009 - 2012 - Eddy Van Esch
'
' History:
' --------
' V1.0: 14-May-2009 : Initial version
'
' V1.1: 27-May-2009 :
'     - Fixed a bug where SHIFT_D with HOME or an arrow key combination produced
'       incorrect results. The shift key appeared to be ignored.
'       Thanks to S. Stamp for reporting the problem.
'
'       Apparently, the HOME and arrow keys (and a number of others) are extended keys and
'       they require the extended key flag to be set.
'       A forum thread about the problem:
'       http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_21737163.html
'     - Added more special keys (see list below)
'     - SUB WaitForNoKeys slightly modified
'
' V1.2: 17-June-2009 :
'     - Added special keys: {CTRL}, {ALT}, {SHIFT}, {WIN}
'       In contrast with the {.._D}, {.._U} keys that can be used with multiple keys in between,
'       the above keys are used with one character at the time only, like this:
'       "{CTRL_D}ac{CTRL_U}" is equivalent to "{CTRL}a{CTRL}c"
'     - Added sub "UniCode_OnOff" to turn UniCode on or off.
'       UniCode_OnOff(1) will make SendString send unicode keystrokes.
'       UniCode_OnOff(0) will make SendString send ansi keystrokes.
'
' V1.3: 09-June-2012 :
'     Some modifications needed to be able to compile with PBWin 10.
'     - INPUTAPI type def is changed in Win32API.
'     - Changed: i = SendInput(lBufCnt+1, inpAPI(0), SIZEOF(inpAPI(0)))
'       into:    i = SendInput(lBufCnt+1, VARPTR(inpAPI(0)), SIZEOF(inpAPI(0)))
'     - VkKeyScanEx replaced by VkKeyScanExW and VkKeyScanExA
'
' ---------------------------------------------------------------------------
' This software allows to send keystrokes to the application that
' currently has focus.
' Rather than using KEYBD_EVENT, this routine uses SENDINPUT.
' The advantage is that when using SendInput, the sent keystrokes can not
' be accidently mixed with keystrokes that the user is entering on the keyboard.
' Also, there is no problem with timings between multiple Keybd_Event commands.
' This routine also handles characters with diacritics, like é,è,ê,ñ, etc. (See **)
' Writing this code, I took inspiration (and a few lines of code :-)) from
' William Burns' file 'SendKeys.inc'
' Not all possible special keys are implemented yet, but it is easy to add more.
'
'Sub WaitForNoKeys was reused from William Burns' file 'SendKeys.inc'. Thanks William!
'
'----------------------------
'**  IMPORTANT !!!
'Although SendString should handle strings with diacritics properly, other active programs
'can mess this up. Especially programs that use a global keyboard hook, such
'as AutoHotKey for example.
'The reason is that these programs use the ToAscii(Ex) API function in their keyboard hook.
'Using ToAscii(Ex) in a hook messes up diacritics handling.
'What you then will notice is an inconsistent behaviour when sending characters with diacritics.
'For example, sending "éééé" could result in "eeéé", "''eé" or some other inconsistent result.
'Very annoying. The only solution is to terminate the application that is using the global hook.
'----------------------------
'
'===============================================================================

#IF NOT %DEF(%WINAPI)
    #INCLUDE "win32api.inc"                        'add win32api if not already added
#ENDIF

GLOBAL glSendString_UniCode AS LONG


'-------------------------------------------------------------------------
'Turn unicode on or off. This can be changed on the fly. SendString will send keystrokes in unicode or
'ansi, depending on the flag set by this sub.
'INPUT: 0 : Turn unicode off
'       1 : Turn unicode on
SUB UniCode_OnOff(lOn AS LONG)
    IF lOn THEN
        glSendString_UniCode = %KEYEVENTF_UNICODE
    ELSE
        glSendString_UniCode = 0
    END IF
END SUB
'-------------------------------------------------------------------------



'-------------------------------------------------------------------------
'This sub places the characters in 'sString' in the keyboard buffer. Then, the characters are sent to
'whatever application currently has focus.
'So, this sub allows to simulate keyboard input.
'Apart from normal ascii characters, SendString can also handle some special characters:
'
'The special characters currently implemented are:
'{CTRL_D}, {CTRL_U}, {ALT_D}, {ALT_U}, {SHIFT_D}, {SHIFT_U}, {WIN_D}, {WIN_U}
'{CTRL}, {ALT}, {SHIFT}, {WIN}
'{BACKSPACE}, {ENTER}, {INSERT}, {DELETE}, {RIGHT}, {LEFT}, {UP}, {DOWN}, {HOME}, {END}, {TAB}, {ESCAPE}
'{PGUP}, {PGDN}, {F1}, {F2}, {F3}, {F4}, {F5}, {F6}, {F7}, {F8}, {F9}, {F10}, {F11}, {F12}
'{APPS}, {NUMLOCK}
'The {.._D} are the 'key down' codes. The {.._U} are the 'key up' codes.
'
'Examples of use:
'SendString "This is a TEST"
'SendString "12{BACKSPACE}{BACKSPACE}abcdef{ENTER}"
'SendString "{CTRL_D}ac{CTRL_U}" simulates a CTRL-a  CTRL-c key combination. In most applications this copies the selected data.
'SendString "{CTRL}a{CTRL}c" simulates a CTRL-a  CTRL-c key combination. In most applications this copies the selected data.
%TimeIncr = 1
SUB SendString(BYVAL sString AS STRING)
    LOCAL  i                     AS LONG
    LOCAL  lBufCnt               AS DWORD
    LOCAL  lFetchingSpecialChar  AS LONG
    LOCAL  lSpecialKeyDown_Ctrl  AS LONG
    LOCAL  lSpecialKeyDown_Shift AS LONG
    LOCAL  lSpecialKeyDown_Alt   AS LONG
    LOCAL  lSpecialKeyDown_Win   AS LONG
    LOCAL  sChar                 AS STRING
    LOCAL  sSpecialChar          AS STRING
    LOCAL  iRet                  AS INTEGER
    LOCAL  dKeybLayout           AS DWORD
    LOCAL  bVkCode               AS BYTE
    LOCAL  bShiftState           AS BYTE
    STATIC Time                  AS DWORD


    LOCAL inpAPI() AS INPUTAPI
    REDIM inpAPI(0 TO 20)

    IF sString = "" THEN EXIT SUB

    dKeybLayout = GETKEYBOARDLAYOUT(0)
    lBufCnt = -1
    Time = 1

            'Replace some characters with diacritics by multiple characters.
    REPLACE "è" WITH "`e" IN sString
    REPLACE "é" WITH "'e" IN sString
    REPLACE "ê" WITH "^e" IN sString
    REPLACE "ë" WITH $DQ + "e" IN sString

    REPLACE "È" WITH "`E" IN sString
    REPLACE "É" WITH "'E" IN sString
    REPLACE "Ê" WITH "^E" IN sString
    REPLACE "Ë" WITH $DQ + "E" IN sString

    REPLACE "ì" WITH "`i" IN sString
    REPLACE "í" WITH "'i" IN sString
    REPLACE "î" WITH "^i" IN sString
    REPLACE "ï" WITH $DQ + "i" IN sString

    REPLACE "Ì" WITH "`I" IN sString
    REPLACE "Í" WITH "'I" IN sString
    REPLACE "Î" WITH "^I" IN sString
    REPLACE "Ï" WITH $DQ + "I" IN sString

    REPLACE "à" WITH "`a" IN sString
    REPLACE "á" WITH "'a" IN sString
    REPLACE "â" WITH "^a" IN sString
    REPLACE "ä" WITH $DQ + "a" IN sString
    REPLACE "ã" WITH "~a" IN sString

    'REPLACE "å" WITH "°a" IN sString
    REPLACE "À" WITH "`A" IN sString
    REPLACE "Á" WITH "'A" IN sString
    REPLACE "Â" WITH "^A" IN sString
    REPLACE "Ã" WITH "~A" IN sString
    REPLACE "Ä" WITH $DQ + "A" IN sString

    REPLACE "ò" WITH "`o" IN sString
    REPLACE "ó" WITH "'o" IN sString
    REPLACE "ô" WITH "^o" IN sString
    REPLACE "ö" WITH $DQ + "o" IN sString
    REPLACE "õ" WITH "~o" IN sString

    REPLACE "Ò" WITH "`O" IN sString
    REPLACE "Ó" WITH "'O" IN sString
    REPLACE "Ô" WITH "^O" IN sString
    REPLACE "Ö" WITH $DQ + "O" IN sString
    REPLACE "Õ" WITH "~O" IN sString

    REPLACE "Ù" WITH "`U" IN sString
    REPLACE "Ú" WITH "'U" IN sString
    REPLACE "Û" WITH "^U" IN sString
    REPLACE "Ü" WITH $DQ + "U" IN sString

    REPLACE "ù" WITH "`u" IN sString
    REPLACE "ú" WITH "'u" IN sString
    REPLACE "û" WITH "^u" IN sString
    REPLACE "ü" WITH $DQ + "u" IN sString

    REPLACE "ñ" WITH "~n" IN sString
    REPLACE "Ñ" WITH "~N" IN sString

    REPLACE "ç" WITH "'c" IN sString
    REPLACE "Ç" WITH "'C" IN sString

    REPLACE "ý" WITH "'y" IN sString
    REPLACE "ÿ" WITH $DQ + "y" IN sString

    REPLACE "Ý" WITH "'Y" IN sString
    REPLACE "Ÿ" WITH $DQ + "Y" IN sString


    lFetchingSpecialChar = 0
    FOR i = 1 TO LEN(sString)                           'Parse the input string character by character
        sChar = MID$(sString, i, 1)
        SELECT CASE sChar
            CASE "{"                                    'Start of a special-character sequence
                lFetchingSpecialChar = 1
                sSpecialChar = ""

            CASE "}"                                    'End of a special-character sequence
                lFetchingSpecialChar = 0
                GOSUB AddSpecialCharToBuffer

            CASE ELSE
                IF lFetchingSpecialChar THEN
                    sSpecialChar = sSpecialChar + sChar 'Compose the special character string
                ELSE
                    GOSUB AddNormalCharToBuffer
                END IF

        END SELECT
    NEXT i

    REDIM PRESERVE inpAPI(0 TO lBufCnt)
    'i = SendInput(lBufCnt+1, inpAPI(0), SIZEOF(inpAPI(0)))
    i = SendInput(lBufCnt+1, VARPTR(inpAPI(0)), SIZEOF(inpAPI(0)))

EXIT SUB

AddSpecialCharToBuffer:
            'Special characters (such as ENTER, BACKSPACE, CTRL-down, etc) that can not be represented
            'by an ASCII character, are defined here.
    sSpecialChar = UCASE$(sSpecialChar)
    SELECT CASE sSpecialChar
        CASE "CTRL"      : lSpecialKeyDown_Ctrl  = 1
        CASE "ALT"       : lSpecialKeyDown_Alt   = 1
        CASE "SHIFT"     : lSpecialKeyDown_Shift = 1
        CASE "WIN"       : lSpecialKeyDown_Win   = 1

        CASE "CTRL_D"    : bVkCode = %VK_CONTROL : GOSUB bVkCode_Down
        CASE "CTRL_U"    : bVkCode = %VK_CONTROL : GOSUB bVkCode_Up
        CASE "ALT_D"     : bVkCode = %VK_MENU    : GOSUB bVkCode_Down
        CASE "ALT_U"     : bVkCode = %VK_MENU    : GOSUB bVkCode_Up
        CASE "SHIFT_D"   : bVkCode = %VK_SHIFT   : GOSUB bVkCode_Down
        CASE "SHIFT_U"   : bVkCode = %VK_SHIFT   : GOSUB bVkCode_Up
        CASE "WIN_D"     : bVkCode = %VK_LWIN    : GOSUB bVkCode_Down
        CASE "WIN_U"     : bVkCode = %VK_LWIN    : GOSUB bVkCode_Up

        CASE "BACKSPACE" : bVkCode = %VK_BACK    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "ENTER"     : bVkCode = %VK_RETURN  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "INSERT"    : bVkCode = %VK_INSERT  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "DELETE"    : bVkCode = %VK_DELETE  : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "RIGHT"     : bVkCode = %VK_RIGHT   : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "LEFT"      : bVkCode = %VK_LEFT    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "UP"        : bVkCode = %VK_UP      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "DOWN"      : bVkCode = %VK_DOWN    : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "HOME"      : bVkCode = %VK_HOME    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "END"       : bVkCode = %VK_END     : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "TAB"       : bVkCode = %VK_TAB     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "ESCAPE"    : bVkCode = %VK_ESCAPE  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "PGUP"      : bVkCode = %VK_PGUP    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "PGDN"      : bVkCode = %VK_PGDN    : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "F1"        : bVkCode = %VK_F1      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F2"        : bVkCode = %VK_F2      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F3"        : bVkCode = %VK_F3      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F4"        : bVkCode = %VK_F4      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F5"        : bVkCode = %VK_F5      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F6"        : bVkCode = %VK_F6      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F7"        : bVkCode = %VK_F7      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F8"        : bVkCode = %VK_F8      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F9"        : bVkCode = %VK_F9      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F10"       : bVkCode = %VK_F10     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F11"       : bVkCode = %VK_F11     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F12"       : bVkCode = %VK_F12     : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "APPS"      : bVkCode = %VK_APPS    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "NUMLOCK"   : bVkCode = %VK_NUMLOCK : GOSUB bVkCode_Down : GOSUB bVkCode_Up

                        'Caps lock
        CASE "CAPITAL"   : bVkCode = %VK_CAPITAL : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE ELSE
    END SELECT

RETURN

bVkCode_Down:       'key 'bVkCode' down
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)
    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

            'Intercept extended keys. They require the extended key flag.
            '
    SELECT CASE bVkCode
        CASE %VK_UP, %VK_DOWN, %VK_LEFT, %VK_RIGHT, %VK_HOME, %VK_END, %VK_INSERT, %VK_DELETE, %VK_PGUP, %VK_PGDN
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_EXTENDEDKEY OR glSendString_UniCode
        CASE ELSE
            inpAPI(lBufCnt).iki.dwFlags = 0 OR glSendString_UniCode
    END SELECT
RETURN


bVkCode_Up:         'key 'bVkCode' up
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)
    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

            'Intercept extended keys. They require the extended key flag.
    SELECT CASE bVkCode
        CASE %VK_UP, %VK_DOWN, %VK_LEFT, %VK_RIGHT, %VK_HOME, %VK_END, %VK_INSERT, %VK_DELETE, %VK_PGUP, %VK_PGDN
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_KEYUP OR %KEYEVENTF_EXTENDEDKEY OR glSendString_UniCode
        CASE ELSE
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_KEYUP OR glSendString_UniCode
    END SELECT
RETURN



AddNormalCharToBuffer:
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)

    IF glSendString_UniCode = %KEYEVENTF_UNICODE THEN
        iRet = VkKeyScanExW(ASC(sChar), dKeybLayout)
    ELSE
        iRet = VkKeyScanExA(ASC(sChar), dKeybLayout)
    END IF

    bVkCode     = LO(BYTE, iRet)
    bShiftState = HI(BYTE, iRet)
    IF (bVkCode = 255) AND (bShiftState = 255) THEN RETURN  'Cant do this char so skip

    IF (BIT(bShiftState, 0)) OR lSpecialKeyDown_Shift THEN     'Shift
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_SHIFT
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_SHIFT, 0)
    END IF

    IF (BIT(bShiftState, 1)) OR lSpecialKeyDown_Ctrl THEN     'Ctrl
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_CONTROL
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_CONTROL, 0)
    END IF

    IF (BIT(bShiftState, 2)) OR lSpecialKeyDown_Alt THEN     'Alt
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_MENU
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_MENU, 0)
    END IF

    IF lSpecialKeyDown_Win THEN     'Win
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_LWIN
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_LWIN, 0)
    END IF

    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dwFlags      = 0 OR glSendString_UniCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR glSendString_UniCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)


    IF (BIT(bShiftState, 0)) OR lSpecialKeyDown_Shift THEN     'Shift
        lSpecialKeyDown_Shift = 0
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_SHIFT
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_SHIFT, 0)
    END IF

    IF (BIT(bShiftState, 1)) OR lSpecialKeyDown_Ctrl THEN     'Ctrl
        lSpecialKeyDown_Ctrl  = 0
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_CONTROL
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_CONTROL, 0)
    END IF

    IF (BIT(bShiftState, 2)) OR lSpecialKeyDown_Alt THEN     'Alt
        lSpecialKeyDown_Alt   = 0
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_MENU
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_MENU, 0)
    END IF

    IF lSpecialKeyDown_Win THEN     'Win
        lSpecialKeyDown_Win   = 0
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_LWIN
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR glSendString_UniCode
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_LWIN, 0)
    END IF
RETURN

END SUB
'-------------------------------------------------------------------------


'-------------------------------------------------------------------------
'Sub borrowed from SendKeys.inc - by William Burns
'Wait until user lets go of the ctrl/Alt/Shift and any other keys
SUB WaitForNoKeys
    LOCAL KeyWasPressed  AS LONG
    LOCAL iKey           AS LONG
    DO                'loop here until user lets go of the ctrl/Alt/Shift and any other keys
        SLEEP 1                       '<-- Modified in V1.1
        KeyWasPressed = 0
        'FOR iKey = 19 TO 255         '0-9 and a-z
        FOR iKey = 0 TO 255           '<-- Modified in V1.1
            IF (GETASYNCKEYSTATE(iKey) AND &H8000) THEN
                KeyWasPressed = 1
                EXIT FOR
            END IF
        NEXT iKey
        IF (GETASYNCKEYSTATE(%VK_CONTROL) AND &H8000) OR (GETASYNCKEYSTATE(%VK_SHIFT) AND &H8000) OR (GETASYNCKEYSTATE(%VK_MENU) AND &H8000) THEN KeyWasPressed = 1  'note: %VK_MENU = %VK_ALT
    LOOP WHILE KeyWasPressed
END SUB
'-------------------------------------------------------------------------


'-------------------------------------------------------------------------
SUB SendString_Uni(BYVAL sString AS STRING)
    LOCAL i                    AS LONG
    LOCAL lBufCnt              AS LONG
    LOCAL lFetchingSpecialChar AS LONG
    LOCAL sChar                AS STRING
    LOCAL sSpecialChar         AS STRING
    LOCAL iRet                 AS INTEGER
    LOCAL dKeybLayout          AS DWORD
    LOCAL bVkCode              AS BYTE
    LOCAL bShiftState          AS BYTE
    LOCAL Time                 AS DWORD
    LOCAL inpAPI() AS INPUTAPI
    REDIM inpAPI(0 TO 20)

    IF sString = "" THEN EXIT SUB

    dKeybLayout = GETKEYBOARDLAYOUT(0)
    lBufCnt = -1
    Time = 10

            'Replace some characters with diacritics by multiple characters.
    REPLACE "è" WITH "`e" IN sString
    REPLACE "é" WITH "'e" IN sString
    REPLACE "ê" WITH "^e" IN sString
    REPLACE "ë" WITH $DQ + "e" IN sString

    REPLACE "È" WITH "`E" IN sString
    REPLACE "É" WITH "'E" IN sString
    REPLACE "Ê" WITH "^E" IN sString
    REPLACE "Ë" WITH $DQ + "E" IN sString

    REPLACE "ì" WITH "`i" IN sString
    REPLACE "í" WITH "'i" IN sString
    REPLACE "î" WITH "^i" IN sString
    REPLACE "ï" WITH $DQ + "i" IN sString

    REPLACE "Ì" WITH "`I" IN sString
    REPLACE "Í" WITH "'I" IN sString
    REPLACE "Î" WITH "^I" IN sString
    REPLACE "Ï" WITH $DQ + "I" IN sString

    REPLACE "à" WITH "`a" IN sString
    REPLACE "á" WITH "'a" IN sString
    REPLACE "â" WITH "^a" IN sString
    REPLACE "ä" WITH $DQ + "a" IN sString
    REPLACE "ã" WITH "~a" IN sString

    'REPLACE "å" WITH "°a" IN sString
    REPLACE "À" WITH "`A" IN sString
    REPLACE "Á" WITH "'A" IN sString
    REPLACE "Â" WITH "^A" IN sString
    REPLACE "Ã" WITH "~A" IN sString
    REPLACE "Ä" WITH $DQ + "A" IN sString

    REPLACE "ò" WITH "`o" IN sString
    REPLACE "ó" WITH "'o" IN sString
    REPLACE "ô" WITH "^o" IN sString
    REPLACE "ö" WITH $DQ + "o" IN sString
    REPLACE "õ" WITH "~o" IN sString

    REPLACE "Ò" WITH "`O" IN sString
    REPLACE "Ó" WITH "'O" IN sString
    REPLACE "Ô" WITH "^O" IN sString
    REPLACE "Ö" WITH $DQ + "O" IN sString
    REPLACE "Õ" WITH "~O" IN sString

    REPLACE "Ù" WITH "`U" IN sString
    REPLACE "Ú" WITH "'U" IN sString
    REPLACE "Û" WITH "^U" IN sString
    REPLACE "Ü" WITH $DQ + "U" IN sString

    REPLACE "ù" WITH "`u" IN sString
    REPLACE "ú" WITH "'u" IN sString
    REPLACE "û" WITH "^u" IN sString
    REPLACE "ü" WITH $DQ + "u" IN sString

    REPLACE "ñ" WITH "~n" IN sString
    REPLACE "Ñ" WITH "~N" IN sString

    REPLACE "ç" WITH "'c" IN sString
    REPLACE "Ç" WITH "'C" IN sString

    REPLACE "ý" WITH "'y" IN sString
    REPLACE "ÿ" WITH $DQ + "y" IN sString

    REPLACE "Ý" WITH "'Y" IN sString
    REPLACE "Ÿ" WITH $DQ + "Y" IN sString


    lFetchingSpecialChar = 0
    FOR i = 1 TO LEN(sString)                           'Parse the input string character by character
        sChar = MID$(sString, i, 1)
        SELECT CASE sChar
            CASE "{"                                    'Start of a special-character sequence
                lFetchingSpecialChar = 1
                sSpecialChar = ""

            CASE "}"                                    'End of a special-character sequence
                lFetchingSpecialChar = 0
                GOSUB AddSpecialCharToBuffer

            CASE ELSE
                IF lFetchingSpecialChar THEN
                    sSpecialChar = sSpecialChar + sChar 'Compose the special character string
                ELSE
                    GOSUB AddNormalCharToBuffer
                END IF

        END SELECT
    NEXT i

    REDIM PRESERVE inpAPI(0 TO lBufCnt)
    'i = SENDINPUT(lBufCnt+1, inpAPI(0), SIZEOF(inpAPI(0)))
    i = SendInput(lBufCnt+1, VARPTR(inpAPI(0)), SIZEOF(inpAPI(0)))

EXIT SUB

AddSpecialCharToBuffer:
            'Special characters (such as ENTER, BACKSPACE, CTRL-down, etc) that can not be represented
            'by an ASCII character, are defined here.
    sSpecialChar = UCASE$(sSpecialChar)
    SELECT CASE sSpecialChar
        CASE "CTRL_D"    : bVkCode = %VK_CONTROL : GOSUB bVkCode_Down
        CASE "CTRL_U"    : bVkCode = %VK_CONTROL : GOSUB bVkCode_Up
        CASE "ALT_D"     : bVkCode = %VK_MENU    : GOSUB bVkCode_Down
        CASE "ALT_U"     : bVkCode = %VK_MENU    : GOSUB bVkCode_Up
        CASE "SHIFT_D"   : bVkCode = %VK_SHIFT   : GOSUB bVkCode_Down
        CASE "SHIFT_U"   : bVkCode = %VK_SHIFT   : GOSUB bVkCode_Up
        CASE "WIN_D"     : bVkCode = %VK_LWIN    : GOSUB bVkCode_Down
        CASE "WIN_U"     : bVkCode = %VK_LWIN    : GOSUB bVkCode_Up

        CASE "BACKSPACE" : bVkCode = %VK_BACK    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "ENTER"     : bVkCode = %VK_RETURN  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "INSERT"    : bVkCode = %VK_INSERT  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "DELETE"    : bVkCode = %VK_DELETE  : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "RIGHT"     : bVkCode = %VK_RIGHT   : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "LEFT"      : bVkCode = %VK_LEFT    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "UP"        : bVkCode = %VK_UP      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "DOWN"      : bVkCode = %VK_DOWN    : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "HOME"      : bVkCode = %VK_HOME    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "END"       : bVkCode = %VK_END     : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "TAB"       : bVkCode = %VK_TAB     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "ESCAPE"    : bVkCode = %VK_ESCAPE  : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "PGUP"      : bVkCode = %VK_PGUP    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "PGDN"      : bVkCode = %VK_PGDN    : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "F1"        : bVkCode = %VK_F1      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F2"        : bVkCode = %VK_F2      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F3"        : bVkCode = %VK_F3      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F4"        : bVkCode = %VK_F4      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F5"        : bVkCode = %VK_F5      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F6"        : bVkCode = %VK_F6      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F7"        : bVkCode = %VK_F7      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F8"        : bVkCode = %VK_F8      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F9"        : bVkCode = %VK_F9      : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F10"       : bVkCode = %VK_F10     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F11"       : bVkCode = %VK_F11     : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "F12"       : bVkCode = %VK_F12     : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE "APPS"      : bVkCode = %VK_APPS    : GOSUB bVkCode_Down : GOSUB bVkCode_Up
        CASE "NUMLOCK"   : bVkCode = %VK_NUMLOCK : GOSUB bVkCode_Down : GOSUB bVkCode_Up

                        'Caps lock
        CASE "CAPITAL"   : bVkCode = %VK_CAPITAL : GOSUB bVkCode_Down : GOSUB bVkCode_Up

        CASE ELSE
    END SELECT

RETURN

bVkCode_Down:       'key 'bVkCode' down
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)
    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

            'Intercept extended keys. They require the extended key flag.
            '
    SELECT CASE bVkCode
        CASE %VK_UP, %VK_DOWN, %VK_LEFT, %VK_RIGHT, %VK_HOME, %VK_END, %VK_INSERT, %VK_DELETE, %VK_PGUP, %VK_PGDN
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_EXTENDEDKEY OR %KEYEVENTF_UNICODE
        CASE ELSE
            inpAPI(lBufCnt).iki.dwFlags = 0  OR %KEYEVENTF_UNICODE
    END SELECT
RETURN


bVkCode_Up:         'key 'bVkCode' up
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)
    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

            'Intercept extended keys. They require the extended key flag.
    SELECT CASE bVkCode
        CASE %VK_UP, %VK_DOWN, %VK_LEFT, %VK_RIGHT, %VK_HOME, %VK_END, %VK_INSERT, %VK_DELETE, %VK_PGUP, %VK_PGDN
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_KEYUP OR %KEYEVENTF_EXTENDEDKEY OR %KEYEVENTF_UNICODE
        CASE ELSE
            inpAPI(lBufCnt).iki.dwFlags = %KEYEVENTF_KEYUP OR %KEYEVENTF_UNICODE
    END SELECT
RETURN



AddNormalCharToBuffer:
    IF (UBOUND(inpAPI()) - lBufCnt) < 5 THEN REDIM PRESERVE inpAPI(lBufCnt + 16)

    IF glSendString_UniCode = %KEYEVENTF_UNICODE THEN
        iRet = VkKeyScanExW(ASC(sChar), dKeybLayout)
    ELSE
        iRet = VkKeyScanExA(ASC(sChar), dKeybLayout)
    END IF

    bVkCode     = LO(BYTE, iRet)
    bShiftState = HI(BYTE, iRet)
    IF (bVkCode = 255) AND (bShiftState = 255) THEN RETURN  'Cant do this char so skip

    IF BIT(bShiftState, 0) THEN     'Shift
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_SHIFT
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_SHIFT, 0)
    END IF

    IF BIT(bShiftState, 1) THEN     'Ctrl
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_CONTROL
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_CONTROL, 0)
    END IF

    IF BIT(bShiftState, 2) THEN     'Alt
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_MENU
        inpAPI(lBufCnt).iki.dwFlags      = 0 OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_MENU, 0)
    END IF

    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dwFlags      = 0 OR %KEYEVENTF_UNICODE
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)

    INCR lBufCnt
    inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
    inpAPI(lBufCnt).iki.wVk          = bVkCode
    inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR %KEYEVENTF_UNICODE
    inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
    inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(bVkCode, 0)


    IF BIT(bShiftState, 0) THEN     'Shift
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_SHIFT
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_SHIFT, 0)
    END IF

    IF BIT(bShiftState, 1) THEN     'Ctrl
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_CONTROL
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_CONTROL, 0)
    END IF

    IF BIT(bShiftState, 2) THEN     'Alt
        INCR lBufCnt
        inpAPI(lBufCnt).dtype            = %INPUT_KEYBOARD
        inpAPI(lBufCnt).iki.wVk          = %VK_MENU
        inpAPI(lBufCnt).iki.dwFlags      = %KEYEVENTF_KEYUP OR %KEYEVENTF_UNICODE
        inpAPI(lBufCnt).iki.dtime        = Time : Time = Time + %TimeIncr
        inpAPI(lBufCnt).iki.wScan        = MAPVIRTUALKEY(%VK_MENU, 0)
    END IF
RETURN

END SUB
'-------------------------------------------------------------------------
