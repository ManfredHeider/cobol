
      *---------------------------------------------------------------*
       RPC-CALL SECTION.

           Perform Check-DISTUTILS-Call
           IF TC-CODE-ERROR = SPACES
           Set TC-RPC-PROG-OK to true
           Move TC-RPCPARM-LENGTH to TC-RPC-DATA-LENGTH
CCCPY1*    Call 'MFREDIR_CALL' using TC-RPC-NAME
CCC004*                                TC-RPC-CTRL-BLOCK
CCC004*                                TC-RPC-NB-PARMS
CCC004*                    by reference RPCPARM
CCCPY2*    End-Call
           Perform Check-RPC-Call
           END-IF
           .
      *---------------------------------------------------------------*

       RPC-Initialize SECTION.

           Move spaces to TC-Code-error TC-DEBUGSTUB
           IF TC-FIRST-CALL
              Set TC-ptr-dll to entry 'USER32'

              display 'MCG_DEBUG'    upon environment-name
              accept TC-DebugStub    from environment-value
              if TC-DebugStub = spaces
                  set tc-ptr-dll       to entry 'MCGen.DLL'
              end-if
              move spaces              to tc-debugstub
              move 'DISTUTLS'          to TC-DISTUTILS
              move 'DISTCAF'           to tc-distcaf
              move '_MFADir'           to tc-mfadir
              Set TC-ptr-mfadir to entry TC-MFADIR
              IF TC-ptr-mfadir = null
                 Move z"MCG ERROR D01" to TC-RPC-TITLE-BOX
                 Move z"_MFADIR.DLL not found."
                      to TC-RPC-MSG-TEXT
                 Perform Send-Message
                 Stop run
              End-if
              Move TC-pgm-name to TC-RPC-PROG-CALLED
              Set TC-DO-NOT-CANCEL to true
              Move zero to TC-RPC-MEMORY-ID
              move 1 to TC-codeset-type *> ASCCI/EBCDIC Translation
              move Length of TC-pgm-name to TC-codeset-length
CCCPY1*       Call '_CODESET' using TC-codeset-type
CCC004*                             TC-codeset-length
CCC004*                             TC-pgm-name
CCCPY2*       End-call
           END-IF
           *> Check if the customer wants to Debug the Stub
           *> We display how the Stub has been generated
           Display 'DEBUGSTUB' upon environment-name
           ACCEPT TC-DEBUGSTUB from environment-value
           IF TC-DEBUGSTUB not = spaces
              Perform DEBUGSTUB
              Perform DISPLAY-STUB
           End-if
           IF TC-DB2-USED = 'Y'
              Move 'CONNECT' To TC-CAF-ACTION
CCCPY1*       CALL TC-DISTCAF USING TC-CAF-ACTION END-CALL
CCC004*    End-IF
CCC004*    IF TC-FIRST-CALL
CCC004*       Set TC-not-first-call to true
CCC004*       Perform Install-Cancel-Entry
CCC004*    END-IF
CCC004*    .

      *---------------------------------------------------------------*
       DISPLAY-STUB SECTION.
CCCPY1*    Call WINAPI "MessageBoxA" using
CCC004*         by value 0 size 4
CCC004*         by reference TC-RPC-MSG-TEXT
CCC004*         by reference "RPC Debug" & x"00"
CCC004*         by value 1 size 4
CCC004*         returning TC-BUTTON
CCCPY2*    End-call
           If TC-BUTTON = 2
              Stop run
           End-if
           .
      *---------------------------------------------------------------*
       Check-DISTUTILS-Call SECTION.

           If TC-CODE-ERROR Not = SPACES
              Initialize TC-RPC-MSG-TEXT
              String "Error " delimited by size
                     TC-CODE-ERROR delimited by size
CCCPY1*              " from DISTUTLS, please call Technical Support."
CCC004*              delimited by size
CCC004*              X"00" delimited by size
CCC004*              into TC-RPC-MSG-TEXT
CCC004*       End-String
CCCPY1*       Call WINAPI "MessageBoxA" using
CCC004*                   by value 0 size 4
CCC004*                   by reference TC-RPC-MSG-TEXT
CCC004*                   by reference "RPC Error" & x"00"
CCC004*                   by value 0 size 4
CCCPY2*       End-call
           End-If
           .

      *---------------------------------------------------------------*
       Check-RPC-Call SECTION.
           Evaluate true
               When TC-RPC-PROG-ABEND
                    Initialize TC-RPC-MSG-TEXT
                    *> Translate Return Code in Abend Message
                    SET TC-ABEND-INDEX TO TC-RPC-RC
                    DIVIDE TC-ABEND-INDEX BY 256 GIVING TC-ABEND-INDEX
                    MULTIPLY TC-ABEND-INDEX BY 256 GIVING TC-ABEND-INDEX
                    SET TC-RPC-RC DOWN BY TC-ABEND-INDEX
                    DIVIDE TC-ABEND-INDEX BY 256 GIVING TC-ABEND-INDEX
                    MOVE TC-ABEND-LIST (1 + TC-ABEND-INDEX:1)
                         TO TC-RPC-ABEND-CODE (1:1)
                    SET TC-ABEND-INDEX TO TC-RPC-RC
                    DIVIDE TC-ABEND-INDEX BY 16 GIVING TC-ABEND-INDEX
                    MULTIPLY TC-ABEND-INDEX BY 16 GIVING TC-ABEND-INDEX
                    SET TC-RPC-RC DOWN BY TC-ABEND-INDEX
                    DIVIDE TC-ABEND-INDEX BY 16 GIVING TC-ABEND-INDEX
                    MOVE TC-ABEND-LIST (1 + TC-ABEND-INDEX:1)
                         TO TC-RPC-ABEND-CODE (2:1)
                    SET TC-ABEND-INDEX TO TC-RPC-RC
                    MOVE TC-ABEND-LIST (1 + TC-ABEND-INDEX:1)
                         TO TC-RPC-ABEND-CODE (3:1)
                    String TC-MVSRPC-ERROR delimited by size
                           'Program ' delimited by size
                           TC-RPC-PROG-CALLED delimited by spaces
                           ' abend ' delimited by size
                           TC-RPC-ABEND-CODE delimited by size
                           X'00' delimited by size
                           into TC-RPC-MSG-TEXT
                    End-String
                    Move z'MVSRPC ERROR OCX' to TC-RPC-TITLE-BOX
                    Move zero to TC-RPC-MEMORY-ID
                    Perform Send-Message
               When TC-RPC-PROG-NOT-FOUND
                    Initialize TC-RPC-MSG-TEXT
                    String TC-MVSRPC-ERROR delimited by size
                           'Program ' delimited by size
                           TC-RPC-PROG-CALLED delimited by spaces
                           ' not found.' delimited by size
                           X'00' delimited by size
                           into TC-RPC-MSG-TEXT
                    End-String
                    Move z'MVSRPC ERROR 806' to TC-RPC-TITLE-BOX
                    Move zero to TC-RPC-MEMORY-ID
                    Perform Send-Message
               When TC-RPC-WRONG-VERSION
                    Initialize TC-RPC-MSG-TEXT
                    String TC-MVSRPC-ERROR delimited by size
                           'Client version of MCG '
                            delimited by size
                        'does not match with mainframe server version.'
                           delimited by size
                           X'00' delimited by size
                           into TC-RPC-MSG-TEXT
                    End-String
                    Move z'MVSRPC ERROR 806' to TC-RPC-TITLE-BOX
                    Move zero to TC-RPC-MEMORY-ID
                    Perform Send-Message
               When other
                    Move TC-RPC-RC to return-code
                    Perform RPC-BACK-TO-CALLER
           End-Evaluate
           .

      *---------------------------------------------------------------*
       SEND-MESSAGE SECTION.
CCCPY1*    Call WINAPI "MessageBoxA" using
CCC004*         by value 0 size 4
CCC004*         by reference TC-RPC-MSG-TEXT
CCC004*         by reference TC-RPC-TITLE-BOX
CCC004*         by value 0 size 4
CCCPY2*    End-call
           .

      *---------------------------------------------------------------*
       EBCDIC-ASCII SECTION.
              Move 0 to TC-codeset-type *> EBCDIC/ASCII
              Move Length of TC-RPC-MSG-TEXT to TC-codeset-length
CCCPY1*       Call '_CODESET' using TC-codeset-type
CCC004*                             TC-codeset-length
CCC004*                             RPCPARM(1:Length of TC-RPC-MSG-TEXT)
CCCPY2*       End-Call.
              Initialize TC-RPC-MSG-TEXT
              Move RPCPARM(1:Length of TC-RPC-MSG-TEXT)
                   to TC-RPC-MSG-TEXT
              Move X"00" to TC-RPC-MSG-TEXT(79:1)
              .

      *---------------------------------------------------------------*
       ASCII-EBCDIC SECTION.
              Move 1 to TC-codeset-type *> ASCII/EBCDIC
              Move Length of TC-RPC-MSG-TEXT to TC-codeset-length
CCCPY1*       Call '_CODESET' using TC-codeset-type
CCC004*                             TC-codeset-length
CCC004*                             RPCPARM(1:Length of TC-RPC-MSG-TEXT)
CCCPY2*       End-Call
              .

      *---------------------------------------------------------------*
       INSTALL-CANCEL-ENTRY SECTION.
              Move zero to TC-Cancel-code
              Move 2 to TC-Flags
              Move length of TC-param-block to TC-size
              Move length of TC-name-buf    to TC-name-len
CCCPY1*       CALL "CBL_GET_PROGRAM_INFO" using
CCC004*                                   by value TC-Cancel-Code
CCC004*                                   by reference TC-param-block
CCC004*                                   by reference TC-name-buf
CCC004*                                   by reference TC-name-len
CCC004*                               RETURNING TC-Cancel-Return-Status
CCCPY2*       END-CALL

              if TC-name-buf(1:TC-name-len) not = "TRANSCAF"
                   Move zero to TC-Cancel-Version
                   Move zero to TC-Cancel-Flags
                   Move zero to TC-Cancel-User-Data-Length
                   Set TC-Cancel-Handle to TC-prog-id
                   set TC-Cancel-User-data to null
CCCPY1*            CALL "CBL_CANCEL_PROC" USING
CCC004*                by value TC-Cancel-Code
CCC004*                by reference TC-Cancel-Block
CCC004*                by value TC-Cancel-User-Data-Length
CCC004*                RETURNING TC-Cancel-Return-Status
CCCPY2*            END-CALL
              end-if
              .

      *---------------------------------------------------------------*
       CLOSE-CONNECTION SECTION.
              IF TC-DB2-USED = 'Y'
                 Move 'DISCONNECT' To TC-CAF-ACTION
CCCPY1*          CALL TC-DISTCAF USING TC-CAF-ACTION END-CALL
CCC004*       End-IF
CCC004*       Set TC-DO-CANCEL to true
CCC004*       Move 0 To TC-RPC-NB-PARMS TC-RPC-DATA-LENGTH
CCC009*       Set address of RPCPARM to null
CCCPY1*       Call 'MFREDIR_CALL' using TC-RPC-NAME
CCC004*                                 TC-RPC-CTRL-BLOCK
CCC004*                                 TC-RPC-NB-PARMS
CCC004*                    by reference RPCPARM
CCCPY2*       End-Call
              Goback
              .
