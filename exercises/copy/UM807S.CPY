      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM807S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM807S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:37:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    20.05.92 11:51:54
      *---------------------------------------------------------------*
      * COPYSTRECKE FUER DIE DATENKOMPRIMIERUNG MIT UPRO MICLZW       *
      *                                                       22.02.91*
      *---------------------------------------------------------------*
      * IN DAS FELD CMP-IN-BUFFER WERDEN DIE ZU KOMPRIMIERENDEN DATEN *
      * EINGESTELLT. IM FELD CMP-IN-BUFFER-LEN MUSS DIE LAENGE DES    *
      * ZU KOMPRIMIERENDEN BEREICHS EINGESTELLT WERDEN.               *
      *                                                               *
      * WIRD DAS UP ZUM ERSTEN MAL AUFGERUFEN, IST DAS FELD           *
      * 'FIRST-TIME' MIT EINER '1' ZU BELEGEN.                        *
      * (BEI FOLGEAUFRUFEN IST DARAUF ZU ACHTEN, DASS DAS FELD AUF '0'*
      *  GESETZT WIRD).                                               *
      * WERDEN NOCH WEITERE BLÖCKE ÜBERGEBEN, SO IST DAS FELD         *
      * 'LAST-TIME' AUF VALUE '0' ZU SETZEN.                          *
      * ERST BEI DER ÜBERGABE DES LETZTEN DATENBLOCKS IST DER INHALT  *
      * DIESER VARIABLEN AUF '1' ZU SETZEN.                           *
      *---------------------------------------------------------------*
      * DAS FELD 'FILE-NAMEN' IST GLEICHBEDEUTEND MIT DEM FELD        *
      * 'FL-NAMEN' AUS FILELINK, UND BEIM LETZTEN AUFRUF ZU FUELLEN   *
      *---------------------------------------------------------------*
      * DAS FELD 'WHAT-TO-DO'  IST MIT EINEM ENTSPRECHENDEN WERT ZU   *
      * BELEGEN (SIEHE 88- MERKNAMEN)                                 *
      *---------------------------------------------------------------*


      *01  LZW-PARAMETER.
      *
      *     EINGABEBEREICH MIT LANEGE DES EINGABEBEREICHS
      *
           05  IN-BUFFER.
               10 IN-BUFFER-BYTE           PIC X   OCCURS 10000.
           05  IN-BUFFER-LEN               PIC S9(5)      COMP-4.
      *
      *     AUSGABEBEREICH MIT LANEGE DES AUSGABEBEREICHS
      *
           05  OUT-BUFFER.
               10 OUT-BUFFER-BYTE          PIC X   OCCURS 10000.
           05  OUT-BUFFER-LEN              PIC S9(5)      COMP-4.
      *
      *     BEIM ERSTEN AUFRUF AUF 1 SETZEN, ANSONSTEN ZERO.
      *
           05  FIRST-TIME                  PIC 9(5)        COMP-4.
      *
      *     BEIM LETZTEN AUFRUF AUF 1 SETZEN, ANSONSTEN ZERO.
      *
           05  LAST-TIME                   PIC 9(5)        COMP-4.
      *
      *     WIRD DER EINGABEPUFFER DEKOMPRIMIERT, GIBT DAS FLAG
      *     CALL-AGAIN AN, OB DIE FUNKTION NOCH EINMAL MIT DEM
      *     GLEICHEN EINGABEBEREICH AUFGERUFEN WERDEN MUSS.
      *     (AUF 1 GESETZT : NOCHMAL AUFRUFEN, ANSONSTEN ZERO)
      *
           05  CALL-AGAIN                  PIC 9(5)        COMP-4.
      *
      *     WHAT-TO-DO GIBT AN, WAS GEMACHT WERDEN SOLL :
      *
           05  WHAT-TO-DO                  PIC 9(5)        COMP-4.
      *         ->  EINFACH KOMPRIMIEREN ODER DEKOMPRIMIEREN
               88 LZW-COMP             VALUE 1.
               88 LZW-DECOMP           VALUE 4.
      *         ->  UEBERSETZEN ASCII NACH EBCDIC ODER UMGEKEHRT
               88 LZW-TRANSLATE-E-A    VALUE 2.
               88 LZW-TRANSLATE-A-E    VALUE 5.
      *         ->  KOMPRIMIEREN UND
      *         ->  UEBERSETZEN ASCII NACH EBCDIC ODER UMGEKEHRT
               88 LZW-COMP-TRAN-E-A    VALUE 3.
               88 LZW-COMP-TRAN-A-E    VALUE 7.
      *         ->  DEKOMPRIMIEREN UND
      *         ->  UEBERSETZEN ASCII NACH EBCDIC ODER UMGEKEHRT
               88 LZW-DECOMP-TRAN-E-A  VALUE 8.
               88 LZW-DECOMP-TRAN-A-E  VALUE 9.
      *
      *     DUMMY FUER EINEN C-FILE POINTER
      *
           05  IN-FILE-PTR                 PIC X(4).
           05  IN-FILE-NAME                PIC X(256).
           05  IN-FILE-LEN                 PIC 9(8)        COMP-4.
           05  IN-BUFFER-POS               PIC S9(5)       COMP-4.
      *
      *     DUMMY FUER EINEN C-FILE POINTER
      *
           05  OUT-FILE-PTR                PIC X(4).
           05  OUT-FILE-NAME               PIC X(256).
           05  OUT-FILE-LEN                PIC 9(8)        COMP-4.
           05  OUT-BUFFER-POS              PIC S9(5)       COMP-4.
      *
           05  SEND-COUNT                  PIC S9(5)      COMP-4.
           05  RECEIVE-COUNT               PIC S9(5)      COMP-4.
      *
           05  SILENT-FLAG                 PIC S9(5)       COMP-4.
           05  COMP-DECOMP-FLAG            PIC S9(5)       COMP-4.
           05  TRANSLATION-FLAG            PIC S9(5)       COMP-4.
      *
      *     DUMMY FUER EINEN C-FILE POINTER
      *
           05  HEADER-PTR                  PIC X(4).
      *
           05  PREDECESSORS       PIC S9(5)   COMP-4   OCCURS 3840.
           05  TABLECHARS         PIC X                OCCURS 3840.
           05  LEFT-R             PIC S9(5)   COMP-4   OCCURS 3840.
           05  RIGHT-R            PIC S9(5)   COMP-4   OCCURS 3840.
      *
           05  CODE-R                      PIC S9(5)       COMP-4.
           05  PREFIX                      PIC S9(5)       COMP-4.
           05  GOT-CHAR-FLAG               PIC S9(5)       COMP-4.
           05  KEEP-CHAR-FLAG              PIC S9(5)       COMP-4.
      *
           05  TABLE-OFFSET                PIC S9(5)       COMP-4.
      *
           05  DECOMP-STRING      PIC X(4096).
CCC009*EJECT
