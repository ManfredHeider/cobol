      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:32:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: AUSGABE ERR-ERROR-BEREICH MIT DISPLAY         * 00002000
      *                 (DISPLAY-ZEILEN)                              * 00002100
      *    --------------------------------------------------------   * 00002200
      *    ERSTELLT: SCHMIDT                                          * 00002300
      *    DATUM:    25.01.1994                                       * 00002400
      ***************************************************************** 00002500
      *    VERSION   01                                              *  00002600
      ****************************************************************  00002700
      * AENDERUNGEN:                                                 *  00002800
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00002900
      * -------------------------------------------------------------*  00003000
      * AFVB00XX!25.01.94! 01 ! NEU                       ! SCHMIDT  *  00004000
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *         !        !    !                           !          *  00010100
      *                                                              *  00010200
      * ENDE-LOGBUCH                                                 *  00010300
      ****************************************************************  00010400
      *                                                                 00011500
      *    DRUCKAUFBEREITETE ERROR-AUSGABE FUER AUSGABE MIT DISPLAY     00011600
      *                                                                 00011800
       01  D-ERROR-BEREICH.                                             00011900
                                                                        00012000
           05  D-ZEILE-01.                                              00012100
               10 FILLER                 PIC  X(040) VALUE              00012200
                           '-----------------   FEHLER AUFGETRETEN  '.  00012300
               10 FILLER                 PIC  X(032) VALUE              00012400
                           '-  FEHLER-ANALYSE   ------------'.          00012500
               10 D-ERR-AKT-DATUM-KURZ   PIC  X(008).                   00012600
                                                                        00012700
           05  D-ZEILE-02.                                              00012800
               10 FILLER                 PIC  X(040) VALUE              00012900
                           'FEHLER-TYP                            : '.  00013000
               10 D-ERR-FEHLER-TYP-TEXT  PIC  X(040).                   00013100
                                                                        00013200
           05  D-ZEILE-03.                                              00013300
               10 FILLER                 PIC  X(040) VALUE              00013400
                           'PROGRAMM-NAME / UPRO-NAME             : '.  00013500
               10 D-ERR-PROGNAME         PIC  X(008).                   00013600
               10 FILLER                 PIC  X(003) VALUE ' / '.       00013700
               10 D-ERR-UPRONAME         PIC  X(008).                   00013800
               10 FILLER                 PIC  X(021).                   00013900
                                                                        00014000
           05  D-ZEILE-04.                                              00014100
               10 FILLER                 PIC  X(040) VALUE              00014200
                           'AUFGETRETEN IN (SECTION, LFD-NR)      : '.  00014300
               10 D-ERR-ORT-SEC          PIC  X(005).                   00014400
               10 FILLER                 PIC  X(001).                   00014500
               10 D-ERR-ORT-LFD          PIC  9(003).                   00014600
               10 FILLER                 PIC  X(031).                   00014700
                                                                        00014800
           05  D-ZEILE-05.                                              00014900
               10 FILLER                 PIC  X(040) VALUE              00015000
                           'SQL-CODE / CICS-RC / FILE-STATUS      : '.  00015100
               10 D-ERR-SQLCODE          PIC  -(009)9.                  00015200
               10 FILLER                 PIC  X(003) VALUE ' / '.       00015300
               10 D-ERR-CICS-RC          PIC  -(004)9.                  00015400
               10 FILLER                 PIC  X(003) VALUE ' / '.       00015500
               10 D-ERR-FILE-STATUS      PIC  X(002).                   00015600
               10 FILLER                 PIC  X(017).                   00015700
                                                                        00015800
           05  D-ZEILE-06.                                              00015900
               10 FILLER                 PIC  X(040) VALUE              00016000
                           'FEHLER-MELDUNG-NR                     : '.  00016100
               10 D-ERR-MELD-KEY.                                       00016200
                   15 D-ERR-MELD-PGM     PIC  X(008).                   00016300
                   15 D-ERR-MELD-TYP     PIC  X(001).                   00016400
                   15 D-ERR-MELD-NR      PIC  9(004).                   00016500
               10 FILLER                 PIC  X(027).                   00016600
                                                                        00016700
           05  D-ZEILE-07.                                              00016800
               10 FILLER                 PIC  X(016) VALUE              00016900
                           'TEXT/SQLERRMC : '.                          00017000
               10 D-ERR-MELD-TEXT        PIC  X(064).                   00017100
                                                                        00017200
           05  D-ZEILE-08.                                              00017300
               10 FILLER                 PIC  X(080).                   00017400
                                                                        00017500
           05  D-ZEILE-09.                                              00018300
               10 FILLER                 PIC  X(040) VALUE              00018400
                           '----------------------------------------'.  00018500
               10 FILLER                 PIC  X(040) VALUE              00018600
                           '----------------------------------------'.  00018700
                                                                        00021001
      *       VAR-BEREICH: 1 ZEILE KOMMENTAR; 4 ZEILEN AUSGABE          00021101
           05  D-ZEILE-10.                                              00021201
               10 FILLER                 PIC  X(045) VALUE              00021301
                      'VARIABLER TRACE-BEREICH (AB NAECHSTE ZEILE) :'.  00021401
               10 FILLER                 PIC  X(035).                   00021501
                                                                        00021601
           05 D-VAR-ZEILE01              PIC  X(080).                   00021701
           05 D-VAR-ZEILE02              PIC  X(080).                   00021801
           05 D-VAR-ZEILE03              PIC  X(080).                   00021901
           05 D-VAR-ZEILE04              PIC  X(080).                   00022001
                                                                        00022701
           05  D-ZEILE-11.                                              00022801
               10 FILLER                 PIC  X(040) VALUE              00022901
                           '---            BITTE HARDCOPY MACHEN UND'.  00023001
               10 FILLER                 PIC  X(040) VALUE              00023101
                           ' DV-ABTEILUNG INFORMIEREN            ---'.  00023201
                                                                        00023301
           05  D-ZEILE-12.                                              00023401
               10 FILLER                 PIC  X(040) VALUE              00023501
                           '----------------------------------------'.  00023601
               10 FILLER                 PIC  X(040) VALUE              00023701
                           '----------------------------------------'.  00023801
      *                                                                 00024200
      *                                                                 00024300
      ******** ENDE COPY-MEMBER D-ERROR-BEREICH (DWAT0027) ************ 00025002
