      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:03:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: (DRVB0006)                                    * 00020000
      *    ARBEITSDATENFELDER/PARAMETER FÜR SCHNITTSTELLENMODULE      * 00020000
      *    SM1, SM2, SM7, SM10 FÜR ONLINE-ALTANWENDUNGEN              * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: O. NAUMANN                                       * 00060000
      *    DATUM:    26.02.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   01   16.09.93                                   *  00090101
      ****************************************************************  00091000
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00095002
      *         !        !    !                           !          *  00096001
      *         !        !    !                           !          *  00097001
      *         !        !    !                           !          *  00098001
      *         !        !    !                           !          *  00099000
      *         !        !    !                           !          *  00099100
      *         !        !    !                           !          *  00099200
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600
      *-------------------------------------------------------------
      *--ARBEITSDATENFELDER
      *-------------------------------------------------------------
       01  WB-F-Lang                   PIC S9(7)  COMP  VALUE 100.
       01  WB-Fehler.
           05  FILLER                  PIC X(07)  VALUE 'DB2RES:'.
           05  WB-HPROGRAMM-ID         PIC X(08)  VALUE  SPACE.
           05  FILLER                  PIC X(01)  VALUE ':'.
           05  WB-UPROGRAMM-ID         PIC X(08)  VALUE  SPACE.
           05  FILLER                  PIC X(01)  VALUE ':'.
           05  WB-FEHLER-NR            PIC +Z99.
           05  FILLER                  PIC X(01)  VALUE ':'.
           05  WB-FEHLER-TEXT          PIC X(70)  VALUE SPACE.
      *-------------------------------------------------------------
      *--EIN-/AUSGABEPARAMETER FUER SCHNITTSTELLENMODULE
      *--SM1, SM2, SM7, SM10
      *-------------------------------------------------------------
       01  PS-HPROGRAMM-ID             PIC X(08).
       01  PR-SQL-CODE                 PIC S9(9) COMP.
       01  PR-RUECK-CODE               PIC 99.
       01  PR-FEHLER-TEXT              PIC X(070).
      *-------------------------------------------------------------
      *--EIN-/AUSGABEPARAMETER FUER SCHNITTSTELLENMODULE
      *--SM2, SM7
      *-------------------------------------------------------------
       01  P-HIST-TABELLE.
           05  P-HISTORIEN-TAB OCCURS 50.
               10  P-TAB-VONDAT        PIC 9(6)    COMP-3.
               10  P-TAB-BISDAT        PIC 9(6)    COMP-3.
               10  P-TAB-DIR           PIC 999     COMP-3.
               10  P-TAB-STUFE         PIC 999     COMP-3.
               10  P-TAB-STATUS        PIC X.
               10  P-TAB-BMANR         PIC 9(7)    COMP-3.
               10  P-TAB-AUSPROV       PIC 999V9   COMP-3.
               10  P-TAB-EPROVKZ       PIC X.
               10  P-TAB-VARPROV       PIC 9(5)V9  COMP-3.
       01  P-ANZ-TAB                   PIC 9(2).
CCC009*    EJECT
