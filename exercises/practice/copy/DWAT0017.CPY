      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:31:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 01470002
      *    COPY-MEMBER: DWAT0017      TITEL VERSORGEN - MENUEDIALOG   * 01480002
      *    --------------------------------------------------------   * 01490002
      *    VERSION   001                                              * 01500002
      *    --------------------------------------------------------   * 01510002
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 01520002
      *    DATUM:    31.03.1993                                       * 01530002
      *    STAND:    06.04.1993                                       * 01540002
      ***************************************************************** 01550002
      *                                                                 01560002
      *    IM MENUEDIALOG WERDEN DIE 2 TITELFELDER IM MASKENKOPF WIE    01570002
      *    FOLGT VERSORGT:                                              01580002
      *    - TITEL 1: KONSTANT                                          01590002
      *    - TITEL 2: ABHÄNGIG VON CARR-DIALOG-AKT                      01600002
      *               DER TITEL 2 IST ZUSAMMEN MIT DEM DIALOG-AKT       01610002
      *               IN EINER TABELLE ABGELEGT                         01620002
      *    (ZUGEHÖRIGES COPY-ELEMENT DER PROCEDURE DIV.: DPAT0017)      01630002
      *                                                                 01640002
      * --------------------------------------------------------------  01650002
      *   TITEL-1-ZEILE FUER MASKEN-KOPF                                01660002
      *   !!! MUSS-FELDER !!!                                           01670002
      *                        1         2  M      3         4          01680002
      *               123456789012345678901234567890123456789012345     01690002
       01  K-DIALOG-TITEL1          PIC  X(045) VALUE                   01700002
                     '                     MENÜ                    '.   01710002
                                                                        01720002
      *    VARIABLEN FÜR ZENTRIEREN TITEL-2                             01730002
       01  Z-DWAT0017-F1.                                               01740002
           05  FILLER                    PIC  X(044).                   01750002
           05  Z-DWAT0017-F1-45          PIC  X(001).                   01760002
       01  Z-DWAT0017-F2.                                               01770002
           05  FILLER                    PIC  X(001).                   01780002
           05  Z-DWAT0017-F2-2B45        PIC  X(044).                   01790002
       01  Z-DWAT0017-ANZ-SP             PIC S9(004) COMP.              01800002
       01  Z-DWAT0017-ANZ-SP-H           PIC S9(004) COMP.              01810002
      *!                                                                01820002
      *!*   TITEL-2-ZEILEN FUER MASKEN-KOPF                             01830002
      *!*   (ABHAENGIG VOM AUFRUF-OP)                                   01840002
      *! 01  T-TITEL2-TAB.                                              01850002
      *!*        * INHALT JEWEILS: DIALOG-AKT, ZUGEH. TITEL-2           01860002
      *!*        *                 (ZA04 FUER TEST-ZWECKE)              01870002
      *!     05  FILLER.                                                01880002
      *!         10  FILLER                PIC  X(004)  VALUE 'ZA04'.   01890002
      *!         10  FILLER                PIC  X(034)                  01900002
      *!*                                   1      MM 2         3       01910002
      *!*                          1234567890123456789012345678901234   01920002
      *!                    VALUE '         TEST-MENÜ-TITEL-2        '. 01930002
      *!     05  FILLER.                                                01940002
      *!         10  FILLER                PIC  X(004)  VALUE 'SF10'.   01950002
      *!         10  FILLER                PIC  X(034)                  01960002
      *!                    VALUE '         SONDER-FUNKTIONEN        '. 01970002
      *!     05  FILLER.                                                01980002
      *!         10  FILLER                PIC  X(004)  VALUE 'VB10'.   01990002
      *!         10  FILLER                PIC  X(034)                  02000002
      *!                    VALUE '     VERMÖGENSBERATER ERFASSEN    '. 02010002
      *!     05  FILLER.                                                02020002
      *!         10  FILLER                PIC  X(004)  VALUE 'VB20'.   02030002
      *!         10  FILLER                PIC  X(034)                  02040002
      *!                    VALUE '      VERMÖGENSBERATER ÄNDERN     '. 02050002
      *!                                                                02060002
      *! 01  T-TITEL2-TAB-RED REDEFINES T-TITEL2-TAB.                   02070002
      *!     05  T-T2-ZEILE OCCURS  4                                   02080002
      *!                    INDEXED BY I-TITEL2.                        02090002
      *!         10  T-TITEL2-DIALOG       PIC  X(004).                 02100002
      *!         10  T-TITEL2-TEXT         PIC  X(034).                 02110002
      *                                                                 02130002
      ******* ENDE COPY-MEMBER DWAT0017 (TITEL - MENUEDIALOG) ********* 02140002
CCC009*    EJECT                                                        02150002
