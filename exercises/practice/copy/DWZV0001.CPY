      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWZV0001.CPY  $
      *    $Revision:   1.0  $
      *
      *    $Log:   L:/MF/CPY/DWZV0001.CPv  $
      *
      *   Rev 1.0   Nov 04 1995 09:34:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DWZV0001       SCHREIBMODULE                  *
      *                                  *--  STEUERUNG               *
      *                                  *--  PARAMETERPRÜFUNG        *
      *                                  *--  VERARBEITUNG            *
      *                                       (Ohne SQL-Verarbeitung) *
      *    --------------------------------------------------------   *
      *    VERSION   000                                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN          (DVAG)                          *
      *    DATUM:    21.01.1993                                       *
      *    STAND:    17.03.1993                                       *
      *****************************************************************
      *
      * -->              K O N S T A N T E                   <---
      *
      *      Anzahl Attribute LOG-Table
         05  K-Attrib-Anzahl      PIC S9(006)
                VALUE            +12.
      *      Max. Wiederholung bei Resource nicht verfügbar/Deadlock
         05  K-Schreib-Max        PIC S9(006)
                VALUE             +5.
         05  K-Nein               PIC  X(001)
                VALUE             'N'.
      *      Konstante für Verarbeitungstyp
         05  K-Insert             PIC  X(001)
                VALUE             'I'.
         05  K-Update             PIC  X(001)
                VALUE             'U'.
         05  K-Delete             PIC  X(001)
                VALUE             'D'.

      *                                                                 00100000
      *
      * -->              V A R I A B L E                     <---
      *

      *      Steuerungsschalter zur LOG-Tableverarbeitung
         05  WB-LOG-Table-Fuell   PIC  X(001)            VALUE SPACE.
             88  WB-LOG-Table-VA                         VALUE 'J'.
             88  WB-LOG-Table-NVA                        VALUE 'N'.

      *      Zwischenbereiche zur Prüfung Date-Ab/Date-Aend
         05  WB-Date-Display-Curr.
           10 WB-Date-TT-C        PIC  X(002).
           10 FILLER              PIC  X(001).
           10 WB-Date-MM-C        PIC  X(002).
           10 FILLER              PIC  X(001).
           10 WB-Date-JJJJ-C      PIC  X(004).

         05  WB-Date-Display.
           10 WB-Date-JJJJ-D      PIC  X(004).
           10 FILLER              PIC  X(001).
           10 WB-Date-MM-D        PIC  X(002).
           10 FILLER              PIC  X(001).
           10 WB-Date-TT-D        PIC  X(002).

         05  WB-Date-Von.
           10 WB-Date-JJJJ-V      PIC  X(004)            VALUE SPACE.
           10 WB-Date-MM-V        PIC  X(002)            VALUE SPACE.
           10 WB-Date-TT-V        PIC  X(002)            VALUE SPACE.

         05  WB-Date-Aend.
           10 WB-Date-JJJJ-A      PIC  X(004)            VALUE SPACE.
           10 WB-Date-MM-A        PIC  X(002)            VALUE SPACE.
           10 WB-Date-TT-A        PIC  X(002)            VALUE SPACE.

CCC009*    EJECT
       01   Workbereich-Link-Steuer.
         05  WLS-Programm-ID      PIC  X(008)            VALUE SPACE.
         05  WLS-Programm-KZ      PIC  X(001)            VALUE SPACE.
             88  WLS-Batch                               VALUE 'B'.
             88  WLS-Dialog                              VALUE 'D'.
         05  WLS-User-ID          PIC  X(008)            VALUE SPACE.
         05  WLS-Terminal-ID      PIC  X(008)            VALUE SPACE.
         05  WLS-Date-Aend        PIC  X(010)            VALUE SPACE.
         05  WLS-Date-Von         PIC  X(010)            VALUE SPACE.
         05  WLS-Aend-KZ          PIC  X(001)            VALUE SPACE.
             88  WLS-Insert                              VALUE 'I'.
             88  WLS-Update                              VALUE 'U'.
             88  WLS-Delete                              VALUE 'D'.

       01   Workbereich-Link-Return.
         05  WLR-Return-Code      PIC  X(001)            VALUE SPACE.
             88  Verarb-OK                               VALUE '0'.
             88  Verarb-Fehler                           VALUE '4'.
         05  WLR-Fehler-Typ       PIC  X(001)            VALUE SPACE.
             88  Fehler-Typ-aus                          VALUE ' '.
             88  Logik-Fehler                            VALUE 'L'.
             88  Rlback                                  VALUE 'R'.
             88  SQL-Error                               VALUE 'D'.
         05  WLR-Programm-ID      PIC  X(008)            VALUE SPACE.
         05  WLR-Fehler-Nr        PIC S9(004)    COMP    VALUE ZERO.
         05  WLR-Fehler-Text      PIC  X(070)            VALUE SPACE.


       01   Z-Table-Bereich.
         05  Z-Last-Update        PIC  X(026).
         05  Z-Last-Update-nl     PIC  X(026).
         05  Z-LOG-MA-Nr          PIC S9(009)   COMP.

