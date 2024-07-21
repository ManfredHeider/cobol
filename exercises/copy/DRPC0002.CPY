      * T13AE19   Stand: 28/11/96    13:31:34
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPC0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPC0002.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 18:44:08   T13EX48
      *AKI00028
      *Datei-Struktur neue RLK-Datei
      *
      *   Rev 2.0   Feb 27 1999 14:47:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Jan 20 1997 11:22:34   T13AE19
      *Neues Feld BA-URSPRUNG
      *
      *   Rev 1.0   Nov 04 1995 09:01:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    20.01.97 17:15:55
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER RK-DATEI       DRPC0002                    *
      *    -------------------------                                   *
      *                                                                *
      *    LAENGE = 100 BYTES        ERSTELLT AM 25.05.87 / BIEHL      *
      *                             UMBENANNT AM 07.07.93 / BORK       *
      *                                                                *
      *    INDEX-DATEI MIT INDEX = RK-SCHL                             *
      *    DATEINAME  = U.UU0226                                       *
      *                                                                *
      *    05.02.1993  BORK                                            *
      *    DATENFELDER FÜR AM-VERS IN DEN SATZ MIT AUFGENOMMEN         *
      *    UND SATZ VON 80 AUF 100 BYTE ERWEITERT                      *
      *
      *    5. 12. 1997 11:22:34   T13EX31
      *        RK-ART           PIC 9  auf   RK-ART  PIC X geändert.
      *        RK-VWNR          PIC X(7)     neu angelegt.
      *        RK-DB2-AUSLESEKZ PIC X        neu angelegt.
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  DRPC0002.
           05  RK-SCHL.
               10  RK-PCNR                 PIC 9(005).
               10  RK-GES                  PIC 9(002).
               10  RK-VSNR                 PIC 9(015).
               10  RK-ABSDAT               PIC 9(006).
               10  RK-GES-KZ               PIC 9(002).
           05  RK-SATZ.
               10  RK-STATKZ               PIC 9(002).
               10  RK-STATDAT              PIC 9(006).
               10  RK-ART                  PIC X(001).
               10  RK-VBNR                 PIC 9(007).
               10  RK-VNNAM                PIC X(039).
               10  RK-PCSEL                PIC 9(002).
               10  RK-BESCHREIBUNG         PIC X(001).
               10  RK-VWPCNR-7.
                   15  RK-VWPCNR.
                       20  RK-VWPCNR-DIR   PIC 9(003).
                       20  RK-VWPCNR-PC    PIC 9(002).
                   15  RK-VWPCNR6-7        PIC 9(002).
               10  RK-VWPCNR-NUM    REDEFINES   RK-VWPCNR-7  PIC 9(007).
               10  RK-DB2-AUSLESEKZ        PIC X(001).
               10  FILLER                  PIC X(004).
           05  RK-DUMMYSATZ   REDEFINES   RK-SATZ.
               10  RK-BEREINDAT            PIC 9(007)   COMP-3.
               10  FILLER                  PIC X(066).

