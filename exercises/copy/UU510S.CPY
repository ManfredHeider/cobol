      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU510S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/UU510S.CPv  $
      *
      *   Rev 2.2   Jun 01 2006 13:43:02   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.1   Jun 01 2006 13:27:18   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:00:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    05.03.92 10:58:11
      ****************************************************************
      *                                                              *
      *          BEWEGUNGS-DATEI ROOT-SEGMENT                        *
      *          VARIABELE LAENGE, MINIMAL 104, MAXIMAL 236 BYTES    *
      *          SCHLUESSEL: P1GES, P1VNR                            *
      *          ALT.-IND.:  P1INTVR (OPAL1)                         *
      *                                                              *
      *          AUFTRAG UU89030 VB-WETTBEWERB 1990                  *
      *          EINFUEHRUNG P1VSBEGINN UND ZAHLWEISE                *
      *          28.11.89  GEIGER                                    *
      *                                                              *
      *   AENDERUNGEN                         NAME   TT.MM.JJ  VERS. *
      *                                                              *
      *   DDR-KENNZEICHEN EINGEFUEGT         GARRE   20.07.90   01   *
      *  (TEXT NACHGETRAGEN AM 05.03.92 VERSION DURCH                *
      *   NEUES PRODUKTIONSUEBERNAHMEVERF.   VIETEN)                 *
      *   ==> KEINE INHALTLICHE AENDERUNG DES COPYMEMBERS AM 05.03.92*
      ****************************************************************
       01  P1ROOTSEG.
           03 P1SA                      PIC XX.
           03 P1KEY.
              05  P1GES                 PIC XX.
              05  P1VSNR                PIC 9(15).
           03 P1INTVSNR                 PIC 9(15).
           03 P1SATZ.
              05  P1EKRIT.
                  10  P1EPGM            PIC X(8).
                  10  P1EDATUM          PIC 9(7)        COMP-3.
                  10  P1EZEIT           PIC 9(7)        COMP-3.
                  10  P1PSNR            PIC 9(7)        COMP-3.
              05  P1AVERM               PIC 9(7)        COMP-3.
              05  P1AVERMPRZ            PIC X.
              05  P1VVERM               PIC 9(7)        COMP-3.
              05  P1VVERMPRZ            PIC X.
              05  P1KDBETRGV            PIC 9(7)        COMP-3.
              05  P1NAME                PIC X(14).
              05  P1VSBEGINN            PIC 9(7)        COMP-3.
              05  P1ZAHLWEISE           PIC 9.
              05  P1DDRKZ               PIC X(1).
              05  P1FILLER              PIC X(2).
              05  P1KDBETRANZ           PIC 99.
              05  P1TABBETR    OCCURS 1 TO 12 DEPENDING ON P1KDBETRANZ.
                  10  P1KDBETR          PIC 9(7)        COMP-3.
                  10  P1GUELTAB         PIC 9(7)        COMP-3.
                  10  P1AENDDAT         PIC 9(7)        COMP-3.

