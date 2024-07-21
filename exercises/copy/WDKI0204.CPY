      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0204.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0204.CPv  $
      *
      *   Rev 2.3   May 06 2003 14:11:44   p13sp16
      *Header Korrektur
      *
      *   REV 2.2   FEB 17 2002 14:17:40   T13AE38
      *AKI00093
      *
      *   REV 2.1   APR 16 1999 10:07:08   I13PC19
      *
      *
      *   REV 2.0   APR 15 1999 13:32:24   P13SP99
      *NEUERSTELLUNG
      *
      *---------------------------------------------------------------*
        05 EA-KUNDENBESTELLUNG-GESAMT.
         10 EA-NI-KUNDENBESTELLUNG-TAB.
           20 EA-NI-KUNDENBESTELLUNG-ELE PIC X
                  OCCURS 17 TIMES.
         10 EA-NI-KUNDENBESTELLUNG
                 REDEFINES EA-NI-KUNDENBESTELLUNG-TAB.
           20 EA-NI-JAHR                     PIC X.
           20 EA-NI-HEFTNUMMER               PIC X.
           20 EA-NI-BESTELLERID              PIC X.
           20 EA-NI-BESTELLERROLLE           PIC X.
           20 EA-NI-KUNDENID                 PIC X.
           20 EA-NI-KUNDENNUMMER             PIC X.
           20 EA-NI-NACHNAME                 PIC X.
           20 EA-NI-ANREDEID                 PIC X.
           20 EA-NI-PLZ                      PIC X.
           20 EA-NI-VORNAME                  PIC X.
           20 EA-NI-STRASSE                  PIC X.
           20 EA-NI-FEHLERSTATUS             PIC X.
           20 EA-NI-ORT                      PIC X.
           20 EA-NI-ANZAHL                   PIC X.
           20 EA-NI-TITELID                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-ADRESSELAND              PIC X.
         10 EA-DCL-KUNDENBESTELLUNG.
           20 EA-JAHR                        PIC -----9    .
           20 EA-HEFTNUMMER                  PIC -----9    .
           20 EA-BESTELLERID                 PIC X(13).
           20 EA-BESTELLERROLLE              PIC X(01).
           20 EA-KUNDENID                    PIC X(13).
           20 EA-KUNDENNUMMER                PIC --------9 .
           20 EA-NACHNAME                    PIC X(30).
           20 EA-ANREDEID                    PIC -----9    .
           20 EA-PLZ                         PIC X(5).
           20 EA-VORNAME                     PIC X(15).
           20 EA-STRASSE                     PIC X(45).
           20 EA-FEHLERSTATUS                PIC -----9    .
           20 EA-ORT                         PIC X(30).
           20 EA-ANZAHL                      PIC -----9    .
           20 EA-TITELID                     PIC -----9    .
           20 EA-LETZTEAENDERUNG             PIC X(26).
           20 EA-ADRESSELAND                 PIC X(03).
      ******************************************************************
