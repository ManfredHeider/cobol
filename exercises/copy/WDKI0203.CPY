      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0203.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WDKI0203.CPv  $
      *
      *   Rev 2.7   May 24 2004 10:52:50   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.6   Feb 16 2004 09:49:44   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.5   May 06 2003 14:08:18   p13sp16
      *Header-Korrektur
      *
      *   REV 2.4   DEC 16 2002 12:03:30   T13EX27
      *AKI00321 DB-Änderung für Fixpack 31
      *
      *   REV 2.3   OCT 15 2002 08:15:46   T13EX27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   REV 2.2   FEB 17 2000 14:17:34   T13AE39
      *AKI00093
      *
      *   REV 2.1   APR 16 1999 10:07:04   I13PC19
      *
      *   REV 2.0   APR 15 1999 13:32:24   P13SP99
      *NEUERSTELLUNG
      *
      *---------------------------------------------------------------*
        05 EA-BESTELLUNG-GESAMT.
         10 EA-NI-BESTELLUNG-TAB.
           20 EA-NI-BESTELLUNG-ELE PIC X OCCURS 28 TIMES.
         10 EA-NI-BESTELLUNG
                 REDEFINES EA-NI-BESTELLUNG-TAB.
           20 EA-NI-JAHR                     PIC X.
           20 EA-NI-HEFTNUMMER               PIC X.
           20 EA-NI-BESTELLERID              PIC X.
           20 EA-NI-BESTELLERROLLE           PIC X.
           20 EA-NI-ANREDEID                 PIC X.
           20 EA-NI-TITELID                  PIC X.
           20 EA-NI-NACHNAME                 PIC X.
           20 EA-NI-VORNAME                  PIC X.
           20 EA-NI-STRASSE                  PIC X.
           20 EA-NI-PLZ                      PIC X.
           20 EA-NI-ORT                      PIC X.
           20 EA-NI-TELEFON                  PIC X.
           20 EA-NI-FAX                      PIC X.
           20 EA-NI-KONTOINHABER             PIC X.
           20 EA-NI-BANKNAME                 PIC X.
           20 EA-NI-BANKLEITZAHL             PIC X.
           20 EA-NI-KONTONUMMER              PIC X.
           20 EA-NI-ANZAHLEIGEN              PIC X.
           20 EA-NI-AUFGABETIMESTAMP         PIC X.
           20 EA-NI-STATUSID                 PIC X.
           20 EA-NI-REPLIKATIMESTAMP         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-ADRESSELAND              PIC X.
           20 EA-NI-BANKLAND                 PIC X.
           20 EA-NI-EMAIL                    PIC X.
           20 EA-NI-URL                      PIC X.
           20 EA-NI-ARCHIVIERT               PIC X.
           20 EA-NI-BESTELLABOKZ             PIC X.
         10 EA-DCL-BESTELLUNG.
           20 EA-JAHR                        PIC -----9.
           20 EA-HEFTNUMMER                  PIC -----9.
           20 EA-BESTELLERID                 PIC X(13).
           20 EA-BESTELLERROLLE              PIC X(1).
           20 EA-ANREDEID                    PIC -----9.
           20 EA-TITELID                     PIC -----9.
           20 EA-NACHNAME                    PIC X(30).
           20 EA-VORNAME                     PIC X(15).
           20 EA-STRASSE                     PIC X(45).
           20 EA-PLZ                         PIC X(5).
           20 EA-ORT                         PIC X(30).
           20 EA-TELEFON                     PIC X(25).
           20 EA-FAX                         PIC X(25).
           20 EA-KONTOINHABER                PIC X(50).
           20 EA-BANKNAME                    PIC X(30).
           20 EA-BANKLEITZAHL                PIC ----------9.
           20 EA-KONTONUMMER                 PIC X(20).
           20 EA-ANZAHLEIGEN                 PIC ----------9.
           20 EA-AUFGABETIMESTAMP            PIC X(026).
           20 EA-STATUSID                    PIC -----9.
           20 EA-REPLIKATIMESTAMP            PIC X(026).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-ADRESSELAND                 PIC X(3).
           20 EA-BANKLAND                    PIC X(3).
           20 EA-EMAIL.
              49 EA-EMAIL-LEN                PIC -----9.
              49 EA-EMAIL-TEXT               PIC X(128).
           20 EA-URL.
              49 EA-URL-LEN                  PIC -----9.
              49 EA-URL-TEXT                 PIC X(128).
           20 EA-ARCHIVIERT                  PIC X(1).
           20 EA-BESTELLABOKZ                PIC X(1).
      *****************************************************************
