      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0506.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0506.CPv  $
      *
      *   Rev 2.3   Jun 03 2002 09:32:38   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.2   Dec 16 1999 16:05:32   I13PC19
      * 
      *
      *   Rev 2.1   Apr 16 1999 10:13:04   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
 
       05 EA-KISTATISTIK-GESAMT.
         10 EA-NI-KISTATISTIK-TAB.
           20 EA-NI-KISTATISTIK-ELE PIC X
210502*           OCCURS 26 TIMES.
210502            OCCURS 27 TIMES.
         10 EA-NI-KISTATISTIK
                 REDEFINES EA-NI-KISTATISTIK-TAB.
           20 EA-NI-STATISTIKID              PIC X.
           20 EA-NI-PCBESITZERID             PIC X.
           20 EA-NI-PCBESITZERROLLE          PIC X.
           20 EA-NI-ERSTELLUHRZEIT           PIC X.
           20 EA-NI-ERSTELLTDATUM            PIC X.
           20 EA-NI-BERECHNETDATUM           PIC X.
           20 EA-NI-RECHNUNGSDATUM           PIC X.
           20 EA-NI-KUNDENID                 PIC X.
           20 EA-NI-KUNDROLLE                PIC X.
           20 EA-NI-KUNDENNUMMER             PIC X.
           20 EA-NI-KUNDENNAME               PIC X.
           20 EA-NI-KUNDENVORNAME            PIC X.
           20 EA-NI-VBID                     PIC X.
           20 EA-NI-VBROLLE                  PIC X.
           20 EA-NI-VBNUMMER                 PIC X.
           20 EA-NI-VBNAME                   PIC X.
           20 EA-NI-VBVORNAME                PIC X.
           20 EA-NI-VBSTRASSEHAUSNR          PIC X.
           20 EA-NI-VBPLZ                    PIC X.
           20 EA-NI-VBORT                    PIC X.
           20 EA-NI-VBBDSLAND                PIC X.
           20 EA-NI-VBLAND                   PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-KSSTATISTIKDATUM         PIC X.
           20 EA-NI-WORKSTATIONID            PIC X.
           20 EA-NI-VSASTATISTIKDATUM        PIC X.
210502     20 EA-NI-KISKIKUNDENAKTEVER       PIC X.
         10 EA-DCL-KISTATISTIK.
           20 EA-STATISTIKID                 PIC X(13).
           20 EA-PCBESITZERID                PIC X(13).
           20 EA-PCBESITZERROLLE             PIC X(1).
           20 EA-ERSTELLUHRZEIT              PIC X(08).
           20 EA-ERSTELLTDATUM               PIC X(10).
           20 EA-BERECHNETDATUM              PIC X(10).
           20 EA-RECHNUNGSDATUM              PIC X(10).
           20 EA-KUNDENID                    PIC X(13).
           20 EA-KUNDROLLE                   PIC X(1).
           20 EA-KUNDENNUMMER                PIC --------9 .
           20 EA-KUNDENNAME                  PIC X(30).
           20 EA-KUNDENVORNAME               PIC X(15).
           20 EA-VBID                        PIC X(13).
           20 EA-VBROLLE                     PIC X(1).
           20 EA-VBNUMMER                    PIC --------9 .
           20 EA-VBNAME                      PIC X(30).
           20 EA-VBVORNAME                   PIC X(15).
           20 EA-VBSTRASSEHAUSNR             PIC X(45).
           20 EA-VBPLZ                       PIC X(15).
           20 EA-VBORT                       PIC X(30).
           20 EA-VBBDSLAND                   PIC X(3).
           20 EA-VBLAND                      PIC X(3).
           20 EA-LETZTEAENDERUNG             PIC X(26).
           20 EA-KSSTATISTIKDATUM            PIC X(10).
           20 EA-WORKSTATIONID               PIC --------9 .
           20 EA-VSASTATISTIKDATUM           PIC X(10).
210502     20 EA-KISKIKUNDENAKTEVER          PIC X(1).
      ******************************************************************
