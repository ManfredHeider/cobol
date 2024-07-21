      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0506.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0506.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:13:46   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-KISTATISTIK.
           10 STATISTIKID                 PIC X(13).
           10 PCBESITZERID                PIC X(13).
           10 PCBESITZERROLLE             PIC X(1).
           10 ERSTELLUHRZEIT              PIC X(008).
           10 NIL-ERSTELLUHRZEIT          PIC X(01).
           10 ERSTELLTDATUM               PIC X(010).
           10 NIL-ERSTELLTDATUM           PIC X(01).
           10 BERECHNETDATUM              PIC X(010).
           10 NIL-BERECHNETDATUM          PIC X(01).
           10 RECHNUNGSDATUM              PIC X(010).
           10 NIL-RECHNUNGSDATUM          PIC X(01).
           10 KUNDENID                    PIC X(13).
           10 NIL-KUNDENID                PIC X(01).
           10 KUNDROLLE                   PIC X(1).
           10 NIL-KUNDROLLE               PIC X(01).
           10 KUNDENNUMMER                PIC S9(9) COMP.
           10 NIL-KUNDENNUMMER            PIC X(01).
           10 KUNDENNAME                  PIC X(30).
           10 NIL-KUNDENNAME              PIC X(01).
           10 KUNDENVORNAME               PIC X(15).
           10 NIL-KUNDENVORNAME           PIC X(01).
           10 VBID                        PIC X(13).
           10 NIL-VBID                    PIC X(01).
           10 VBROLLE                     PIC X(1).
           10 NIL-VBROLLE                 PIC X(01).
           10 VBNUMMER                    PIC S9(9) COMP.
           10 NIL-VBNUMMER                PIC X(01).
           10 VBNAME                      PIC X(30).
           10 NIL-VBNAME                  PIC X(01).
           10 VBVORNAME                   PIC X(15).
           10 NIL-VBVORNAME               PIC X(01).
           10 VBSTRASSEHAUSNR             PIC X(45).
           10 NIL-VBSTRASSEHAUSNR         PIC X(01).
           10 VBPLZ                       PIC X(15).
           10 NIL-VBPLZ                   PIC X(01).
           10 VBORT                       PIC X(30).
           10 NIL-VBORT                   PIC X(01).
           10 VBBDSLAND                   PIC X(3).
           10 NIL-VBBDSLAND               PIC X(01).
           10 VBLAND                      PIC X(3).
           10 NIL-VBLAND                  PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 KSSTATISTIKDATUM            PIC X(010).
           10 WORKSTATIONID               PIC S9(9) COMP.
           10 NIL-WORKSTATIONID           PIC X(01).
           10 VSASTATISTIKDATUM           PIC X(010).
           10 KISKIKUNDENAKTEVER          PIC X(1).
      *****************************************************************
