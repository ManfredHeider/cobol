      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0501.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0501.CPv  $
      *
      *   Rev 2.7   Feb 18 2015 16:56:12   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.6   Jun 11 2003 13:13:34   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BANKECKARTE.
           10 BECVERTRAGSID                 PIC  X(0013).
           10 BECKONTOUNR                   PIC S9(4) COMP.
           10 BECLFDNR                      PIC S9(4) COMP.
           10 LETZTEAENDERUNG               PIC  X(026).
           10 BECKARTENART                  PIC  X(0003).
           10 NIL-BECKARTENART              PIC  X(01).
           10 BECANZKARTEN                  PIC S9(4) COMP.
           10 NIL-BECANZKARTEN              PIC  X(01).
           10 BECKARTENNR.
             49 BECKARTENNR-LEN             PIC S9(4) COMP.
             49 BECKARTENNR-TEXT            PIC X(16).
           10 NIL-BECKARTENNR               PIC  X(01).

