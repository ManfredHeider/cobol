      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0341.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0341.CPv  $
      *
      *   Rev 2.7   Feb 16 2004 09:52:04   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.6   Jun 11 2003 13:11:56   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERBVERTRAGSTEIL.
           10 VVTVERTRAGSID               PIC X(13).
           10 VVTGUTHABPRODLFDNR          PIC S9(4) COMP.
           10 VVTLAUFENDENUMMER           PIC S9(4) COMP.
           10 VVTDARLEHENSSUMME           PIC S9(9)V9(2) COMP-3.
           10 NIL-VVTDARLEHENSSUMME       PIC X(01).
           10 VVTDARLSUMWKZ               PIC X(1).
           10 VVTDARLEHENSZINSS           PIC S9(3)V9(2) COMP-3.
           10 NIL-VVTDARLEHENSZINSS       PIC X(01).
           10 VVTRATE                     PIC S9(9)V9(2) COMP-3.
           10 NIL-VVTRATE                 PIC X(01).
           10 VVTRATEWKZ                  PIC X(1).
           10 VVTRESTSCHULD               PIC S9(9)V9(2) COMP-3.
           10 NIL-VVTRESTSCHULD           PIC X(01).
           10 VVTRESTSCHULDWKZ            PIC X(1).
           10 VVTFESTGESCHBIS             PIC X(010).
           10 NIL-VVTFESTGESCHBIS         PIC X(01).
           10 VVTTILGUNGSWERT             PIC S9(9)V9(2) COMP-3.
           10 NIL-VVTTILGUNGSWERT         PIC X(01).
           10 VVTTILGWERTWKZ              PIC X(1).
           10 VVTTILGUNGSEINHEIT          PIC S9(4) COMP.
           10 NIL-VVTTILGUNGSEINHEIT      PIC X(01).
           10 VVTTILGUNGDYN               PIC S9(3)V9(2) COMP-3.
           10 NIL-VVTTILGUNGDYN           PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 VVTZWECK                    PIC X(40).
           10 NIL-VVTZWECK                PIC X(01).
           10 VVTARTID                    PIC S9(4) COMP.
      *****************************************************************
