      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0025.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0025.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:06:22   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:32:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HYPOTHEK.
           10 HYPPERSONENID               PIC X(13).
           10 HYPPERSONENROLLE            PIC X(1).
           10 HYPHWELFDNUMMER             PIC S9(9) COMP.
           10 HYPLAUFENDENUMMER           PIC S9(9) COMP.
           10 HYPBANKBLZ                  PIC S9(9) COMP.
           10 NIL-HYPBANKBLZ              PIC X(01).
           10 HYPBANKBLZTEXT              PIC X(30).
           10 NIL-HYPBANKBLZTEXT          PIC X(01).
           10 HYPDARLEHENSSUMME           PIC S9(9)V9(2) COMP-3.
           10 NIL-HYPDARLEHENSSUMME       PIC X(01).
           10 HYPDARLSUMMEWKZ             PIC X(1).
           10 HYPFESTGELEGTBIS            PIC X(010).
           10 NIL-HYPFESTGELEGTBIS        PIC X(01).
           10 HYPMONBELASTUNG             PIC S9(9)V9(2) COMP-3.
           10 NIL-HYPMONBELASTUNG         PIC X(01).
           10 HYPMONBELASTWKZ             PIC X(1).
           10 HYPRESTSCHULD               PIC S9(9)V9(2) COMP-3.
           10 NIL-HYPRESTSCHULD           PIC X(01).
           10 HYPRESTSCHULDWKZ            PIC X(1).
           10 HYPTILGUNG                  PIC S9(3)V9(2) COMP-3.
           10 NIL-HYPTILGUNG              PIC X(01).
           10 HYPZINSEN                   PIC S9(3)V9(2) COMP-3.
           10 NIL-HYPZINSEN               PIC X(01).
           10 HYPANLAGEZIEL               PIC X(50).
           10 NIL-HYPANLAGEZIEL           PIC X(01).
           10 HYPNOTIZID                  PIC X(13).
           10 NIL-HYPNOTIZID              PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 HYPDARLEHENSART             PIC S9(4) COMP.
           10 NIL-HYPDARLEHENSART         PIC X(01).
           10 HYPTILGUNGSBEITRAG          PIC S9(9)V9(2) COMP-3.
           10 NIL-HYPTILGUNGSBEITRAG      PIC X(01).
           10 HYPTILGUNGSBEITWKZ          PIC X(1).
      *****************************************************************
