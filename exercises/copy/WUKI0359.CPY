      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0359.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0359.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:12:26   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERTRAGROLLEBEZ.
           10 VRBVERTRAGSID               PIC X(13).
           10 VRBPERSONENID               PIC X(13).
           10 VRBPERSONENROLLEKZ          PIC X(1).
           10 VRBPABPERSONENID            PIC X(13).
           10 NIL-VRBPABPERSONENID        PIC X(01).
           10 VRBPABADRESSEID             PIC X(13).
           10 NIL-VRBPABADRESSEID         PIC X(01).
           10 VRBABLAUFDATUM              PIC X(010).
           10 NIL-VRBABLAUFDATUM          PIC X(01).
           10 VRBGUELTIGKEITSDAT          PIC X(010).
           10 NIL-VRBGUELTIGKEITSDAT      PIC X(01).
           10 VRBAVKENNZEICHEN            PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 VRBKOMMBETRKZ               PIC X(1).
      *****************************************************************
