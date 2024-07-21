      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0045.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0045.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:07:18   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-PERSPERSBEZIEHUNG.
           10 PPBEZPERSONENID1            PIC X(13).
           10 PPBEZPERSONENID2            PIC X(13).
           10 PPBEZBEZART                 PIC S9(4) COMP.
           10 PPBEZINVBEZART              PIC S9(4) COMP.
           10 PPBEZGUELTIGVON             PIC X(010).
           10 NIL-PPBEZGUELTIGVON         PIC X(01).
           10 PPBEZGUELTIGBIS             PIC X(010).
           10 NIL-PPBEZGUELTIGBIS         PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 PPBEZWERT                   PIC S9(6)V9(2) COMP-3.
           10 NIL-PPBEZWERT               PIC X(01).
      *****************************************************************
