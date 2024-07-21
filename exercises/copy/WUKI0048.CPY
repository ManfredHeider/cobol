      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0048.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0048.CPv  $
      *
      *   Rev 2.7   Apr 04 2005 08:39:48   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.6   Jun 11 2003 13:07:30   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-PERSREFERENZNUMMER.
           10 PREFNUMPERSONENID           PIC X(13).
           10 PREFNUMARTID                PIC S9(4) COMP.
           10 PREFNUMLFDNUMMER            PIC S9(9) COMP.
           10 PREFNUMMER                  PIC X(30).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 PREFAUSGESTELLTAM           PIC X(010).
           10 PREFAUSGESTELLTORT.
              49 PREFAUSGESTELLTORT-LEN        PIC S9(4) COMP.
              49 PREFAUSGESTELLTORT-TEXT       PIC X(30).
           10 PREFGUELTIGBIS              PIC X(010).
           10 NIL-PREFGUELTIGBIS          PIC X(01).
      *****************************************************************
