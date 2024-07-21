      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0356.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/WUKI0356.CPv  $
      *
      *   Rev 2.7   Oct 17 2005 10:08:12   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jul 03 2003 09:35:14   t13ex27
      *FKI00748 - DB-Änderung aus AKI00106
      *
      *   Rev 2.5   25 Jul 2001 10:33:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERSUMFANGBEZ.
           10 VUMVERTRAGSID               PIC X(13).
           10 VUMVERSTARLFDNR             PIC S9(4) COMP.
           10 VUMUMFANGID                 PIC S9(9) COMP.
           10 VUMUMFANGLFDNR              PIC S9(4) COMP.
           10 VUMUMFANGTEXT.
              49 VUMUMFANGTEXT-LEN             PIC S9(4) COMP.
              49 VUMUMFANGTEXT-TEXT            PIC X(20).
           10 VUMUMFANGPROZ               PIC S9(3)V9(2) COMP-3.
           10 NIL-VUMUMFANGPROZ           PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 VUMUMFEGR                   PIC S9(9) COMP.
           10 NIL-VUMUMFEGR               PIC X(01).
           10 VUMEGREH                    PIC X(1).
           10 NIL-VUMEGREH                PIC X(01).
      *****************************************************************
