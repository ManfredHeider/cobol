      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0365.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/Wuki0365.CPv  $
      *
      *   Rev 2.6   Jul 03 2003 09:35:16   t13ex27
      *FKI00748 - DB-Änderung aus AKI00106
      *
      *   Rev 2.5   25 Jul 2001 10:33:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WAGNISBEZ.
           10 WABVERTRAGSID               PIC X(13).
           10 WABVERSTARIFLFDNR           PIC S9(4) COMP.
           10 WABHPFLGRZUSATZKZ           PIC X(1).
           10 WABWAGNISID                 PIC S9(9) COMP.
           10 WABWAGNISLFDNR              PIC S9(4) COMP.
           10 WABWAGNISTEXT.
              49 WABWAGNISTEXT-LEN             PIC S9(4) COMP.
              49 WABWAGNISTEXT-TEXT            PIC X(25).
           10 WABWAGNISKZ                 PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
      *****************************************************************
