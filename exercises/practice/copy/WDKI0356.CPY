      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0356.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0356.CPv  $
      *
      *   Rev 2.3   Oct 17 2005 08:58:08   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.2   10 May 2000 08:59:18   T13AE38
      *AKI00106
      *
      *   Rev 2.1   Apr 16 1999 10:10:56   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:46   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSUMFANGBEZ-GESAMT.
         10 EA-NI-VERSUMFANGBEZ-TAB.
           20 EA-NI-VERSUMFANGBEZ-ELE PIC X OCCURS 9 TIMES.
         10 EA-NI-VERSUMFANGBEZ
                 REDEFINES EA-NI-VERSUMFANGBEZ-TAB.
           20 EA-NI-VUMVERTRAGSID            PIC X.
           20 EA-NI-VUMVERSTARLFDNR          PIC X.
           20 EA-NI-VUMUMFANGID              PIC X.
           20 EA-NI-VUMUMFANGLFDNR           PIC X.
           20 EA-NI-VUMUMFANGTEXT            PIC X.
           20 EA-NI-VUMUMFANGPROZ            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-VUMUMFEGR                PIC X.
           20 EA-NI-VUMEGREH                 PIC X.
         10 EA-DCL-VERSUMFANGBEZ.
           20 EA-VUMVERTRAGSID               PIC X(13).
           20 EA-VUMVERSTARLFDNR             PIC -----9.
           20 EA-VUMUMFANGID                 PIC ----------9.
           20 EA-VUMUMFANGLFDNR              PIC -----9.
           20 EA-VUMUMFANGTEXT.
              49 EA-VUMUMFANGTEXT-LEN        PIC -----9.
              49 EA-VUMUMFANGTEXT-TEXT       PIC X(20).
           20 EA-VUMUMFANGPROZ               PIC ---9.99.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-VUMUMFEGR                   PIC ----------9.
           20 EA-VUMEGREH                    PIC X(1).
      *****************************************************************
