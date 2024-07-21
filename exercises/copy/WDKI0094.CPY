      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0094.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/WDKI0094.CPv  $
      *
      *   Rev 2.2   May 27 2003 11:39:22   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.1   Oct 15 2002 08:15:42   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.0   Sep 30 2002 12:41:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05 EA-KKVLISTE-GESAMT.
         10 EA-NI-KKVLISTE-TAB.
           20 EA-NI-KKVLISTE-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-KKVLISTE
                 REDEFINES EA-NI-KKVLISTE-TAB.
           20 EA-NI-KKVLID                   PIC X.
           20 EA-NI-KKVLERSTELLDAT           PIC X.
           20 EA-NI-KKVLABLAUFDAT            PIC X.
           20 EA-NI-KKVLEMPFVBNR             PIC X.
           20 EA-NI-KKVLFREIKZ               PIC X.
           20 EA-NI-KKVLFREIDAT              PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-KKVLAUFANFRAGEKZ         PIC X.
           20 EA-NI-KKVLORGABETREUER         PIC X.
           20 EA-NI-KKVLNAME                 PIC X.
         10 EA-DCL-KKVLISTE.
           20 EA-KKVLID                      PIC X(13).
           20 EA-KKVLERSTELLDAT              PIC X(010).
           20 EA-KKVLABLAUFDAT               PIC X(010).
           20 EA-KKVLEMPFVBNR                PIC ----------9.
           20 EA-KKVLFREIKZ                  PIC X(1).
           20 EA-KKVLFREIDAT                 PIC X(010).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-KKVLAUFANFRAGEKZ            PIC X(1).
           20 EA-KKVLORGABETREUER            PIC X(40).
           20 EA-KKVLNAME                    PIC X(60).
      *****************************************************************
