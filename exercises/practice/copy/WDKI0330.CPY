      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0330.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WDKI0330.CPv  $
      *
      *   Rev 2.2   Oct 17 2005 08:57:32   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.1   Apr 16 1999 10:07:50   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:34   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-LEBENTEILSCHICHT-GESAMT.
         10 EA-NI-LEBENTEILSCHICHT-TAB.
           20 EA-NI-LEBENTEILSCHICHT-ELE PIC X OCCURS 15 TIMES.
         10 EA-NI-LEBENTEILSCHICHT
                 REDEFINES EA-NI-LEBENTEILSCHICHT-TAB.
           20 EA-NI-LTSVERTRAGSID            PIC X.
           20 EA-NI-LTSVERSTARLFDNR          PIC X.
           20 EA-NI-LTSZAHLDYNERHOEH         PIC X.
           20 EA-NI-LTSTTNR                  PIC X.
           20 EA-NI-LTSDATDYNERHOEH          PIC X.
           20 EA-NI-LTSRATENBEITRAG          PIC X.
           20 EA-NI-LTSRATENBEITRWKZ         PIC X.
           20 EA-NI-LTSVERSSUMME             PIC X.
           20 EA-NI-LTSVERSSUMMEWKZ          PIC X.
           20 EA-NI-LTSTARBEZEICHNUNG        PIC X.
           20 EA-NI-LTSBEITRZUSTID           PIC X.
           20 EA-NI-LTSVERTRZUSTANDID        PIC X.
           20 EA-NI-LTSVERTRZUSTANDDAT       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-LTSSCHICHTZUGARTID       PIC X.
         10 EA-DCL-LEBENTEILSCHICHT.
           20 EA-LTSVERTRAGSID               PIC X(13).
           20 EA-LTSVERSTARLFDNR             PIC -----9.
           20 EA-LTSZAHLDYNERHOEH            PIC -----9.
           20 EA-LTSTTNR                     PIC -----9.
           20 EA-LTSDATDYNERHOEH             PIC X(010).
           20 EA-LTSRATENBEITRAG             PIC -----------9.99.
           20 EA-LTSRATENBEITRWKZ            PIC X(1).
           20 EA-LTSVERSSUMME                PIC -----------9.99.
           20 EA-LTSVERSSUMMEWKZ             PIC X(1).
           20 EA-LTSTARBEZEICHNUNG           PIC X(15).
           20 EA-LTSBEITRZUSTID              PIC X(4).
           20 EA-LTSVERTRZUSTANDID           PIC -----9.
           20 EA-LTSVERTRZUSTANDDAT          PIC X(010).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-LTSSCHICHTZUGARTID          PIC X(1).
      *****************************************************************
