      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0341.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0341.CPv  $
      *
      *   Rev 2.3   Feb 16 2004 09:50:00   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.2   Jun 03 2002 09:32:34   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:10:28   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:38   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERBVERTRAGSTEIL-GESAMT.
         10 EA-NI-VERBVERTRAGSTEIL-TAB.
           20 EA-NI-VERBVERTRAGSTEIL-ELE PIC X OCCURS 18 TIMES.
         10 EA-NI-VERBVERTRAGSTEIL
                 REDEFINES EA-NI-VERBVERTRAGSTEIL-TAB.
           20 EA-NI-VVTVERTRAGSID            PIC X.
           20 EA-NI-VVTGUTHABPRODLFDNR       PIC X.
           20 EA-NI-VVTLAUFENDENUMMER        PIC X.
           20 EA-NI-VVTDARLEHENSSUMME        PIC X.
           20 EA-NI-VVTDARLSUMWKZ            PIC X.
           20 EA-NI-VVTDARLEHENSZINSS        PIC X.
           20 EA-NI-VVTRATE                  PIC X.
           20 EA-NI-VVTRATEWKZ               PIC X.
           20 EA-NI-VVTRESTSCHULD            PIC X.
           20 EA-NI-VVTRESTSCHULDWKZ         PIC X.
           20 EA-NI-VVTFESTGESCHBIS          PIC X.
           20 EA-NI-VVTTILGUNGSWERT          PIC X.
           20 EA-NI-VVTTILGWERTWKZ           PIC X.
           20 EA-NI-VVTTILGUNGSEINHEIT       PIC X.
           20 EA-NI-VVTTILGUNGDYN            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-VVTZWECK                 PIC X.
           20 EA-NI-VVTARTID                 PIC X.
         10 EA-DCL-VERBVERTRAGSTEIL.
           20 EA-VVTVERTRAGSID               PIC X(13).
           20 EA-VVTGUTHABPRODLFDNR          PIC -----9.
           20 EA-VVTLAUFENDENUMMER           PIC -----9.
           20 EA-VVTDARLEHENSSUMME           PIC ---------9.99.
           20 EA-VVTDARLSUMWKZ               PIC X(1).
           20 EA-VVTDARLEHENSZINSS           PIC ---9.99.
           20 EA-VVTRATE                     PIC ---------9.99.
           20 EA-VVTRATEWKZ                  PIC X(1).
           20 EA-VVTRESTSCHULD               PIC ---------9.99.
           20 EA-VVTRESTSCHULDWKZ            PIC X(1).
           20 EA-VVTFESTGESCHBIS             PIC X(010).
           20 EA-VVTTILGUNGSWERT             PIC ---------9.99.
           20 EA-VVTTILGWERTWKZ              PIC X(1).
           20 EA-VVTTILGUNGSEINHEIT          PIC -----9.
           20 EA-VVTTILGUNGDYN               PIC ---9.99.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-VVTZWECK                    PIC X(40).
           20 EA-VVTARTID                    PIC -----9.
      *****************************************************************
