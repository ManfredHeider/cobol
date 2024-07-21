      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0048.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0048.CPv  $
      *
      *   Rev 2.3   Apr 04 2005 08:38:58   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.2   Oct 15 2002 08:15:40   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:04:58   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:12   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PERSREFERENZNUMMER-GESAMT.
         10 EA-NI-PERSREFERENZNUMMER-TAB.
           20 EA-NI-PERSREFERENZNUMMER-ELE PIC X OCCURS 8 TIMES.
         10 EA-NI-PERSREFERENZNUMMER
                 REDEFINES EA-NI-PERSREFERENZNUMMER-TAB.
           20 EA-NI-PREFNUMPERSONENID        PIC X.
           20 EA-NI-PREFNUMARTID             PIC X.
           20 EA-NI-PREFNUMLFDNUMMER         PIC X.
           20 EA-NI-PREFNUMMER               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-PREFAUSGESTELLTAM        PIC X.
           20 EA-NI-PREFAUSGESTELLTORT       PIC X.
           20 EA-NI-PREFGUELTIGBIS           PIC X.
         10 EA-DCL-PERSREFERENZNUMMER.
           20 EA-PREFNUMPERSONENID           PIC X(13).
           20 EA-PREFNUMARTID                PIC -----9.
           20 EA-PREFNUMLFDNUMMER            PIC ----------9.
           20 EA-PREFNUMMER                  PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-PREFAUSGESTELLTAM           PIC X(010).
           20 EA-PREFAUSGESTELLTORT.
              49 EA-PREFAUSGESTELLTORT-LEN   PIC -----9.
              49 EA-PREFAUSGESTELLTORT-TEXT  PIC X(30).
           20 EA-PREFGUELTIGBIS              PIC X(010).
      *****************************************************************
