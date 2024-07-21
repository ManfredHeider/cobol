      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0316.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WDKI0316.CPv  $
      *
      *   Rev 2.6   Oct 12 2006 12:16:08   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.5   Dec 12 2005 07:38:12   t13ex27
      *AKI00533: DB-Änderung für Fix 82 EA-Programme und WDKI's
      *
      *   Rev 2.4   Oct 17 2005 08:57:18   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.3   Oct 18 2004 09:43:40   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.2   Oct 15 2002 08:15:54   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   Apr 16 1999 10:07:28   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:30   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GUTHABENZAHLUNG-GESAMT.
         10 EA-NI-GUTHABENZAHLUNG-TAB.
           20 EA-NI-GUTHABENZAHLUNG-ELE PIC X OCCURS 35 TIMES.
         10 EA-NI-GUTHABENZAHLUNG
                 REDEFINES EA-NI-GUTHABENZAHLUNG-TAB.
           20 EA-NI-FOZVERTRAGSID            PIC X.
           20 EA-NI-FOZGUTHABPRODLFDNR       PIC X.
           20 EA-NI-FOZFONDID                PIC X.
           20 EA-NI-FOZLFDNR                 PIC X.
           20 EA-NI-FOZZAHLUNGSWEISEID       PIC X.
           20 EA-NI-FOZPROZSATZDYNAMIS       PIC X.
           20 EA-NI-FOZZAHLUNGWKZ            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-FOZEINAUSDATUM           PIC X.
           20 EA-NI-FOZEINAUSTYP             PIC X.
           20 EA-NI-FOZBUCHUNGSTEXTID        PIC X.
           20 EA-NI-FOZZAHLUNG               PIC X.
           20 EA-NI-FOZEINZAHFRISTEND        PIC X.
           20 EA-NI-FOZSPARPLANEROEF         PIC X.
           20 EA-NI-FOZSPARPLANSCHLUSS       PIC X.
           20 EA-NI-FOZKONTOINHABER          PIC X.
           20 EA-NI-FOZBANKNAME              PIC X.
           20 EA-NI-FOZBANKBLZ               PIC X.
           20 EA-NI-FOZBANKKONTONR           PIC X.
           20 EA-NI-FOZDYNAMIK               PIC X.
           20 EA-NI-FOZDATBEGINNAUS          PIC X.
           20 EA-NI-FOZDATENDEAUS            PIC X.
           20 EA-NI-FOZKOSTENPROZ            PIC X.
           20 EA-NI-FOZURSPRSPARLST          PIC X.
           20 EA-NI-FOZURSPRSPARLSTWKZ       PIC X.
           20 EA-NI-FOZURSPARRATE            PIC X.
           20 EA-NI-FOZURSPARRATEWKZ         PIC X.
           20 EA-NI-FOZAAUFSCHLURSPR         PIC X.
           20 EA-NI-FOZAAUFSCHLAKTUELL       PIC X.
           20 EA-NI-FOZVERTRLAUFZEIT         PIC X.
           20 EA-NI-FOZBEGDATDYNAM           PIC X.
           20 EA-NI-FOZZLASTSCHRRUECK        PIC X.
           20 EA-NI-FOZKUENDMERKMAL          PIC X.
           20 EA-NI-FOZDATVERTRAUFLOE        PIC X.
           20 EA-NI-FOZVERTRAGSTYP           PIC X.
         10 EA-DCL-GUTHABENZAHLUNG.
           20 EA-FOZVERTRAGSID               PIC X(13).
           20 EA-FOZGUTHABPRODLFDNR          PIC -----9.
           20 EA-FOZFONDID                   PIC -----9.
           20 EA-FOZLFDNR                    PIC -----9.
           20 EA-FOZZAHLUNGSWEISEID          PIC -----9.
           20 EA-FOZPROZSATZDYNAMIS          PIC ---9.99.
           20 EA-FOZZAHLUNGWKZ               PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-FOZEINAUSDATUM              PIC -----9.
           20 EA-FOZEINAUSTYP                PIC X(1).
           20 EA-FOZBUCHUNGSTEXTID           PIC -----9.
           20 EA-FOZZAHLUNG                  PIC ---------9.99.
           20 EA-FOZEINZAHFRISTEND           PIC X(010).
           20 EA-FOZSPARPLANEROEF            PIC X(010).
           20 EA-FOZSPARPLANSCHLUSS          PIC X(010).
           20 EA-FOZKONTOINHABER             PIC X(27).
           20 EA-FOZBANKNAME                 PIC X(20).
           20 EA-FOZBANKBLZ                  PIC X(11).
           20 EA-FOZBANKKONTONR              PIC X(18).
           20 EA-FOZDYNAMIK                  PIC -9.99.
           20 EA-FOZDATBEGINNAUS             PIC X(010).
           20 EA-FOZDATENDEAUS               PIC X(010).
           20 EA-FOZKOSTENPROZ               PIC -9.99.
           20 EA-FOZURSPRSPARLST             PIC ------9.99.
           20 EA-FOZURSPRSPARLSTWKZ          PIC X(1).
           20 EA-FOZURSPARRATE               PIC ----9.99.
           20 EA-FOZURSPARRATEWKZ            PIC X(1).
           20 EA-FOZAAUFSCHLURSPR            PIC --9.99.
           20 EA-FOZAAUFSCHLAKTUELL          PIC --9.99.
           20 EA-FOZVERTRLAUFZEIT            PIC -----9.
           20 EA-FOZBEGDATDYNAM              PIC X(010).
           20 EA-FOZZLASTSCHRRUECK           PIC -----9.
           20 EA-FOZKUENDMERKMAL             PIC X(1).
           20 EA-FOZDATVERTRAUFLOE           PIC X(010).
           20 EA-FOZVERTRAGSTYP              PIC X(2).
      *****************************************************************
