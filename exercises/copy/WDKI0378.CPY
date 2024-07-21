      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0378.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0378.CPv  $
      *
      *   Rev 2.3   Apr 04 2005 08:39:28   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.2   13 Jul 2000 10:09:52   T13EX30
      *AKI00122
      *Keine Änderung
      *
      *   Rev 2.1   Apr 16 1999 10:11:16   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-KRTARIFARTBEZ-GESAMT.
         10 EA-NI-KRTARIFARTBEZ-TAB.
           20 EA-NI-KRTARIFARTBEZ-ELE PIC X OCCURS 16 TIMES.
         10 EA-NI-KRTARIFARTBEZ
                 REDEFINES EA-NI-KRTARIFARTBEZ-TAB.
           20 EA-NI-KRTVERTRAGSID            PIC X.
           20 EA-NI-KRTVERSTARLFDNR          PIC X.
           20 EA-NI-KRTKVTARIFARTID          PIC X.
           20 EA-NI-KRTDATABGANGVERTR        PIC X.
           20 EA-NI-KRTDATZUGANGVERTR        PIC X.
           20 EA-NI-KRTMONATSBEITRAG         PIC X.
           20 EA-NI-KRTMONBEITRWKZ           PIC X.
           20 EA-NI-KRTSELBSTBETEILIG        PIC X.
           20 EA-NI-KRTSELBSTBETWKZ          PIC X.
           20 EA-NI-KRTTARIFNAMEN            PIC X.
           20 EA-NI-KRTRISIKOZUSCHLAG        PIC X.
           20 EA-NI-KRTVERSLEISTUNG          PIC X.
           20 EA-NI-KRTVERSLSTWKZ            PIC X.
           20 EA-NI-KRTGUTSCHRIFT            PIC X.
           20 EA-NI-KRTGUTSCHRWKZ            PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-KRTARIFARTBEZ.
           20 EA-KRTVERTRAGSID               PIC X(13).
           20 EA-KRTVERSTARLFDNR             PIC -----9.
           20 EA-KRTKVTARIFARTID             PIC -----9.
           20 EA-KRTDATABGANGVERTR           PIC X(010).
           20 EA-KRTDATZUGANGVERTR           PIC X(010).
           20 EA-KRTMONATSBEITRAG            PIC ---------9.99.
           20 EA-KRTMONBEITRWKZ              PIC X(1).
           20 EA-KRTSELBSTBETEILIG           PIC ---------9.99.
           20 EA-KRTSELBSTBETWKZ             PIC X(1).
           20 EA-KRTTARIFNAMEN               PIC X(8).
           20 EA-KRTRISIKOZUSCHLAG           PIC X(10).
           20 EA-KRTVERSLEISTUNG             PIC ---------9.99.
           20 EA-KRTVERSLSTWKZ               PIC X(1).
           20 EA-KRTGUTSCHRIFT               PIC ---------9.99.
           20 EA-KRTGUTSCHRWKZ               PIC X(1).
           20 EA-LETZTEAENDERUNG             PIC X(026).
      *****************************************************************
