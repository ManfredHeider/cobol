      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0302.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/WDKI0302.CPv  $
      *
      *   Rev 2.9   Oct 12 2006 12:15:16   t13ex27
      *AKI00624: DB-Änderung für Fix 93 EA-Programme
      *
      *   Rev 2.8   Oct 17 2005 08:57:04   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.7   Apr 04 2005 08:39:06   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.6   May 24 2004 10:53:00   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.5   Mar 10 2003 16:06:38   t13ex27
      *AKI00342 - DB-Änderung für Fixpack 36
      *
      *   Rev 2.4   Jan 21 2003 12:06:14   t13ex27
      *AKI00330 - DB-Änderung für Fixpack 33
      *
      *   Rev 2.3   13 Dec 2000 15:53:22   T13EX27
      *DB-Änderung BADENIA
      *
      *   Rev 2.2   29 Nov 2000 12:02:38   T13EX27
      *Datenbankaenderung Tabelle BAUSPAREN
      *
      *   Rev 2.1   Apr 16 1999 10:07:14   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:26   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BAUSPAREN-GESAMT.
         10 EA-NI-BAUSPAREN-TAB.
           20 EA-NI-BAUSPAREN-ELE PIC X OCCURS 41 TIMES.
         10 EA-NI-BAUSPAREN
                 REDEFINES EA-NI-BAUSPAREN-TAB.
           20 EA-NI-BASVERTRAGSID            PIC X.
           20 EA-NI-BASGUTHABPRODLFDNR       PIC X.
           20 EA-NI-BASKONTOSTANDKZID        PIC X.
           20 EA-NI-BASBERWERTUNGSZAHL       PIC X.
           20 EA-NI-BASDARLZSGEMTARIF        PIC X.
           20 EA-NI-BASDATKUMULWOHNBPR       PIC X.
           20 EA-NI-BASDATWOHNBAUPRAEM       PIC X.
           20 EA-NI-BASDATZUTEILAUSSI        PIC X.
           20 EA-NI-BASDISAGIOPROZENT        PIC X.
           20 EA-NI-BASENDESTEUERLFZ         PIC X.
           20 EA-NI-BASFEHLBETRMINANSP       PIC X.
           20 EA-NI-BASFEHLBETRMAWKZ         PIC X.
           20 EA-NI-BASKUMULWOHNBAUPR        PIC X.
           20 EA-NI-BASKUMWOHNBPRWKZ         PIC X.
           20 EA-NI-BASSTEUERBESCHEIKZ       PIC X.
           20 EA-NI-BASTARIFVARINTIII        PIC X.
           20 EA-NI-BASVERTRAGSSUMME         PIC X.
           20 EA-NI-BASVTSUMMEWKZ            PIC X.
           20 EA-NI-BASMINDESTANSPROZ        PIC X.
           20 EA-NI-BASPRODSCHL              PIC X.
           20 EA-NI-BASZINSPLUS              PIC X.
           20 EA-NI-BASBWZVORJAHR            PIC X.
           20 EA-NI-BASZINSLFDJAHR           PIC X.
           20 EA-NI-BASWOHNSPARKEY           PIC X.
           20 EA-NI-BASERHOEHEZBPROZ         PIC X.
           20 EA-NI-BASVERMITTELTKZ          PIC X.
           20 EA-NI-BASBLANKODARLKZ          PIC X.
           20 EA-NI-BASDATSPERRFR            PIC X.
           20 EA-NI-BASZINSVORTRAG           PIC X.
           20 EA-NI-BASDATAGEING             PIC X.
           20 EA-NI-BASFINPRODSCHL           PIC X.
           20 EA-NI-BASDATZINSFESTAB         PIC X.
           20 EA-NI-BASDATZINSFESTBIS        PIC X.
           20 EA-NI-BASZINSNOMINAL           PIC X.
           20 EA-NI-BASZINSEFFEKTIV          PIC X.
           20 EA-NI-BASANNURATE              PIC X.
           20 EA-NI-BASLAUFZEITFIN           PIC X.
           20 EA-NI-BASSONDTILGJAHR          PIC X.
           20 EA-NI-BASDARLHOEHE             PIC X.
           20 EA-NI-BASDARLSALDO             PIC X.
           20 EA-NI-BASDATAFBEGINN           PIC X.
         10 EA-DCL-BAUSPAREN.
           20 EA-BASVERTRAGSID               PIC X(13).
           20 EA-BASGUTHABPRODLFDNR          PIC -----9.
           20 EA-BASKONTOSTANDKZID           PIC -----9.
           20 EA-BASBERWERTUNGSZAHL          PIC ---9.99999.
           20 EA-BASDARLZSGEMTARIF           PIC ---9.99.
           20 EA-BASDATKUMULWOHNBPR          PIC X(010).
           20 EA-BASDATWOHNBAUPRAEM          PIC X(010).
           20 EA-BASDATZUTEILAUSSI           PIC X(010).
           20 EA-BASDISAGIOPROZENT           PIC ---9.99.
           20 EA-BASENDESTEUERLFZ            PIC X(010).
           20 EA-BASFEHLBETRMINANSP          PIC ---------9.99.
           20 EA-BASFEHLBETRMAWKZ            PIC X(1).
           20 EA-BASKUMULWOHNBAUPR           PIC ---------9.99.
           20 EA-BASKUMWOHNBPRWKZ            PIC X(1).
           20 EA-BASSTEUERBESCHEIKZ          PIC X(1).
           20 EA-BASTARIFVARINTIII           PIC X(1).
           20 EA-BASVERTRAGSSUMME            PIC ---------9.99.
           20 EA-BASVTSUMMEWKZ               PIC X(1).
           20 EA-BASMINDESTANSPROZ           PIC ---9.99.
           20 EA-BASPRODSCHL                 PIC X(2).
           20 EA-BASZINSPLUS                 PIC ---------9.99.
           20 EA-BASBWZVORJAHR               PIC ---9.99999.
           20 EA-BASZINSLFDJAHR              PIC ---------9.99.
           20 EA-BASWOHNSPARKEY              PIC -----9.
           20 EA-BASERHOEHEZBPROZ            PIC ---9.99.
           20 EA-BASVERMITTELTKZ             PIC X(1).
           20 EA-BASBLANKODARLKZ             PIC X(1).
           20 EA-BASDATSPERRFR               PIC X(010).
           20 EA-BASZINSVORTRAG              PIC ---------9.99.
           20 EA-BASDATAGEING                PIC X(010).
           20 EA-BASFINPRODSCHL              PIC -----9.
           20 EA-BASDATZINSFESTAB            PIC X(010).
           20 EA-BASDATZINSFESTBIS           PIC X(010).
           20 EA-BASZINSNOMINAL              PIC ---9.999.
           20 EA-BASZINSEFFEKTIV             PIC ---9.999.
           20 EA-BASANNURATE                 PIC ---------9.99.
           20 EA-BASLAUFZEITFIN              PIC -----9.
           20 EA-BASSONDTILGJAHR             PIC ---------9.99.
           20 EA-BASDARLHOEHE                PIC ---------9.99.
           20 EA-BASDARLSALDO                PIC ---------9.99.
           20 EA-BASDATAFBEGINN              PIC X(010).
      *****************************************************************
