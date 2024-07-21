      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0302.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/WUKI0302.CPv  $
      *
      *   Rev 2.19   Apr 15 2020 08:12:08   t13ex78
      *AKI01771: Standard OZP ZTKI0302
      *
      *   Rev 2.18   Aug 11 2016 08:44:08   t13ex73
      *AKI01448: Badenia - Vereinigen GHP und Bausparen
      *
      *   Rev 2.17   Dec 17 2014 09:56:10   t13ex73
      *AKI01330: AKI01330 - Migration stornierter Verträge
      *
      *   Rev 2.16   Nov 15 2012 14:12:40   t13ex14
      *AKI01157: Badenia: Behandlung übersparter Verträge OZP
      *
      *   Rev 2.15   Jun 02 2010 09:48:00   t13ex14
      *AKI00970: DB-Änderungen für Rel. 5.12: Badenia
      *
      *   Rev 2.14   Oct 07 2009 10:40:42   t13ex14
      *AKI00873: DB-Änderungen für Rel. 4.62 - Badenia
      *
      *   Rev 2.13   Sep 11 2009 07:56:12   t13ex14
      *AKI00843: DB-Änderungen für Rel. 4.60 - Badenia
      *
      *   Rev 2.12   05 Sep 2008 07:35:46   t13ex27
      *AKI00710: DB-Änderung Tabelle BAUSPAREN
      *
      *   Rev 2.11   03 Sep 2008 08:29:08   t13ex27
      *AKI00710: DB-Änderung Tabelle BAUSPAREN
      *
      *   Rev 2.10   Oct 12 2006 13:35:00   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.9   Oct 17 2005 10:07:04   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.8   Apr 04 2005 08:39:56   t13ex27
      *AKI00474: DB-Änderung für Fix 73 EA-Programme
      *
      *   Rev 2.7   May 24 2004 11:44:22   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jun 11 2003 13:10:30   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0302.
           10 BASVERTRAGSID                 PIC  X(0013).
           10 BASGUTHABPRODLFDNR            PIC S9(4) COMP.
           10 BASKONTOSTANDKZID             PIC S9(4) COMP.
           10 NIL-BASKONTOSTANDKZID         PIC  X(01).
           10 BASBERWERTUNGSZAHL            PIC S9(3)V9(5) COMP-3.
           10 NIL-BASBERWERTUNGSZAHL        PIC  X(01).
           10 BASDARLZSGEMTARIF             PIC S9(3)V9(2) COMP-3.
           10 NIL-BASDARLZSGEMTARIF         PIC  X(01).
           10 BASDATKUMULWOHNBPR            PIC  X(010).
           10 NIL-BASDATKUMULWOHNBPR        PIC  X(01).
           10 BASDATWOHNBAUPRAEM            PIC  X(010).
           10 NIL-BASDATWOHNBAUPRAEM        PIC  X(01).
           10 BASDATZUTEILAUSSI             PIC  X(010).
           10 NIL-BASDATZUTEILAUSSI         PIC  X(01).
           10 BASDISAGIOPROZENT             PIC S9(3)V9(2) COMP-3.
           10 NIL-BASDISAGIOPROZENT         PIC  X(01).
           10 BASENDESTEUERLFZ              PIC  X(010).
           10 NIL-BASENDESTEUERLFZ          PIC  X(01).
           10 BASFEHLBETRMINANSP            PIC S9(9)V9(2) COMP-3.
           10 NIL-BASFEHLBETRMINANSP        PIC  X(01).
           10 BASFEHLBETRMAWKZ              PIC  X(0001).
           10 BASKUMULWOHNBAUPR             PIC S9(9)V9(2) COMP-3.
           10 NIL-BASKUMULWOHNBAUPR         PIC  X(01).
           10 BASKUMWOHNBPRWKZ              PIC  X(0001).
           10 BASSTEUERBESCHEIKZ            PIC  X(0001).
           10 BASTARIFVARINTIII             PIC  X(0001).
           10 BASVERTRAGSSUMME              PIC S9(9)V9(2) COMP-3.
           10 NIL-BASVERTRAGSSUMME          PIC  X(01).
           10 BASVTSUMMEWKZ                 PIC  X(0001).
           10 BASMINDESTANSPROZ             PIC S9(3)V9(2) COMP-3.
           10 NIL-BASMINDESTANSPROZ         PIC  X(01).
           10 BASPRODSCHL                   PIC  X(0002).
           10 NIL-BASPRODSCHL               PIC  X(01).
           10 BASBONUSVORJAHR               PIC S9(9)V9(2) COMP-3.
           10 NIL-BASBONUSVORJAHR           PIC  X(01).
           10 BASBWZVORJAHR                 PIC S9(3)V9(5) COMP-3.
           10 NIL-BASBWZVORJAHR             PIC  X(01).
           10 BASZINSLFDJAHR                PIC S9(9)V9(2) COMP-3.
           10 NIL-BASZINSLFDJAHR            PIC  X(01).
           10 BASWOHNSPARKEY                PIC S9(4) COMP.
           10 NIL-BASWOHNSPARKEY            PIC  X(01).
           10 BASERHOEHEZBPROZ              PIC S9(3)V9(2) COMP-3.
           10 NIL-BASERHOEHEZBPROZ          PIC  X(01).
           10 BASVERMITTELTKZ               PIC  X(0001).
           10 NIL-BASVERMITTELTKZ           PIC  X(01).
           10 BASBLANKODARLKZ               PIC  X(0001).
           10 NIL-BASBLANKODARLKZ           PIC  X(01).
           10 BASDATSPERRFR                 PIC  X(010).
           10 NIL-BASDATSPERRFR             PIC  X(01).
           10 BASZINSVORTRAG                PIC S9(9)V9(2) COMP-3.
           10 NIL-BASZINSVORTRAG            PIC  X(01).
           10 BASDATAGEING                  PIC  X(010).
           10 NIL-BASDATAGEING              PIC  X(01).
           10 BASFINPRODSCHL                PIC S9(4) COMP.
           10 NIL-BASFINPRODSCHL            PIC  X(01).
           10 BASDATZINSFESTAB              PIC  X(010).
           10 NIL-BASDATZINSFESTAB          PIC  X(01).
           10 BASDATZINSFESTBIS             PIC  X(010).
           10 NIL-BASDATZINSFESTBIS         PIC  X(01).
           10 BASZINSNOMINAL                PIC S9(3)V9(3) COMP-3.
           10 NIL-BASZINSNOMINAL            PIC  X(01).
           10 BASZINSEFFEKTIV               PIC S9(3)V9(3) COMP-3.
           10 NIL-BASZINSEFFEKTIV           PIC  X(01).
           10 BASANNURATE                   PIC S9(9)V9(2) COMP-3.
           10 NIL-BASANNURATE               PIC  X(01).
           10 BASLAUFZEITFIN                PIC S9(4) COMP.
           10 NIL-BASLAUFZEITFIN            PIC  X(01).
           10 BASSONDTILGJAHR               PIC S9(9)V9(2) COMP-3.
           10 NIL-BASSONDTILGJAHR           PIC  X(01).
           10 BASDARLHOEHE                  PIC S9(9)V9(2) COMP-3.
           10 NIL-BASDARLHOEHE              PIC  X(01).
           10 BASDARLSALDO                  PIC S9(9)V9(2) COMP-3.
           10 NIL-BASDARLSALDO              PIC  X(01).
           10 BASDATAFBEGINN                PIC  X(010).
           10 NIL-BASDATAFBEGINN            PIC  X(01).
           10 BASBONUSFAKTOR                PIC S9(2)V9(2) COMP-3.
           10 NIL-BASBONUSFAKTOR            PIC  X(01).
           10 BASBONUSAKTUELL               PIC S9(9)V9(2) COMP-3.
           10 NIL-BASBONUSAKTUELL           PIC  X(01).
           10 BASZINSPLUSGENER              PIC S9(2)V COMP-3.
           10 NIL-BASZINSPLUSGENER          PIC  X(01).
           10 BASBEITRAG                    PIC S9(9)V9(2) COMP-3.
           10 NIL-BASBEITRAG                PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 BASEINZBETRAGMON              PIC S9(7)V9(2) COMP-3.
           10 NIL-BASEINZBETRAGMON          PIC  X(01).
           10 BASEINZGDATUMMON              PIC  X(010).
           10 NIL-BASEINZGDATUMMON          PIC  X(01).
           10 BASEINZBETRAGJHR              PIC S9(7)V9(2) COMP-3.
           10 NIL-BASEINZBETRAGJHR          PIC  X(01).
           10 BASEINZGDATUMJHR              PIC  X(010).
           10 NIL-BASEINZGDATUMJHR          PIC  X(01).
           10 BASEINZUGDYNAM                PIC S9(3)V9(2) COMP-3.
           10 NIL-BASEINZUGDYNAM            PIC  X(01).
           10 BASMINBEWERTZAHL              PIC S9(3)V9(3) COMP-3.
           10 NIL-BASMINBEWERTZAHL          PIC  X(01).
           10 BASSTEUERLFZDAT               PIC  X(010).
           10 NIL-BASSTEUERLFZDAT           PIC  X(01).
           10 BASWOPKZ                      PIC  X(0001).
           10 NIL-BASWOPKZ                  PIC  X(01).
           10 BASDAUERZULAGEKZ              PIC  X(0001).
           10 NIL-BASDAUERZULAGEKZ          PIC  X(01).
           10 BASVARIANWECHSKZ              PIC  X(0001).
           10 NIL-BASVARIANWECHSKZ          PIC  X(01).
           10 BASFINANZIERNGKZ              PIC  X(0001).
           10 NIL-BASFINANZIERNGKZ          PIC  X(01).
           10 BASZUTEILANWDAT               PIC  X(010).
           10 NIL-BASZUTEILANWDAT           PIC  X(01).
           10 BASDARLEHNAKTEKZ              PIC  X(0001).
           10 NIL-BASDARLEHNAKTEKZ          PIC  X(01).
           10 BASENDEBINDFRIST              PIC  X(010).
           10 NIL-BASENDEBINDFRIST          PIC  X(01).
           10 BASLETZTEDATAKTU              PIC  X(010).
           10 NIL-BASLETZTEDATAKTU          PIC  X(01).
           10 BASZIELTILGUNG                PIC S9(3)V9(2) COMP-3.
           10 NIL-BASZIELTILGUNG            PIC  X(01).
           10 BASMEHRZUTEILUNG              PIC S9(7)V9(2) COMP-3.
           10 NIL-BASMEHRZUTEILUNG          PIC  X(01).
           10 BASABTRETUNGBLZ               PIC S9(9) COMP.
           10 NIL-BASABTRETUNGBLZ           PIC  X(01).
           10 BASTARIFUMSTBTRG              PIC S9(5)V9(2) COMP-3.
           10 NIL-BASTARIFUMSTBTRG          PIC  X(01).
           10 BASANFTILGUNG                 PIC S9(3)V9(2) COMP-3.
           10 NIL-BASANFTILGUNG             PIC  X(01).
           10 BASLETZTEZUTEIL               PIC  X(010).
           10 NIL-BASLETZTEZUTEIL           PIC  X(01).
           10 BASZUTEILANWART               PIC  X(010).
           10 NIL-BASZUTEILANWART           PIC  X(01).
           10 BASWOPANSPRUCH                PIC S9(7)V9(2) COMP-3.
           10 NIL-BASWOPANSPRUCH            PIC  X(01).
           10 BASSPARZULFREIAB              PIC  X(010).
           10 NIL-BASSPARZULFREIAB          PIC  X(01).
           10 BASVWLKZ                      PIC  X(0001).
           10 NIL-BASVWLKZ                  PIC  X(01).
           10 BASIMMOFITKZ                  PIC  X(0001).
           10 NIL-BASIMMOFITKZ              PIC  X(01).
           10 BASWERBVERBOTKZ               PIC  X(0001).
           10 NIL-BASWERBVERBOTKZ           PIC  X(01).
           10 BASDATGHBZINSBEGR             PIC  X(010).
           10 NIL-BASDATGHBZINSBEGR         PIC  X(01).
           10 BASMAXZINSKAPITAL             PIC S9(9)V9(2) COMP-3.
           10 NIL-BASMAXZINSKAPITAL         PIC  X(01).
           10 BASMIETKAUTIONKZ              PIC  X(0001).
           10 NIL-BASMIETKAUTIONKZ          PIC  X(01).
           10 BASPASSWVEREINB               PIC  X(0001).
           10 NIL-BASPASSWVEREINB           PIC  X(01).
           10 BASDATLETZTERKAZ              PIC  X(010).
           10 NIL-BASDATLETZTERKAZ          PIC  X(01).
           10 BASNRLETZTERKAZ               PIC  X(0007).
           10 NIL-BASNRLETZTERKAZ           PIC  X(01).
           10 BASDATNVBESCHEIN              PIC  X(010).
           10 NIL-BASDATNVBESCHEIN          PIC  X(01).
           10 BASVLPERSONALTKZ              PIC  X(0001).
           10 NIL-BASVLPERSONALTKZ          PIC  X(01).
           10 BASONLKIPROD-KTY              PIC S9(4) COMP.
           10 NIL-BASONLKIPROD-KTY          PIC  X(01).
           10 BASONLKIPROD-KNR              PIC S9(15)V COMP-3.
           10 NIL-BASONLKIPROD-KNR          PIC  X(01).
           10 BASONLKIART-KTY               PIC S9(4) COMP.
           10 NIL-BASONLKIART-KTY           PIC  X(01).
           10 BASONLKIART-KNR               PIC S9(15)V COMP-3.
           10 NIL-BASONLKIART-KNR           PIC  X(01).
           10 BASFRONTTEC-KTY               PIC S9(4) COMP.
           10 NIL-BASFRONTTEC-KTY           PIC  X(01).
           10 BASFRONTTEC-KNR               PIC S9(15)V COMP-3.
           10 NIL-BASFRONTTEC-KNR           PIC  X(01).
           10 ABB-VERSION                   PIC  X(0007).
           10 NIL-ABB-VERSION               PIC  X(01).
           10 BASPRODUKTID                  PIC S9(4) COMP.
           10 BASSCHLIESSDATUM              PIC  X(010).
           10 NIL-BASSCHLIESSDATUM          PIC  X(01).
           10 BASZINSSATZ                   PIC S9(3)V9(2) COMP-3.
           10 NIL-BASZINSSATZ               PIC  X(01).
           10 BASGUTHABEN                   PIC S9(9)V9(2) COMP-3.
           10 NIL-BASGUTHABEN               PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 BASBONUSART                   PIC  X(0002).
           10 NIL-BASBONUSART               PIC  X(01).
           10 BASBONUSBEGDATUM              PIC  X(010).
           10 NIL-BASBONUSBEGDATUM          PIC  X(01).
           10 BASTUBAKTION                  PIC  X(0001).
           10 NIL-BASTUBAKTION              PIC  X(01).
           10 BASSONDERZAHLUNG              PIC S9(3)V9(2) COMP-3.
           10 NIL-BASSONDERZAHLUNG          PIC  X(01).
           10 BASSONDERTILGUNG              PIC S9(3)V9(2) COMP-3.
           10 NIL-BASSONDERTILGUNG          PIC  X(01).

