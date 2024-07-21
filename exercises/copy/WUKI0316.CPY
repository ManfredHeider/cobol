      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0316.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/WUKI0316.CPv  $
      *
      *   Rev 2.16   Jan 21 2021 17:05:52   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.15   Jan 20 2021 14:09:46   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.14   Jun 29 2011 07:55:54   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.13   Jun 28 2010 10:18:14   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: DWS
      *
      *   Rev 2.12   Aug 12 2009 12:06:28   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60-DWS
      *
      *   Rev 2.11   Oct 12 2006 13:35:50   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.10   Jul 24 2006 09:47:32   t13ex27
      *AKI00604: DB-Änderung für Fix 90 - PMKI0079
      *
      *   Rev 2.9   Dec 12 2005 09:17:56   t13ex27
      *AKI00533: DB-Änderung für Fix 82 PMKI0079 und WUKI's
      *
      *   Rev 2.8   Oct 17 2005 10:07:20   t13ex27
      *AKI00521: DB-Änderung für Fix 80 PMKI0079 und WUKI's
      *
      *   Rev 2.7   Oct 18 2004 09:44:28   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.6   Jun 11 2003 13:11:32   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:14   p13sp16
      *Archiverstellung
      *
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0316.
           10 FOZVERTRAGSID                 PIC  X(0013).
           10 FOZGUTHABPRODLFDNR            PIC S9(4) COMP.
           10 FOZFONDID                     PIC S9(4) COMP.
           10 FOZLFDNR                      PIC S9(4) COMP.
           10 FOZZAHLUNGSWEISEID            PIC S9(4) COMP.
           10 NIL-FOZZAHLUNGSWEISEID        PIC  X(01).
           10 FOZPROZSATZDYNAMIS            PIC S9(3)V9(2) COMP-3.
           10 NIL-FOZPROZSATZDYNAMIS        PIC  X(01).
           10 FOZZAHLUNGWKZ                 PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 FOZEINAUSDATUM                PIC S9(4) COMP.
           10 NIL-FOZEINAUSDATUM            PIC  X(01).
           10 FOZEINAUSTYP                  PIC  X(0001).
           10 FOZBUCHUNGSTEXTID             PIC S9(4) COMP.
           10 NIL-FOZBUCHUNGSTEXTID         PIC  X(01).
           10 FOZZAHLUNG                    PIC S9(9)V9(2) COMP-3.
           10 NIL-FOZZAHLUNG                PIC  X(01).
           10 FOZEINZAHFRISTEND             PIC  X(010).
           10 NIL-FOZEINZAHFRISTEND         PIC  X(01).
           10 FOZSPARPLANEROEF              PIC  X(010).
           10 NIL-FOZSPARPLANEROEF          PIC  X(01).
           10 FOZSPARPLANSCHLUSS            PIC  X(010).
           10 NIL-FOZSPARPLANSCHLUSS        PIC  X(01).
           10 FOZKONTOINHABER               PIC  X(0027).
           10 NIL-FOZKONTOINHABER           PIC  X(01).
           10 FOZBANKNAME                   PIC  X(0020).
           10 NIL-FOZBANKNAME               PIC  X(01).
           10 FOZBANKBLZ                    PIC  X(0011).
           10 NIL-FOZBANKBLZ                PIC  X(01).
           10 FOZBANKKONTONR                PIC  X(0018).
           10 NIL-FOZBANKKONTONR            PIC  X(01).
           10 FOZDYNAMIK                    PIC S9(1)V9(2) COMP-3.
           10 NIL-FOZDYNAMIK                PIC  X(01).
           10 FOZDATBEGINNAUS               PIC  X(010).
           10 NIL-FOZDATBEGINNAUS           PIC  X(01).
           10 FOZDATENDEAUS                 PIC  X(010).
           10 NIL-FOZDATENDEAUS             PIC  X(01).
           10 FOZKOSTENPROZ                 PIC S9(1)V9(2) COMP-3.
           10 NIL-FOZKOSTENPROZ             PIC  X(01).
           10 FOZURSPRSPARLST               PIC S9(6)V9(2) COMP-3.
           10 NIL-FOZURSPRSPARLST           PIC  X(01).
           10 FOZURSPRSPARLSTWKZ            PIC  X(0001).
           10 NIL-FOZURSPRSPARLSTWKZ        PIC  X(01).
           10 FOZURSPARRATE                 PIC S9(7)V9(2) COMP-3.
           10 NIL-FOZURSPARRATE             PIC  X(01).
           10 FOZURSPARRATEWKZ              PIC  X(0001).
           10 NIL-FOZURSPARRATEWKZ          PIC  X(01).
           10 FOZAAUFSCHLURSPR              PIC S9(2)V9(2) COMP-3.
           10 NIL-FOZAAUFSCHLURSPR          PIC  X(01).
           10 FOZAAUFSCHLAKTUELL            PIC S9(2)V9(2) COMP-3.
           10 NIL-FOZAAUFSCHLAKTUELL        PIC  X(01).
           10 FOZVERTRLAUFZEIT              PIC S9(4) COMP.
           10 NIL-FOZVERTRLAUFZEIT          PIC  X(01).
           10 FOZBEGDATDYNAM                PIC  X(010).
           10 NIL-FOZBEGDATDYNAM            PIC  X(01).
           10 FOZZLASTSCHRRUECK             PIC S9(4) COMP.
           10 NIL-FOZZLASTSCHRRUECK         PIC  X(01).
           10 FOZKUENDMERKMAL               PIC  X(0001).
           10 NIL-FOZKUENDMERKMAL           PIC  X(01).
           10 FOZDATVERTRAUFLOE             PIC  X(010).
           10 NIL-FOZDATVERTRAUFLOE         PIC  X(01).
           10 FOZVERTRAGSTYP                PIC  X(0002).
           10 NIL-FOZVERTRAGSTYP            PIC  X(01).
           10 FOZBONIFIKATION               PIC S9(3)V9(4) COMP-3.
           10 NIL-FOZBONIFIKATION           PIC  X(01).
           10 FOZURSPRVNR                   PIC S9(12)V COMP-3.
           10 NIL-FOZURSPRVNR               PIC  X(01).
           10 FOZMAHNSTATUS                 PIC  X(0002).
           10 NIL-FOZMAHNSTATUS             PIC  X(01).
           10 FOZDAUERZULAGEKZ              PIC  X(0001).
           10 NIL-FOZDAUERZULAGEKZ          PIC  X(01).
           10 FOZZULBERECHTKZ               PIC  X(0001).
           10 NIL-FOZZULBERECHTKZ           PIC  X(01).
           10 FOZWEGKAPUEBERTR              PIC  X(0001).
           10 NIL-FOZWEGKAPUEBERTR          PIC  X(01).
           10 FOZUEBERTRAGART               PIC  X(0001).
           10 NIL-FOZUEBERTRAGART           PIC  X(01).
           10 FOZDATKAPUEBERTR              PIC  X(010).
           10 NIL-FOZDATKAPUEBERTR          PIC  X(01).
           10 FOZDATGELDBEWEG               PIC  X(010).
           10 NIL-FOZDATGELDBEWEG           PIC  X(01).
           10 FOZANZKINDERZUL               PIC S9(4) COMP.
           10 NIL-FOZANZKINDERZUL           PIC  X(01).
           10 FOZGARANTIESUMME              PIC S9(13)V9(2) COMP-3.
           10 NIL-FOZGARANTIESUMME          PIC  X(01).
           10 FOZDATBEGABLSTAB              PIC  X(010).
           10 NIL-FOZDATBEGABLSTAB          PIC  X(01).
           10 FOZDATBEGHOECHSI              PIC  X(010).
           10 NIL-FOZDATBEGHOECHSI          PIC  X(01).
           10 FOZDATLETHOECHST              PIC  X(010).
           10 NIL-FOZDATLETHOECHST          PIC  X(01).
           10 FOZBETRHOECHSTST              PIC S9(13)V9(2) COMP-3.
           10 NIL-FOZBETRHOECHSTST          PIC  X(01).
           10 FOZDATKUENDIGUNG              PIC  X(010).
           10 NIL-FOZDATKUENDIGUNG          PIC  X(01).
           10 FOZPIN                        PIC S9(9) COMP.
           10 NIL-FOZPIN                    PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 FOZABRKTO-KTY                 PIC S9(4) COMP.
           10 NIL-FOZABRKTO-KTY             PIC  X(01).
           10 FOZABRKTO-KNR                 PIC S9(4) COMP.
           10 NIL-FOZABRKTO-KNR             PIC  X(01).
