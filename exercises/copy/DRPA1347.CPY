      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  MÄRZ 2000
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1347.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRPA1347.CPv  $
      *
      *   Rev 2.14   Jul 21 2020 14:30:24   t13ae37
      *APA02927: Karrieresystem 2021: alle Vertriebsgesellschaften 
      *(Provision) - Erweiterung des Loadbestandes 
      *
      *   Rev 2.13   Mar 28 2017 11:30:56   t13ae37
      *APA02353: Neuer Produktpartner TeleSon SST
      *
      *   Rev 2.12   Feb 28 2017 09:55:18   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.11   Feb 03 2016 14:59:04   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.10   Jul 16 2015 15:09:20   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.9   Oct 07 2013 11:03:16   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.8   May 08 2013 14:29:06   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.7   Jun 19 2012 09:29:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.6   May 21 2012 08:54:30   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   Aug 16 2010 10:05:36   T13EX44
      *APA01080: Erweiterung AVS-DAISY
      *
      *   Rev 2.4   Dec 10 2007 12:49:56   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.3   16 May 2007 06:45:14   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.2   Oct 19 2005 12:02:28   t13ex44
      *APA00388: Erweiterung SA815 (Satzlänge/3-stellige Prod-Schlü
      *ssel)
      *
      *   Rev 2.1   25 Apr 2000 10:17:20   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Mar 01 2000 09:57:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1347                                      *
      *    --------------------------------------------------------   *
      *    BEWEGUNGSDATEN AUS PAMS                                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KRUFF                                            *
      *    DATUM:    14.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00084!14.03.00!2.1 ! NEU                       ! WOLFF    *
      * APA00388!04.10.05!2.2 ! PDPT-FIRST-INSERT HINZU   ! SCHEIDEL *
      * APA00604!12.04.07!2.3 ! PEWA-ID ENTFERNT          ! SCHEIDEL *
      *         !        !    ! PAMS-PIN HINZU            !          *
      * APA00622!20.09.07!2.4 ! KNR-VGES HINZU            ! STORCH   *
      * APA01080!03.08.10!2.5 ! PAMS-INSERT HINZU         ! SCHEIDEL *
      * APA01101!23.02.12!2.6 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01526!04.04.13!2.8 ! AML-ATTRIBUTE HINZU       ! BUCH     *
      * APA01932!13.04.15!2.10! UST-ATTRIBUTE HINZU       ! BUCH     *
      * APA02208!29.01.16!2.11! UST-KORREKTUR-ATTRIBUTE   ! BUCH     *
      * APA02353!20.03.17!2.13! Teleson                   ! BUCH     *
      * APA02927!08.07.20!2.14! PEST-AVERM-KARR           ! BUCH     *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE BEWEGUNGSDATEN AUS  *
      *    PAMS.                                                     *
      *                                                              *
      ****************************************************************
      *
          30 PAMS-BEWEGUNGSDATEN.
             35 PDPT-ID                       PIC S9(015)
                                                  PACKED-DECIMAL.
             35 PDTA-ID                       PIC S9(015)
                                                  PACKED-DECIMAL.
             35 PGEB-GES                      PIC S9(004) BINARY.
             35 PVER-VNR                      PIC  X(015).
             35 PAMS-ABR-SORT-NR              PIC  X(015).
             35 PROD-PROD-SCHL                PIC S9(004) BINARY.
             35 PMPT-FAELLIG-VON              PIC  X(010).
             35 PRINTE                        PIC S9(015)
                                                  PACKED-DECIMAL.
             35 PVER-VERSION                  PIC S9(004) BINARY.
             35 PVER-KUNDE-NAME               PIC  X(025).
             35 PVER-KUNDE-VNAME              PIC  X(020).
             35 PMBE-RFOLGE-NR                PIC S9(004) BINARY.
             35 PMPT-ID                       PIC S9(015)
                                                  PACKED-DECIMAL.
             35 PMPT-VERSION                  PIC S9(004) BINARY.
             35 PMPT-GSMT-PROV                PIC S9(009)V99
                                                  PACKED-DECIMAL.
             35 PMPT-PPFL-BEITRAG             PIC S9(009)V99
                                                  PACKED-DECIMAL.
             35 PVPT-VT-PRIO                  PIC S9(004) BINARY.
             35 PMPE-PIN                      PIC S9(009) BINARY.
             35 PDPT-PRODART                  PIC S9(004) BINARY.
             35 PDPT-DGV                      PIC S9(004) BINARY.
             35 PMPT-EINH                     PIC S9(006)V99
                                                  PACKED-DECIMAL.
             35 KNR-BERCH-WAEHRUNG            PIC S9(004) BINARY.
             35 KNR-KZ-EH-MAX                 PIC S9(004) BINARY.
             35 KNR-BUCHUNGS-STAND            PIC S9(004) BINARY.
             35 PEST-AVERM                    PIC S9(009) BINARY.
             35 PEST-VM-GESCHFT-KZ            PIC  X(001).
             35 PDPT-FIRST-INSERT             PIC  X(026).
             35 KNR-MANDANT                   PIC S9(004) BINARY.
             35 PAMS-INSERT                   PIC  X(026).
             35 MAX-INDEX-PE                  PIC S9(004) BINARY.
 
040413       35 PMBE-UNR                      PIC S9(004) BINARY.
040413       35 PMBE-SCHICHT-NR               PIC S9(004) BINARY.
040413       35 KNR-TARIF-KZ                  PIC S9(004) BINARY.
040413       35 PVER-VP-NAME                  PIC  X(025).
040413       35 PVER-VP-VNAME                 PIC  X(020).
040413       35 PMPR-HAFTZ-VON                PIC  X(010).
040413       35 PMPR-HAFTZ-BIS                PIC  X(010).
040413*** IN DER PPFL-SUMME IST DIE PRO-RATA-SUMME GESPEICHERT
040413*** BEI GES20
040413       35 PMPR-PPFL-SUMME               PIC S9(009)V99 COMP-3.
040413*** IN DER PG-SUMME IST DIE PROVISIONSPFLICHTIGE SUMME GESPEICHERT
040413*** BEI GES20
040413       35 PMPR-PG-SUMME                 PIC S9(009)V99 COMP-3.
040413       35 KNR-DYN-KZ-1                  PIC S9(004) BINARY.
040413       35 KNR-DYN-KZ-2                  PIC S9(004) BINARY.
040413       35 PMBE-POL-DAT                  PIC  X(010).
040413       35 PMPR-PRO-RATA-Z               PIC S9(004) BINARY.
040413       35 PMPR-PRO-RATA-N               PIC S9(004) BINARY.
040413       35 PMBE-PROV-SCHL                PIC  X(005).
040413       35 KNR-RAB-ST                    PIC S9(004) BINARY.
040413       35 PMPR-RWERT-SUMME              PIC S9(009)V99 COMP-3.
040413       35 PMBE-SAM-NR                   PIC S9(009) BINARY.
040413       35 PMBE-BESVV                    PIC  X(001).
040413       35 PMPR-PGV                      PIC S9(004) BINARY.
040413       35 PVPT-BUZTAR                   PIC S9(004) BINARY.
040413       35 PMBE-INKST-DAT                PIC  X(010).
040413       35 PVER-VN-STRASSE               PIC  X(035).
040413       35 PVER-VN-PLZ                   PIC  X(005).
040413       35 PVER-VN-ORT                   PIC  X(035).
040413       35 PMPR-PG-PRODART               PIC S9(004) BINARY.
040413       35 KNR-ZAHLWEISE                 PIC S9(4) USAGE COMP.
040413       35 KNR-BEW-HRKFT                 PIC S9(4) USAGE COMP.
040413       35 PMPR-BEW-SCHL                 PIC S9(9) USAGE COMP.
040413       35 PMBE-VVG-KZ                   PIC X(1).
040413       35 PVER-URSPR-POL-DAT            PIC X(10).
040413       35 PMPR-PG-TARIF                 PIC X(15).
040413       35 PAMS-VERTRAGSDAUER            PIC S9(4) USAGE COMP.
130415       35 PAMS-KZ-KORR-BEWEG            PIC  X(1).
130415       35 PMPT-PG-UST-BETRAG            PIC S9(009)V99 COMP-3.
130415       35 PMPT-PG-UST-SATZ              PIC  9(03)V9(3) COMP-3.
200317*      35 PMBE-PG-RECH-NR               PIC  X(12).
200317       35 PMBE-PG-RECH-NR               PIC  X(30).
130415       35 PMPT-FAELLIG-BIS              PIC  X(10).
290116       35 PDPT-UST-JAHR                 PIC S9(04) BINARY.
290116       35 PAMS-UST-JAHR-KZ              PIC X(01).
130217       35 PMPT-VSP-KOMBI-NR             PIC S9(005) BINARY.
130217       35 PMPT-VSP-KOMBI-BZ             PIC  X(003).
130217       35 PMPT-VSP-FN-BER               PIC  X(001).
130217       35 PMPT-VSP-FN-REL               PIC  X(001).
130217       35 PMPT-VSP-FN-FP-KZ             PIC  X(001).
080720       35 PEST-AVERM-KARR               PIC S9(009) BINARY.
140921       35 PMPR-NBB-SUMME                PIC S9(009)V99 COMP-3.
140921       35 PMPR-AUW-SUMME                PIC S9(009)V99 COMP-3.
 
      ******* ENDE COPY-MEMBER **************************************
 
 
 


