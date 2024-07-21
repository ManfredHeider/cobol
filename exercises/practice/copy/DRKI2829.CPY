      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2829.CPY  $
      *    $Revision:   2.21  $
      *
      *    $Log:   L:/mf/CPY/DRKI2829.CPv  $
      *
      *   Rev 2.21   Nov 24 2020 08:47:02   u130685
      *AKI01805: Bankprodukt OZP/Copy
      *
      *   Rev 2.20   Apr 17 2020 13:37:24   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.19   Jan 22 2020 10:43:50   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen
      *
      *   Rev 2.18   Jun 19 2019 14:01:12   t13ex39
      *AKI01687: Deutsche Bank: Anzeige gebührenbefreites Konto im
      *Bestandsvertrag inkl. Erweiterung der Selektionen
      *
      *   Rev 2.17   Mar 18 2019 17:39:26   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.16   Nov 29 2018 09:43:58   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.15   Jan 25 2018 06:53:14   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.13   Aug 25 2016 09:52:24   t13ex15
      *AKI01449: Deutsche Bank Konto: Datum des Saldos ZTKI0465
      *
      *   Rev 2.11   Nov 06 2015 12:30:52   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens OZP ZTKI0465
      *
      *   Rev 2.10   Oct 21 2015 12:43:22   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.9   Sep 29 2015 12:26:24   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.8   Aug 14 2015 16:30:14   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.7   Mar 17 2015 14:39:32   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Jan 13 2015 13:19:14   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.5   Jan 16 2014 14:43:22   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.4   Jan 16 2014 13:26:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.3   Jan 16 2014 13:08:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.2   Jan 15 2014 15:27:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.1   Jan 15 2014 13:04:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.0   Jan 15 2014 13:03:40   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2829                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2829                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 13:46:58 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01805!23.11.20!    ! ATTRIBUT ESAFE NEU        !U130685   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2829
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2829.
           05  PI-DRKI2829-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2829-BKPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2829-BKPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2829.
           05  PO-DRKI2829-RC                     PIC  X(003).
               88 PO-DRKI2829-RC-OK               VALUE 'OK '.
               88 PO-DRKI2829-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2829-NI.
               10 NI-DRKI2829-TAB.
                  15 DRKI2829-ELE
                     PIC S9(04) COMP OCCURS 4 TIMES.
               10 NI-DRKI2829 REDEFINES NI-DRKI2829-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2829-N-BKPBLNZSCHLKNR   PIC S9(4) COMP.
             15 PO-DRKI2829-N-BKPBLNZSCHLKTY   PIC S9(4) COMP.
             15 PO-DRKI2829-NI-BKPDETAILKNR   PIC S9(4) COMP.
             15 PO-DRKI2829-NI-BKPDETAILKTY   PIC S9(4) COMP.
END   *

           05  PO-DRKI2829-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2829-BKPABRECHKONTO   PIC  X(0010).
             10 PO-DRKI2829-BKPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2829-BKPANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPANTEILRATE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPBIC   PIC  X(0011).
             10 PO-DRKI2829-BKPBILANZSCHLKNR   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPBILANZSCHLKTY   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPBLZKUNDEN   PIC  X(0008).
             10 PO-DRKI2829-BKPDARLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPDARLBETRAKT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPDARLEHENSART   PIC  X(0001).
             10 PO-DRKI2829-BKPDATBEGENDE   PIC  X(010).
             10 PO-DRKI2829-BKPDATEROFFNUNG   PIC  X(010).
             10 PO-DRKI2829-BKPDATFAELLIG   PIC  X(010).
             10 PO-DRKI2829-BKPDATKREDITAUSZ   PIC  X(010).
             10 PO-DRKI2829-BKPDATKREDITZUS   PIC  X(010).
             10 PO-DRKI2829-BKPDATSCHLIESS   PIC  X(010).
             10 PO-DRKI2829-BKPDATUMSALDO   PIC  X(010).
             10 PO-DRKI2829-BKPDATVOLLAUSZ   PIC  X(010).
             10 PO-DRKI2829-BKPDETAILKNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2829-BKPDETAILKTY   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPEIGENVERMKZ   PIC  X(0001).
             10 PO-DRKI2829-BKPENDEZINSFEST   PIC  X(010).
             10 PO-DRKI2829-BKPESAFE   PIC  X(0001).
             10 PO-DRKI2829-BKPEZRFREIJAHRE   PIC  X(010).
             10 PO-DRKI2829-BKPGESAMTLAUFZEIT   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPIBAN   PIC  X(0034).
             10 PO-DRKI2829-BKPKFWDATFEST   PIC  X(010).
             10 PO-DRKI2829-BKPKFWERSTR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPKFWFOLGER   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPKFWKREDITB   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPKFWSOLLZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPKONDSCHL   PIC  X(0002).
             10 PO-DRKI2829-BKPKONTOART   PIC  X(0003).
             10 PO-DRKI2829-BKPKOSTSCHLZKP   PIC  X(0001).
             10 PO-DRKI2829-BKPKREDITART   PIC  X(0006).
             10 PO-DRKI2829-BKPLIMITEXT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPONLREVERSKZ   PIC  X(0001).
             10 PO-DRKI2829-BKPPIN   PIC S9(9) COMP.
             10 PO-DRKI2829-BKPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI2829-BKPRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPRESTLAUFZEIT   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPSICHPAKET   PIC  X(0001).
             10 PO-DRKI2829-BKPSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPTELREVERSKZ   PIC  X(0001).
             10 PO-DRKI2829-BKPTILGUNGANF   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-BKPTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPTILGUNGSART   PIC  X(0001).
             10 PO-DRKI2829-BKPUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2829-BKPULTOSPARKZ   PIC  X(0001).
             10 PO-DRKI2829-BKPVERSUMFANG1   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPVERSUMFANG2   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPVERTRAGSENDE   PIC  X(010).
             10 PO-DRKI2829-BKPWAEHRSCHL   PIC S9(5)V COMP-3.
             10 PO-DRKI2829-BKPZAHLWEISEKNR   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPZAHLWEISEKTY   PIC S9(4) COMP.
             10 PO-DRKI2829-BKPZINSANTEIL   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2829-LETZTEAENDERUNG   PIC  X(0026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
