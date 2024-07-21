      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2830.CPY  $
      *    $Revision:   2.22  $
      *
      *    $Log:   L:/mf/CPY/DRKI2830.CPv  $
      *
      *   Rev 2.22   Nov 24 2020 08:47:20   u130685
      *AKI01805: Bankprodukt OZP/Copy
      *
      *   Rev 2.21   Apr 17 2020 13:37:34   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.20   Jan 22 2020 10:44:32   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen
      *
      *   Rev 2.19   Jun 19 2019 14:01:46   t13ex39
      *AKI01687: Deutsche Bank: Anzeige gebührenbefreites Konto im
      *Bestandsvertrag inkl. Erweiterung der Selektionen
      *
      *   Rev 2.18   Mar 18 2019 17:40:06   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.17   Nov 29 2018 09:46:24   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.16   Jan 25 2018 06:53:28   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.14   Aug 25 2016 09:52:32   t13ex15
      *AKI01449: Deutsche Bank Konto: Datum des Saldos ZTKI0465
      *
      *   Rev 2.12   Nov 06 2015 12:30:48   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens OZP ZTKI0465
      *
      *   Rev 2.11   Oct 21 2015 12:43:28   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.10   Sep 29 2015 12:26:30   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.9   Aug 14 2015 16:30:20   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.7   Mar 17 2015 14:39:36   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Jan 13 2015 13:19:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.5   Jan 16 2014 14:43:28   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.4   Jan 16 2014 13:26:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.3   Jan 16 2014 13:08:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.2   Jan 15 2014 15:27:46   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.1   Jan 15 2014 13:05:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.0   Jan 15 2014 13:03:48   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2830                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2830                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 13:47:01 / U130685
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
      *    FÜR DIE FUNKTION DRKI2830
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2830.
           05  PI-DRKI2830-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2830-BKPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2830-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2830-N-BKPBLNZSCHLKNR    PIC S9(4) COMP.
             15 PI-DRKI2830-N-BKPBLNZSCHLKTY    PIC S9(4) COMP.
             15 PI-DRKI2830-NI-BKPDETAILKNR     PIC S9(4) COMP.
             15 PI-DRKI2830-NI-BKPDETAILKTY     PIC S9(4) COMP.
END   *

           05  PI-DRKI2830-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2830-BKPABRECHKONTO   PIC  X(0010).
             10 PI-DRKI2830-BKPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2830-BKPANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPANTEILRATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPBIC   PIC  X(0011).
             10 PI-DRKI2830-BKPBILANZSCHLKNR   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPBILANZSCHLKTY   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPBLZKUNDEN   PIC  X(0008).
             10 PI-DRKI2830-BKPDARLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPDARLBETRAKT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPDARLEHENSART   PIC  X(0001).
             10 PI-DRKI2830-BKPDATBEGENDE   PIC  X(010).
             10 PI-DRKI2830-BKPDATEROFFNUNG   PIC  X(010).
             10 PI-DRKI2830-BKPDATFAELLIG   PIC  X(010).
             10 PI-DRKI2830-BKPDATKREDITAUSZ   PIC  X(010).
             10 PI-DRKI2830-BKPDATKREDITZUS   PIC  X(010).
             10 PI-DRKI2830-BKPDATSCHLIESS   PIC  X(010).
             10 PI-DRKI2830-BKPDATUMSALDO   PIC  X(010).
             10 PI-DRKI2830-BKPDATVOLLAUSZ   PIC  X(010).
             10 PI-DRKI2830-BKPDETAILKNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2830-BKPDETAILKTY   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPEIGENVERMKZ   PIC  X(0001).
             10 PI-DRKI2830-BKPENDEZINSFEST   PIC  X(010).
             10 PI-DRKI2830-BKPESAFE   PIC  X(0001).
             10 PI-DRKI2830-BKPEZRFREIJAHRE   PIC  X(010).
             10 PI-DRKI2830-BKPGESAMTLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPIBAN   PIC  X(0034).
             10 PI-DRKI2830-BKPKFWDATFEST   PIC  X(010).
             10 PI-DRKI2830-BKPKFWERSTR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPKFWFOLGER   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPKFWKREDITB   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPKFWSOLLZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPKONDSCHL   PIC  X(0002).
             10 PI-DRKI2830-BKPKONTOART   PIC  X(0003).
             10 PI-DRKI2830-BKPKOSTSCHLZKP   PIC  X(0001).
             10 PI-DRKI2830-BKPKREDITART   PIC  X(0006).
             10 PI-DRKI2830-BKPLIMITEXT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPONLREVERSKZ   PIC  X(0001).
             10 PI-DRKI2830-BKPPIN   PIC S9(9) COMP.
             10 PI-DRKI2830-BKPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2830-BKPRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPRESTLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPSICHPAKET   PIC  X(0001).
             10 PI-DRKI2830-BKPSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPTELREVERSKZ   PIC  X(0001).
             10 PI-DRKI2830-BKPTILGUNGANF   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2830-BKPTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPTILGUNGSART   PIC  X(0001).
             10 PI-DRKI2830-BKPUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2830-BKPULTOSPARKZ   PIC  X(0001).
             10 PI-DRKI2830-BKPVERSUMFANG1   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPVERSUMFANG2   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPVERTRAGSENDE   PIC  X(010).
             10 PI-DRKI2830-BKPWAEHRSCHL   PIC S9(5)V COMP-3.
             10 PI-DRKI2830-BKPZAHLWEISEKNR   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPZAHLWEISEKTY   PIC S9(4) COMP.
             10 PI-DRKI2830-BKPZINSANTEIL   PIC S9(12)V9(2) COMP-3.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2830-NI1.
           05 PI-DRKI2830-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 4.
       01  PI-DRKI2830-NI7.
           05 PI-DRKI2830-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 4.
      *
       01  PO-DRKI2830.
           05  PO-DRKI2830-RC                     PIC  X(003).
               88 PO-DRKI2830-RC-OK               VALUE 'OK '.
               88 PO-DRKI2830-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
