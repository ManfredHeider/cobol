      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2831.CPY  $
      *    $Revision:   2.21  $
      *
      *    $Log:   L:/mf/CPY/DRKI2831.CPv  $
      *
      *   Rev 2.21   Nov 24 2020 08:47:36   u130685
      *AKI01805: Bankprodukt OZP/Copy
      *
      *   Rev 2.20   Apr 17 2020 13:37:42   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.19   Jan 22 2020 10:45:18   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen
      *
      *   Rev 2.18   Jun 19 2019 14:02:22   t13ex39
      *AKI01687: Deutsche Bank: Anzeige gebührenbefreites Konto im
      *Bestandsvertrag inkl. Erweiterung der Selektionen
      *
      *   Rev 2.17   Mar 18 2019 17:40:44   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.16   Nov 29 2018 09:48:10   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.15   Jan 25 2018 06:53:42   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.13   Aug 25 2016 09:52:38   t13ex15
      *AKI01449: Deutsche Bank Konto: Datum des Saldos ZTKI0465
      *
      *   Rev 2.11   Nov 06 2015 12:30:42   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens OZP ZTKI0465
      *
      *   Rev 2.10   Oct 21 2015 12:43:34   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.9   Sep 29 2015 12:26:36   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.8   Aug 14 2015 16:30:26   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.7   Mar 17 2015 14:39:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Jan 13 2015 13:19:26   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.5   Jan 16 2014 14:43:34   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.4   Jan 16 2014 13:26:14   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.3   Jan 16 2014 13:08:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.2   Jan 15 2014 15:27:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.1   Jan 15 2014 13:05:10   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.0   Jan 15 2014 13:03:54   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2831                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2831                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 13:47:04 / U130685
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
      *    FÜR DIE FUNKTION DRKI2831
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2831.
           05  PI-DRKI2831-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2831-BKPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2831-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2831-N-BKPBLNZSCHLKNR    PIC S9(4) COMP.
             15 PI-DRKI2831-N-BKPBLNZSCHLKTY    PIC S9(4) COMP.
             15 PI-DRKI2831-NI-BKPDETAILKNR     PIC S9(4) COMP.
             15 PI-DRKI2831-NI-BKPDETAILKTY     PIC S9(4) COMP.
END   *

           05  PI-DRKI2831-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2831-BKPABRECHKONTO   PIC  X(0010).
             10 PI-DRKI2831-BKPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2831-BKPANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPANTEILRATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPBIC   PIC  X(0011).
             10 PI-DRKI2831-BKPBILANZSCHLKNR   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPBILANZSCHLKTY   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPBLZKUNDEN   PIC  X(0008).
             10 PI-DRKI2831-BKPDARLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPDARLBETRAKT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPDARLEHENSART   PIC  X(0001).
             10 PI-DRKI2831-BKPDATBEGENDE   PIC  X(010).
             10 PI-DRKI2831-BKPDATEROFFNUNG   PIC  X(010).
             10 PI-DRKI2831-BKPDATFAELLIG   PIC  X(010).
             10 PI-DRKI2831-BKPDATKREDITAUSZ   PIC  X(010).
             10 PI-DRKI2831-BKPDATKREDITZUS   PIC  X(010).
             10 PI-DRKI2831-BKPDATSCHLIESS   PIC  X(010).
             10 PI-DRKI2831-BKPDATUMSALDO   PIC  X(010).
             10 PI-DRKI2831-BKPDATVOLLAUSZ   PIC  X(010).
             10 PI-DRKI2831-BKPDETAILKNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2831-BKPDETAILKTY   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPEIGENVERMKZ   PIC  X(0001).
             10 PI-DRKI2831-BKPENDEZINSFEST   PIC  X(010).
             10 PI-DRKI2831-BKPESAFE   PIC  X(0001).
             10 PI-DRKI2831-BKPEZRFREIJAHRE   PIC  X(010).
             10 PI-DRKI2831-BKPGESAMTLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPIBAN   PIC  X(0034).
             10 PI-DRKI2831-BKPKFWDATFEST   PIC  X(010).
             10 PI-DRKI2831-BKPKFWERSTR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPKFWFOLGER   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPKFWKREDITB   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPKFWSOLLZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPKONDSCHL   PIC  X(0002).
             10 PI-DRKI2831-BKPKONTOART   PIC  X(0003).
             10 PI-DRKI2831-BKPKOSTSCHLZKP   PIC  X(0001).
             10 PI-DRKI2831-BKPKREDITART   PIC  X(0006).
             10 PI-DRKI2831-BKPLIMITEXT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPONLREVERSKZ   PIC  X(0001).
             10 PI-DRKI2831-BKPPIN   PIC S9(9) COMP.
             10 PI-DRKI2831-BKPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2831-BKPRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPRESTLAUFZEIT   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPSICHPAKET   PIC  X(0001).
             10 PI-DRKI2831-BKPSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPTELREVERSKZ   PIC  X(0001).
             10 PI-DRKI2831-BKPTILGUNGANF   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI2831-BKPTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPTILGUNGSART   PIC  X(0001).
             10 PI-DRKI2831-BKPUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2831-BKPULTOSPARKZ   PIC  X(0001).
             10 PI-DRKI2831-BKPVERSUMFANG1   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPVERSUMFANG2   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPVERTRAGSENDE   PIC  X(010).
             10 PI-DRKI2831-BKPWAEHRSCHL   PIC S9(5)V COMP-3.
             10 PI-DRKI2831-BKPZAHLWEISEKNR   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPZAHLWEISEKTY   PIC S9(4) COMP.
             10 PI-DRKI2831-BKPZINSANTEIL   PIC S9(12)V9(2) COMP-3.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2831-NI1.
           05 PI-DRKI2831-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 4.
       01  PI-DRKI2831-NI7.
           05 PI-DRKI2831-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 4.

      *
       01  PO-DRKI2831.
           05  PO-DRKI2831-RC                     PIC  X(003).
               88 PO-DRKI2831-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2831-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
