      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2939.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2939.CPv  $
      *
      *   Rev 2.8   Nov 24 2020 09:04:36   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.7   Feb 24 2020 11:41:28   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.6   Mar 09 2018 16:11:52   t13ex78
      *AKI01565: OZPs und Copies - TCKI0315
      *
      *   Rev 2.4   Jan 16 2018 09:42:52   t13ex78
      *AKI01562: OZPs und Copies
      *
      *   Rev 2.3   Jan 07 2016 14:07:36   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.2   Nov 06 2015 12:29:08   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.1   Nov 27 2014 09:45:34   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.0   Oct 21 2014 12:39:08 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2939                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2939                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:59 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *---------!--------!--------------------------------!----------*
      * AKI01805!13.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2939
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2939.
           05  PI-DRKI2939-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2939-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2939-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2939-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2939-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
             15 PI-DRKI2939-NI-GHPWPHGDAT       PIC S9(4) COMP.
END   *

           05  PI-DRKI2939-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2939-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2939-GHPANLAGENAME.
                49 PI-DRKI2939-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI2939-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI2939-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI2939-GHPBLZKUNDEN   PIC  X(0008).
             10 PI-DRKI2939-GHPBRPERFGESBEG   PIC  X(010).
             10 PI-DRKI2939-GHPBRPERFGESEND   PIC  X(010).
             10 PI-DRKI2939-GHPBRPERFLFJBEG   PIC  X(010).
             10 PI-DRKI2939-GHPBRPERFLFJEND   PIC  X(010).
             10 PI-DRKI2939-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI2939-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI2939-GHPEMAILADRESSE.
                49 PI-DRKI2939-GHPMLDRSS-LEN PIC S9(4) COMP.
                49 PI-DRKI2939-GHPMLDRSS-TEXT PIC X(128).
             10 PI-DRKI2939-GHPESAFE   PIC  X(0001).
             10 PI-DRKI2939-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI2939-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI2939-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI2939-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI2939-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI2939-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI2939-GHPNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2939-GHPNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2939-GHPNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2939-GHPNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2939-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI2939-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI2939-GHPPERFDAT   PIC  X(010).
             10 PI-DRKI2939-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2939-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2939-GHPPORTFOLIONR   PIC  X(0010).
             10 PI-DRKI2939-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2939-GHPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2939-GHPRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2939-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI2939-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI2939-GHPSONDERKONDI   PIC  X(0001).
             10 PI-DRKI2939-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI2939-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI2939-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI2939-GHPWPHGDAT   PIC  X(010).
             10 PI-DRKI2939-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2939-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2939-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2939-NI1.
           05 PI-DRKI2939-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 2.
       01  PI-DRKI2939-NI7.
           05 PI-DRKI2939-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 2.
      *
       01  PO-DRKI2939.
           05  PO-DRKI2939-RC                     PIC  X(003).
               88 PO-DRKI2939-RC-OK               VALUE 'OK '.
               88 PO-DRKI2939-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
