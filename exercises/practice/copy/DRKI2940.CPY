      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2940.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2940.CPv  $
      *
      *   Rev 2.8   Nov 24 2020 09:04:52   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.7   Feb 24 2020 11:42:00   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.6   Mar 09 2018 16:12:04   t13ex78
      *AKI01565: OZPs und Copies - TCKI0315
      *
      *   Rev 2.4   Jan 16 2018 09:42:58   t13ex78
      *AKI01562: OZPs und Copies
      *
      *   Rev 2.3   Jan 07 2016 14:07:30   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.2   Nov 06 2015 12:29:02   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.1   Nov 27 2014 09:45:40   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.0   Oct 21 2014 12:39:11 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2940                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2940                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:19:01 / U130685
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
      *    FÜR DIE FUNKTION DRKI2940
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2940.
           05  PI-DRKI2940-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2940-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2940-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2940-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2940-NI-GHPPERFGESAMT    PIC S9(4) COMP.
             15 PI-DRKI2940-NI-GHPPERFLFDJAHR   PIC S9(4) COMP.
             15 PI-DRKI2940-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
             15 PI-DRKI2940-NI-GHPWPHGDAT       PIC S9(4) COMP.
END   *

           05  PI-DRKI2940-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2940-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2940-GHPANLAGENAME.
                49 PI-DRKI2940-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI2940-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI2940-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI2940-GHPBLZKUNDEN   PIC  X(0008).
             10 PI-DRKI2940-GHPBRPERFGESBEG   PIC  X(010).
             10 PI-DRKI2940-GHPBRPERFGESEND   PIC  X(010).
             10 PI-DRKI2940-GHPBRPERFLFJBEG   PIC  X(010).
             10 PI-DRKI2940-GHPBRPERFLFJEND   PIC  X(010).
             10 PI-DRKI2940-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI2940-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI2940-GHPEMAILADRESSE.
                49 PI-DRKI2940-GHPMLDRSS-LEN PIC S9(4) COMP.
                49 PI-DRKI2940-GHPMLDRSS-TEXT PIC X(128).
             10 PI-DRKI2940-GHPESAFE   PIC  X(0001).
             10 PI-DRKI2940-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI2940-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI2940-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI2940-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI2940-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI2940-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI2940-GHPNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2940-GHPNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2940-GHPNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2940-GHPNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2940-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI2940-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI2940-GHPPERFDAT   PIC  X(010).
             10 PI-DRKI2940-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2940-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2940-GHPPORTFOLIONR   PIC  X(0010).
             10 PI-DRKI2940-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2940-GHPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2940-GHPRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2940-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI2940-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI2940-GHPSONDERKONDI   PIC  X(0001).
             10 PI-DRKI2940-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI2940-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI2940-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI2940-GHPWPHGDAT   PIC  X(010).
             10 PI-DRKI2940-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2940-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2940-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2940-NI1.
           05 PI-DRKI2940-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 4.
       01  PI-DRKI2940-NI7.
           05 PI-DRKI2940-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 4.

      *
       01  PO-DRKI2940.
           05  PO-DRKI2940-RC                     PIC  X(003).
               88 PO-DRKI2940-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2940-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
