      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2938.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2938.CPv  $
      *
      *   Rev 2.8   Nov 24 2020 09:04:22   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.7   Feb 24 2020 11:40:54   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.6   Mar 09 2018 16:11:40   t13ex78
      *AKI01565: OZPs und Copies - TCKI0315
      *
      *   Rev 2.4   Jan 16 2018 09:42:44   t13ex78
      *AKI01562: OZPs und Copies
      *
      *   Rev 2.3   Jan 07 2016 14:07:42   t13ex39
      *AKI01397: Kennzeichen für Sonderkonditionen
      *
      *   Rev 2.2   Nov 06 2015 12:29:12   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.1   Nov 27 2014 09:45:30   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.0   Oct 21 2014 12:39:05 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2938                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2938                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:57 / U130685
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
      *    FÜR DIE FUNKTION DRKI2938
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2938.
           05  PI-DRKI2938-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2938-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2938-GHPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2938.
           05  PO-DRKI2938-RC                     PIC  X(003).
               88 PO-DRKI2938-RC-OK               VALUE 'OK '.
               88 PO-DRKI2938-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2938-NI.
               10 NI-DRKI2938-TAB.
                  15 DRKI2938-ELE
                     PIC S9(04) COMP OCCURS 2 TIMES.
               10 NI-DRKI2938 REDEFINES NI-DRKI2938-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2938-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.
             15 PO-DRKI2938-NI-GHPWPHGDAT   PIC S9(4) COMP.
END   *

           05  PO-DRKI2938-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2938-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2938-GHPANLAGENAME.
                49 PO-DRKI2938-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PO-DRKI2938-GHPANLAGENAME-TEXT PIC X(15).
             10 PO-DRKI2938-GHPANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI2938-GHPBLZKUNDEN   PIC  X(0008).
             10 PO-DRKI2938-GHPBRPERFGESBEG   PIC  X(010).
             10 PO-DRKI2938-GHPBRPERFGESEND   PIC  X(010).
             10 PO-DRKI2938-GHPBRPERFLFJBEG   PIC  X(010).
             10 PO-DRKI2938-GHPBRPERFLFJEND   PIC  X(010).
             10 PO-DRKI2938-GHPDEPOTART   PIC  X(0001).
             10 PO-DRKI2938-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PO-DRKI2938-GHPEMAILADRESSE.
                49 PO-DRKI2938-GHPMLDRSS-LEN PIC S9(4) COMP.
                49 PO-DRKI2938-GHPMLDRSS-TEXT PIC X(128).
             10 PO-DRKI2938-GHPESAFE   PIC  X(0001).
             10 PO-DRKI2938-GHPFAXREVERS   PIC  X(0001).
             10 PO-DRKI2938-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI2938-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPGUTHABENWKZ   PIC  X(0001).
             10 PO-DRKI2938-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PO-DRKI2938-GHPINFOSPERREDATUM   PIC  X(010).
             10 PO-DRKI2938-GHPINFOWPERREKZ   PIC  X(0001).
             10 PO-DRKI2938-GHPNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2938-GHPNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI2938-GHPNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2938-GHPNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI2938-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PO-DRKI2938-GHPONLINEREVERS   PIC  X(0001).
             10 PO-DRKI2938-GHPPERFDAT   PIC  X(010).
             10 PO-DRKI2938-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI2938-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI2938-GHPPORTFOLIONR   PIC  X(0010).
             10 PO-DRKI2938-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2938-GHPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI2938-GHPRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI2938-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PO-DRKI2938-GHPSCHLIESSDATUM   PIC  X(010).
             10 PO-DRKI2938-GHPSONDERKONDI   PIC  X(0001).
             10 PO-DRKI2938-GHPSUBTYPE   PIC  X(0001).
             10 PO-DRKI2938-GHPTELEFONREVERS   PIC  X(0001).
             10 PO-DRKI2938-GHPVERFUEGUNGID   PIC  X(0002).
             10 PO-DRKI2938-GHPWPHGDAT   PIC  X(010).
             10 PO-DRKI2938-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2938-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2938-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
