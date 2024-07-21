      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2350.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2350.CPv  $
      *
      *   Rev 2.7   Nov 24 2020 09:04:06   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.6   Dec 08 2015 12:52:08   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens - DWS
      *
      *   Rev 2.0   Dec 7 2015 13:10:55 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2350                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2350                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:52 / U130685
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
      *    FÜR DIE FUNKTION DRKI2350
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2350.
           05  PI-DRKI2350-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2350-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2350-GHPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2350.
           05  PO-DRKI2350-RC                     PIC  X(003).
               88 PO-DRKI2350-RC-OK               VALUE 'OK '.
               88 PO-DRKI2350-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI2350-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2350-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2350-GHPANLAGENAME.
                49 PO-DRKI2350-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PO-DRKI2350-GHPANLAGENAME-TEXT PIC X(15).
             10 PO-DRKI2350-GHPANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI2350-GHPBRPERFGESBEG   PIC  X(010).
             10 PO-DRKI2350-GHPBRPERFGESEND   PIC  X(010).
             10 PO-DRKI2350-GHPBRPERFLFJBEG   PIC  X(010).
             10 PO-DRKI2350-GHPBRPERFLFJEND   PIC  X(010).
             10 PO-DRKI2350-GHPDEPOTART   PIC  X(0001).
             10 PO-DRKI2350-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2350-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PO-DRKI2350-GHPESAFE   PIC  X(0001).
             10 PO-DRKI2350-GHPFAXREVERS   PIC  X(0001).
             10 PO-DRKI2350-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2350-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI2350-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2350-GHPGUTHABENWKZ   PIC  X(0001).
             10 PO-DRKI2350-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PO-DRKI2350-GHPINFOSPERREDATUM   PIC  X(010).
             10 PO-DRKI2350-GHPINFOWPERREKZ   PIC  X(0001).
             10 PO-DRKI2350-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PO-DRKI2350-GHPONLINEREVERS   PIC  X(0001).
             10 PO-DRKI2350-GHPPERFDAT   PIC  X(010).
             10 PO-DRKI2350-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI2350-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI2350-GHPPORTFOLIONR   PIC  X(0010).
             10 PO-DRKI2350-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2350-GHPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI2350-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI2350-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2350-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PO-DRKI2350-GHPSCHLIESSDATUM   PIC  X(010).
             10 PO-DRKI2350-GHPSUBTYPE   PIC  X(0001).
             10 PO-DRKI2350-GHPTELEFONREVERS   PIC  X(0001).
             10 PO-DRKI2350-GHPVERFUEGUNGID   PIC  X(0002).
             10 PO-DRKI2350-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2350-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2350-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
