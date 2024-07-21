      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3045.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3045.CPv  $
      *
      *   Rev 2.2   Nov 24 2020 09:05:22   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.1   Nov 06 2015 12:29:26   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.0   Nov 6 2015 11:47:08 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3045                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3045                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:19:05 / U130685
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
      *    FÜR DIE FUNKTION DRKI3045
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3045.
           05  PI-DRKI3045-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3045-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3045-GHPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3045.
           05  PO-DRKI3045-RC                     PIC  X(003).
               88 PO-DRKI3045-RC-OK               VALUE 'OK '.
               88 PO-DRKI3045-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3045-NI.
               10 NI-DRKI3045-TAB.
                  15 DRKI3045-ELE
                     PIC S9(04) COMP OCCURS 4 TIMES.
               10 NI-DRKI3045 REDEFINES NI-DRKI3045-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3045-N-GHPBSCHLSSVBNR   PIC S9(4) COMP.
             15 PO-DRKI3045-N-GHPFFNNGSDTM   PIC S9(4) COMP.
             15 PO-DRKI3045-NI-GHPPROVISIONKZ   PIC S9(4) COMP.
             15 PO-DRKI3045-N-GHPSCHLSSDTM   PIC S9(4) COMP.
END   *

           05  PO-DRKI3045-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3045-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI3045-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PO-DRKI3045-GHPESAFE   PIC  X(0001).
             10 PO-DRKI3045-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PO-DRKI3045-GHPONLINEREVERS   PIC  X(0001).
             10 PO-DRKI3045-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI3045-GHPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI3045-GHPSCHLIESSDATUM   PIC  X(010).
             10 PO-DRKI3045-GHPTELEFONREVERS   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
