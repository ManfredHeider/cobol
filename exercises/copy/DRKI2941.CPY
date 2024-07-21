      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2941.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2941.CPv  $
      *
      *   Rev 2.3   Nov 24 2020 09:05:06   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.2   Feb 24 2020 11:43:08   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.1   Nov 27 2014 09:45:46   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.0   Oct 21 2014 12:39:14 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2941                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2941                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:19:04 / U130685
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
      *    FÜR DIE FUNKTION DRKI2941
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2941.
           05  PI-DRKI2941-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2941-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2941-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2941-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2941-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2941-GHPBLZKUNDEN   PIC  X(0008).
             10 PI-DRKI2941-GHPESAFE   PIC  X(0001).
             10 PI-DRKI2941-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI2941-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI2941-GHPVERFUEGUNGID   PIC  X(0002).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2941-NI1.
           05 PI-DRKI2941-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2941-NI7.
           05 PI-DRKI2941-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2941.
           05  PO-DRKI2941-RC                     PIC  X(003).
               88 PO-DRKI2941-RC-OK               VALUE 'OK '.
               88 PO-DRKI2941-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
