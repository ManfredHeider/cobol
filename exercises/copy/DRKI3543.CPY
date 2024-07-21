      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3543.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3543.CPv  $
      *
      *   Rev 2.1   Jan 30 2019 11:35:40   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.0   Dec 18 2018 06:25:50 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3543                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3543                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.12.2018 / 06:25:50 / T13EX39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3543
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3543.
           05  PI-DRKI3543-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3543-G48ZKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3543-G48ZKVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3543.
           05  PO-DRKI3543-RC                     PIC  X(003).
               88 PO-DRKI3543-RC-OK               VALUE 'OK '.
               88 PO-DRKI3543-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3543-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3543-G48ZKANREDE   PIC  X(0001).
             10 PO-DRKI3543-G48ZKDELKZ   PIC  X(0001).
             10 PO-DRKI3543-G48ZKDEPOTKZ   PIC  X(0001).
             10 PO-DRKI3543-G48ZKGEBDAT   PIC  X(0008).
             10 PO-DRKI3543-G48ZKGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3543-G48ZKHAUSNR   PIC  X(0009).
             10 PO-DRKI3543-G48ZKLKZ   PIC  X(0003).
             10 PO-DRKI3543-G48ZKNACHNAME   PIC  X(0035).
             10 PO-DRKI3543-G48ZKORT   PIC  X(0028).
             10 PO-DRKI3543-G48ZKPLZ   PIC  X(0009).
             10 PO-DRKI3543-G48ZKPOSTFACH   PIC  X(0009).
             10 PO-DRKI3543-G48ZKSTR   PIC  X(0028).
             10 PO-DRKI3543-G48ZKTELNR   PIC  X(0009).
             10 PO-DRKI3543-G48ZKTELVOR   PIC  X(0006).
             10 PO-DRKI3543-G48ZKVORNAME   PIC  X(0035).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

