      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3395.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3395.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:47:22   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Aug 30 2018 15:28:00 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3395                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3395                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.10.2018 / 12:58:55 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3395
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3395.
           05  PI-DRKI3395-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3395-G42ZKLFDNR   PIC S9(9) COMP.
             10 PI-DRKI3395-G42ZKVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3395.
           05  PO-DRKI3395-RC                     PIC  X(003).
               88 PO-DRKI3395-RC-OK               VALUE 'OK '.
               88 PO-DRKI3395-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3395-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3395-G42ZKANREDE   PIC  X(0001).
             10 PO-DRKI3395-G42ZKGEBDAT   PIC  X(0008).
             10 PO-DRKI3395-G42ZKGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3395-G42ZKLKZ   PIC  X(0003).
             10 PO-DRKI3395-G42ZKNACHNAME   PIC  X(0035).
             10 PO-DRKI3395-G42ZKORT   PIC  X(0040).
             10 PO-DRKI3395-G42ZKPLZ   PIC  X(0009).
             10 PO-DRKI3395-G42ZKSTRHNR   PIC  X(0040).
             10 PO-DRKI3395-G42ZKVORNAME   PIC  X(0035).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

