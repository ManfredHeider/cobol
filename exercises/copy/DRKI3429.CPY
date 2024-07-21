      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3429.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3429.CPv  $
      *
      *   Rev 2.5   May 10 2019 12:55:06   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.4   Nov 22 2018 10:00:22   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.3   Nov 05 2018 15:47:34   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.2   Oct 27 2018 15:15:42   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.1   Oct 25 2018 14:36:44   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.0   Sep 7 2018 11:04:32 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3429                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3429                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.04.2019 / 10:59:41 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI3429
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3429.
           05  PI-DRKI3429-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3429-G42DVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3429.
           05  PO-DRKI3429-RC                     PIC  X(003).
               88 PO-DRKI3429-RC-OK               VALUE 'OK '.
               88 PO-DRKI3429-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

