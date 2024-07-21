      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3632.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3632.CPv  $
      *
      *   Rev 2.5   Jul 28 2020 14:18:36   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.4   Jul 20 2020 09:12:44   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.3   Apr 29 2019 14:59:34   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.2   Apr 29 2019 14:50:10   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.1   Apr 11 2019 13:25:04   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.0   Mar 15 2019 12:46:17 I13AZ81
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3632                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE F�R DIE FUNKTION DRKI3632                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.03.2019 / 12:46:17 / I13AZ81
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01671!19.03.19!2.0 ! NEU                       ! N.RUBEN  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3632
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3632.
           05  PI-DRKI3632-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3632-G48SPDVNR   PIC  X(0015).
             10 PI-DRKI3632-G48SPDUKONTONR   PIC  X(0002).
END   *

      *
       01  PO-DRKI3632.
           05  PO-DRKI3632-RC                     PIC  X(003).
               88 PO-DRKI3632-RC-OK               VALUE 'OK '.
               88 PO-DRKI3632-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************





