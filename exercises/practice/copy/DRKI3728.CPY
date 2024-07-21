      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3728.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3728.CPv  $
      *
      *   Rev 2.5   Apr 08 2020 14:41:22   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.4   Mar 17 2020 09:52:34   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.3   Feb 19 2020 10:52:38   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.2   Feb 19 2020 10:14:26   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.1   Feb 18 2020 15:25:32   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.0   Oct 22 2019 09:58:23 I13AZ84
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3728                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3728                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.10.2019 / 09:58:23 / I13AZ84
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01726!22.10.19!1.0 !NEUERSTELLUNG              !M.ATAR    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3728
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3728.
           05  PI-DRKI3728-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3728-G48FSAVNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3728.
           05  PO-DRKI3728-RC                     PIC  X(003).
               88 PO-DRKI3728-RC-OK               VALUE 'OK '.
               88 PO-DRKI3728-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER ***************************************




