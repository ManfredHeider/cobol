      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3811.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3811.CPv  $
      *
      *   Rev 2.4   Oct 16 2020 10:41:22   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.3   Oct 16 2020 10:34:50   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.2   Oct 16 2020 09:45:00   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.0   Aug 13 2020 13:19:14 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3811                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3811                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.08.2020 / 13:19:14 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3811
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3811.
           05  PI-DRKI3811-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3811-G48DSUNTERNR   PIC  X(0002).
             10 PI-DRKI3811-G48DSVNR   PIC  X(0015).
             10 PI-DRKI3811-G48DSWAEHRUNGSKZ   PIC  X(0003).
END   *

      *
       01  PO-DRKI3811.
           05  PO-DRKI3811-RC                     PIC  X(003).
               88 PO-DRKI3811-RC-OK               VALUE 'OK '.
               88 PO-DRKI3811-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************



