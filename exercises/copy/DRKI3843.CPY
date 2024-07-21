      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3843.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3843.CPv  $
      *
      *   Rev 2.1   Nov 23 2020 17:16:56   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *B E S T A N D
      *
      *   Rev 2.0   Sep 2 2020 14:48:05 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3843                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3843                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         02.09.2020 / 14:48:05 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3843
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3843.
           05  PI-DRKI3843-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3843-G48DBUNTERNR   PIC  X(0002).
             10 PI-DRKI3843-G48DBVNR   PIC  X(0015).
             10 PI-DRKI3843-G48DBWAEHRUNGSKZ   PIC  X(0003).
             10 PI-DRKI3843-G48DBWPKENNNR   PIC  X(0006).
END   *

      *
       01  PO-DRKI3843.
           05  PO-DRKI3843-RC                     PIC  X(003).
               88 PO-DRKI3843-RC-OK               VALUE 'OK '.
               88 PO-DRKI3843-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

