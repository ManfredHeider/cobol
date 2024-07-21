      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3044.CPv  $
      *
      *   Rev 2.1   Oct 21 2015 12:43:54   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.0   Oct 21 2015 12:05:11 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3044                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3044                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.10.2015 / 12:05:11 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3044
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3044.
           05  PI-DRKI3044-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3044-BKPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3044-BKPVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI3044-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI3044-BKPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI3044-BKPEIGENVERMKZ   PIC  X(0001).
END   *


      *
       01  PO-DRKI3044.
           05  PO-DRKI3044-RC                     PIC  X(003).
               88 PO-DRKI3044-RC-OK               VALUE 'OK '.
               88 PO-DRKI3044-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

