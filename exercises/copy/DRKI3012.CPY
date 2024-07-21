      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3012.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3012.CPv  $
      *
      *   Rev 2.4   Oct 21 2015 12:43:48   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.3   Sep 29 2015 12:26:46   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.2   Aug 14 2015 16:30:36   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.1   Jun 17 2015 10:02:14   t13ex27
      *FKI01501: EVM-KZ anpassen bei KdBv
      *
      *   Rev 2.0   Jun 15 2015 11:51:14 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3012                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3012                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.10.2015 / 12:05:09 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3012
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3012.
           05  PI-DRKI3012-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3012-BKPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3012-BKPVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI3012-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI3012-BKPEIGENVERMKZ   PIC  X(0001).
END   *


      *
       01  PO-DRKI3012.
           05  PO-DRKI3012-RC                     PIC  X(003).
               88 PO-DRKI3012-RC-OK               VALUE 'OK '.
               88 PO-DRKI3012-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

