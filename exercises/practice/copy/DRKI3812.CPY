      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3812.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3812.CPv  $
      *
      *   Rev 2.4   Oct 16 2020 10:41:30   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.3   Oct 16 2020 10:34:58   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.2   Oct 16 2020 09:45:18   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.0   Aug 13 2020 13:19:16 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3812                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3812                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.08.2020 / 13:19:16 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3812
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3812.
           05  PI-DRKI3812-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3812-G48DSUNTERNR   PIC  X(0002).
             10 PI-DRKI3812-G48DSVNR   PIC  X(0015).
             10 PI-DRKI3812-G48DSWAEHRUNGSKZ   PIC  X(0003).
END   *

      *
       01  PO-DRKI3812.
           05  PO-DRKI3812-RC                     PIC  X(003).
               88 PO-DRKI3812-RC-OK               VALUE 'OK '.
               88 PO-DRKI3812-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3812-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3812-G48DSANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI3812-G48DSBIC   PIC  X(0011).
             10 PO-DRKI3812-G48DSDELKZ   PIC  X(0001).
             10 PO-DRKI3812-G48DSDEPOTART   PIC  X(0001).
             10 PO-DRKI3812-G48DSIBAN   PIC  X(0034).
             10 PO-DRKI3812-G48DSKONTOINH   PIC  X(0028).
             10 PO-DRKI3812-G48DSMANDREF   PIC  X(0035).
             10 PO-DRKI3812-G48DSNRGWBARPRO   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3812-G48DSNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3812-G48DSNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3812-G48DSNRGWOHNPRO   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3812-G48DSOFFNUNGSDATUM   PIC  X(0008).
             10 PO-DRKI3812-G48DSRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3812-G48DSRISIKOKLASSE   PIC  X(0001).
             10 PO-DRKI3812-G48DSSCHLIESSDATUM   PIC  X(0008).
             10 PO-DRKI3812-G48DSSONDERKOND   PIC  X(0001).
             10 PO-DRKI3812-G48DSWPHGDAT   PIC  X(0008).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************



