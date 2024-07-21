      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3810.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3810.CPv  $
      *
      *   Rev 2.4   Oct 16 2020 10:41:14   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.3   Oct 16 2020 10:34:40   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.2   Oct 16 2020 09:44:50   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.0   Aug 13 2020 13:19:12 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3810                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3810                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.08.2020 / 13:19:12 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3810
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3810.
           05  PI-DRKI3810-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3810-G48DSUNTERNR   PIC  X(0002).
             10 PI-DRKI3810-G48DSVNR   PIC  X(0015).
             10 PI-DRKI3810-G48DSWAEHRUNGSKZ   PIC  X(0003).
END   *

           05  PI-DRKI3810-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3810-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3810-G48DSANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI3810-G48DSBIC   PIC  X(0011).
             10 PI-DRKI3810-G48DSDELKZ   PIC  X(0001).
             10 PI-DRKI3810-G48DSDEPOTART   PIC  X(0001).
             10 PI-DRKI3810-G48DSIBAN   PIC  X(0034).
             10 PI-DRKI3810-G48DSKONTOINH   PIC  X(0028).
             10 PI-DRKI3810-G48DSMANDREF   PIC  X(0035).
             10 PI-DRKI3810-G48DSNRGWBARPRO   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3810-G48DSNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3810-G48DSNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI3810-G48DSNRGWOHNPRO   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI3810-G48DSOFFNUNGSDATUM   PIC  X(0008).
             10 PI-DRKI3810-G48DSRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI3810-G48DSRISIKOKLASSE   PIC  X(0001).
             10 PI-DRKI3810-G48DSSCHLIESSDATUM   PIC  X(0008).
             10 PI-DRKI3810-G48DSSONDERKOND   PIC  X(0001).
             10 PI-DRKI3810-G48DSWPHGDAT   PIC  X(0008).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3810-NI1.
           05 PI-DRKI3810-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3810-NI7.
           05 PI-DRKI3810-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3810.
           05  PO-DRKI3810-RC                     PIC  X(003).
               88 PO-DRKI3810-RC-OK               VALUE 'OK '.
               88 PO-DRKI3810-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************



