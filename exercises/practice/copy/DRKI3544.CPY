      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3544.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3544.CPv  $
      *
      *   Rev 2.1   Jan 30 2019 11:35:54   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.0   Dec 18 2018 06:25:52 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3544                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3544                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.12.2018 / 06:25:52 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3544
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3544.
           05  PI-DRKI3544-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3544-G48ZKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3544-G48ZKVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3544-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3544-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3544-G48ZKANREDE   PIC  X(0001).
             10 PI-DRKI3544-G48ZKDELKZ   PIC  X(0001).
             10 PI-DRKI3544-G48ZKDEPOTKZ   PIC  X(0001).
             10 PI-DRKI3544-G48ZKGEBDAT   PIC  X(0008).
             10 PI-DRKI3544-G48ZKGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3544-G48ZKHAUSNR   PIC  X(0009).
             10 PI-DRKI3544-G48ZKLKZ   PIC  X(0003).
             10 PI-DRKI3544-G48ZKNACHNAME   PIC  X(0035).
             10 PI-DRKI3544-G48ZKORT   PIC  X(0028).
             10 PI-DRKI3544-G48ZKPLZ   PIC  X(0009).
             10 PI-DRKI3544-G48ZKPOSTFACH   PIC  X(0009).
             10 PI-DRKI3544-G48ZKSTR   PIC  X(0028).
             10 PI-DRKI3544-G48ZKTELNR   PIC  X(0009).
             10 PI-DRKI3544-G48ZKTELVOR   PIC  X(0006).
             10 PI-DRKI3544-G48ZKVORNAME   PIC  X(0035).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3544-NI1.
           05 PI-DRKI3544-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3544-NI7.
           05 PI-DRKI3544-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3544.
           05  PO-DRKI3544-RC                     PIC  X(003).
               88 PO-DRKI3544-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3544-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

