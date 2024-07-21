      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3396.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3396.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:47:34   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Aug 30 2018 15:28:02 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3396                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3396                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.10.2018 / 12:58:57 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3396
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3396.
           05  PI-DRKI3396-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3396-G42ZKLFDNR   PIC S9(9) COMP.
             10 PI-DRKI3396-G42ZKVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3396-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3396-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3396-G42ZKANREDE   PIC  X(0001).
             10 PI-DRKI3396-G42ZKGEBDAT   PIC  X(0008).
             10 PI-DRKI3396-G42ZKGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3396-G42ZKLKZ   PIC  X(0003).
             10 PI-DRKI3396-G42ZKNACHNAME   PIC  X(0035).
             10 PI-DRKI3396-G42ZKORT   PIC  X(0040).
             10 PI-DRKI3396-G42ZKPLZ   PIC  X(0009).
             10 PI-DRKI3396-G42ZKSTRHNR   PIC  X(0040).
             10 PI-DRKI3396-G42ZKVORNAME   PIC  X(0035).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3396-NI1.
           05 PI-DRKI3396-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3396-NI7.
           05 PI-DRKI3396-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3396.
           05  PO-DRKI3396-RC                     PIC  X(003).
               88 PO-DRKI3396-RC-OK               VALUE 'OK '.
               88 PO-DRKI3396-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

