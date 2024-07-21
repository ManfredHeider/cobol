      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3281.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3281.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:27:02   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 23 2018 15:11:07 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3281                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3281                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.04.2018 / 10:28:59 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3281
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3281.
           05  PI-DRKI3281-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3281-GENKEZPNR   PIC S9(4) COMP.
             10 PI-DRKI3281-GENKGESELLSCHID   PIC  X(0002).
             10 PI-DRKI3281-GENKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3281-GENKVNR   PIC  X(0015).
END   *

           05  PI-DRKI3281-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3281-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3281-GENKANREDE   PIC  X(0002).
             10 PI-DRKI3281-GENKAUSL-KZN   PIC  X(0003).
             10 PI-DRKI3281-GENKGEBDAT   PIC  X(0008).
             10 PI-DRKI3281-GENKGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3281-GENKNACHNAME   PIC  X(0040).
             10 PI-DRKI3281-GENKNACHNAME2   PIC  X(0040).
             10 PI-DRKI3281-GENKORT   PIC  X(0030).
             10 PI-DRKI3281-GENKPLZ   PIC  X(0015).
             10 PI-DRKI3281-GENKROLLE   PIC  X(0002).
             10 PI-DRKI3281-GENKSTRASSE   PIC  X(0045).
             10 PI-DRKI3281-GENKVERARB-KZN   PIC  X(0001).
             10 PI-DRKI3281-GENKVORNAME   PIC  X(0030).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3281-NI1.
           05 PI-DRKI3281-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3281-NI7.
           05 PI-DRKI3281-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3281.
           05  PO-DRKI3281-RC                     PIC  X(003).
               88 PO-DRKI3281-RC-OK               VALUE 'OK '.
               88 PO-DRKI3281-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

