      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3280.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3280.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:26:20   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 23 2018 15:11:05 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3280                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3280                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.04.2018 / 10:28:57 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3280
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3280.
           05  PI-DRKI3280-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3280-GENKEZPNR   PIC S9(4) COMP.
             10 PI-DRKI3280-GENKGESELLSCHID   PIC  X(0002).
             10 PI-DRKI3280-GENKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3280-GENKVNR   PIC  X(0015).
END   *

           05  PI-DRKI3280-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3280-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3280-GENKANREDE   PIC  X(0002).
             10 PI-DRKI3280-GENKAUSL-KZN   PIC  X(0003).
             10 PI-DRKI3280-GENKGEBDAT   PIC  X(0008).
             10 PI-DRKI3280-GENKGESCHLECHT   PIC  X(0001).
             10 PI-DRKI3280-GENKNACHNAME   PIC  X(0040).
             10 PI-DRKI3280-GENKNACHNAME2   PIC  X(0040).
             10 PI-DRKI3280-GENKORT   PIC  X(0030).
             10 PI-DRKI3280-GENKPLZ   PIC  X(0015).
             10 PI-DRKI3280-GENKROLLE   PIC  X(0002).
             10 PI-DRKI3280-GENKSTRASSE   PIC  X(0045).
             10 PI-DRKI3280-GENKVERARB-KZN   PIC  X(0001).
             10 PI-DRKI3280-GENKVORNAME   PIC  X(0030).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3280-NI1.
           05 PI-DRKI3280-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3280-NI7.
           05 PI-DRKI3280-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3280.
           05  PO-DRKI3280-RC                     PIC  X(003).
               88 PO-DRKI3280-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3280-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

