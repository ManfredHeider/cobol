      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3301.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:35:32   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 17:32:19 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3301                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3301                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2018 / 17:32:19 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3301
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3301.
           05  PI-DRKI3301-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3301-GUPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3301-GUPPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3301-GUPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3301-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3301-NI-GUPGEBDATUM      PIC S9(4) COMP.
             15 PI-DRKI3301-NI-GUPNACHNAME      PIC S9(4) COMP.
             15 PI-DRKI3301-NI-GUPVORNAME       PIC S9(4) COMP.
END   *

           05  PI-DRKI3301-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3301-GUPGEBDATUM   PIC  X(010).
             10 PI-DRKI3301-GUPNACHNAME   PIC  X(0040).
             10 PI-DRKI3301-GUPVORNAME   PIC  X(0030).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3301-NI1.
           05 PI-DRKI3301-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 3.
       01  PI-DRKI3301-NI7.
           05 PI-DRKI3301-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 3.
      *
       01  PO-DRKI3301.
           05  PO-DRKI3301-RC                     PIC  X(003).
               88 PO-DRKI3301-RC-OK               VALUE 'OK '.
               88 PO-DRKI3301-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

