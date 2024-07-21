      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3300.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3300.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:34:48   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 17:32:17 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3300                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3300                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2018 / 17:32:17 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3300
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3300.
           05  PI-DRKI3300-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3300-GUPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3300-GUPPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3300-GUPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3300-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3300-NI-GUPGEBDATUM      PIC S9(4) COMP.
             15 PI-DRKI3300-NI-GUPNACHNAME      PIC S9(4) COMP.
             15 PI-DRKI3300-NI-GUPVORNAME       PIC S9(4) COMP.
END   *

           05  PI-DRKI3300-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3300-GUPGEBDATUM   PIC  X(010).
             10 PI-DRKI3300-GUPNACHNAME   PIC  X(0040).
             10 PI-DRKI3300-GUPVORNAME   PIC  X(0030).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3300-NI1.
           05 PI-DRKI3300-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 3.
       01  PI-DRKI3300-NI7.
           05 PI-DRKI3300-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 3.

      *
       01  PO-DRKI3300.
           05  PO-DRKI3300-RC                     PIC  X(003).
               88 PO-DRKI3300-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3300-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

