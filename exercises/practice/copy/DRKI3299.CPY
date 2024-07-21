      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3299.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3299.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:34:06   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 17:32:15 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3299                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3299                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2018 / 17:32:15 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3299
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3299.
           05  PI-DRKI3299-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3299-GUPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3299-GUPPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3299-GUPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3299.
           05  PO-DRKI3299-RC                     PIC  X(003).
               88 PO-DRKI3299-RC-OK               VALUE 'OK '.
               88 PO-DRKI3299-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3299-NI.
               10 NI-DRKI3299-TAB.
                  15 DRKI3299-ELE
                     PIC S9(04) COMP OCCURS 3 TIMES.
               10 NI-DRKI3299 REDEFINES NI-DRKI3299-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3299-NI-GUPGEBDATUM   PIC S9(4) COMP.
             15 PO-DRKI3299-NI-GUPNACHNAME   PIC S9(4) COMP.
             15 PO-DRKI3299-NI-GUPVORNAME   PIC S9(4) COMP.
END   *

           05  PO-DRKI3299-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3299-GUPGEBDATUM   PIC  X(010).
             10 PO-DRKI3299-GUPNACHNAME   PIC  X(0040).
             10 PO-DRKI3299-GUPVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

