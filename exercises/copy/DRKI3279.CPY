      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3279.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3279.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:25:38   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 23 2018 15:11:03 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3279                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3279                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.04.2018 / 10:28:55 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3279
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3279.
           05  PI-DRKI3279-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3279-GENKEZPNR   PIC S9(4) COMP.
             10 PI-DRKI3279-GENKGESELLSCHID   PIC  X(0002).
             10 PI-DRKI3279-GENKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3279-GENKVNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3279.
           05  PO-DRKI3279-RC                     PIC  X(003).
               88 PO-DRKI3279-RC-OK               VALUE 'OK '.
               88 PO-DRKI3279-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3279-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3279-GENKANREDE   PIC  X(0002).
             10 PO-DRKI3279-GENKAUSL-KZN   PIC  X(0003).
             10 PO-DRKI3279-GENKGEBDAT   PIC  X(0008).
             10 PO-DRKI3279-GENKGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3279-GENKNACHNAME   PIC  X(0040).
             10 PO-DRKI3279-GENKNACHNAME2   PIC  X(0040).
             10 PO-DRKI3279-GENKORT   PIC  X(0030).
             10 PO-DRKI3279-GENKPLZ   PIC  X(0015).
             10 PO-DRKI3279-GENKROLLE   PIC  X(0002).
             10 PO-DRKI3279-GENKSTRASSE   PIC  X(0045).
             10 PO-DRKI3279-GENKVERARB-KZN   PIC  X(0001).
             10 PO-DRKI3279-GENKVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

