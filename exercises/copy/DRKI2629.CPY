      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2629.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2629.CPv  $
      *
      *   Rev 2.2   Sep 03 2012 10:15:52   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.1   Aug 15 2012 11:54:44   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.0   Jun 26 2012 11:54:52 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2629                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2629                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.07.2012 / 08:38:07 / T13EX14
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
      *    FÜR DIE FUNKTION DRKI2629
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2629.
           05  PI-DRKI2629-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2629-VBULFDNR   PIC S9(4) COMP.
             10 PI-DRKI2629-VBUVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI2629.
           05  PO-DRKI2629-RC                     PIC  X(003).
               88 PO-DRKI2629-RC-OK               VALUE 'OK '.
               88 PO-DRKI2629-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2629-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2629-VBUABLEHNKZ   PIC  X(0001).
             10 PO-DRKI2629-VBUAKTDAT   PIC  X(010).
             10 PO-DRKI2629-VBUAUSTRITTSDAT   PIC  X(010).
             10 PO-DRKI2629-VBUAUSWABLEHNKZ   PIC  X(0001).
             10 PO-DRKI2629-VBUAUSWAUSTRTTDAT   PIC  X(010).
             10 PO-DRKI2629-VBUAUSWDAT   PIC  X(010).
             10 PO-DRKI2629-VBUAUSWSTRUKTSTUFE   PIC S9(9) COMP.
             10 PO-DRKI2629-N-VBSWSTRKTSTF   PIC S9(4) COMP.
             10 PO-DRKI2629-VBUMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2629-NI-VBUMANDANTNR   PIC S9(4) COMP.
             10 PO-DRKI2629-VBUPARTGESID   PIC S9(4) COMP.
             10 PO-DRKI2629-NI-VBUPARTGESID   PIC S9(4) COMP.
             10 PO-DRKI2629-VBUSTATUS   PIC  X(0001).
             10 PO-DRKI2629-VBUSTRUKTURSTUFE   PIC S9(9) COMP.
             10 PO-DRKI2629-N-VBSTRKTRSTF   PIC S9(4) COMP.
             10 PO-DRKI2629-VBUVBNRGELIEF   PIC S9(9) COMP.
             10 PO-DRKI2629-NI-VBUVBNRGELIEF   PIC S9(4) COMP.
             10 PO-DRKI2629-VBUVRBVBNR   PIC S9(9) COMP.
             10 PO-DRKI2629-NI-VBUVRBVBNR   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
