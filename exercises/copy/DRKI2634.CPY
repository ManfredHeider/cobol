      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2634.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2634.CPv  $
      *
      *   Rev 2.1   Aug 15 2012 13:56:16   t13ex14
      *AKI01155: VerwFond berein. 1: Auswertung VBUNGUELTIGAV
      *
      *   Rev 2.0   Jun 29 2012 11:07:28   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2634  FUNKTIONEN LIST-OZP
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2634                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01155!02.07.12!    ! NEU                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2634
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  FETCH  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2634.
           05 PI-DRKI2634-PROGRAMM                PIC X(008).
           05 PI-DRKI2634-UFKT-CODE               PIC 9(002).
           05 PI-DRKI2634-DATEN.
              10 PI-DRKI2634-VBUMANDANTNR         PIC S9(9) COMP.
              10 PI-DRKI2634-VBUSTATUS            PIC  X(0001).
      *
       01  PO-DRKI2634.
           05  PO-DRKI2634-RC                     PIC  X(003).
               88 PO-DRKI2634-RC-OK               VALUE 'OK '.
               88 PO-DRKI2634-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI2634-RC-EOF              VALUE 'EOF'.
               88 PO-DRKI2634-RC-LER              VALUE 'LER'.
      *
           05  PO-DRKI2634-DATEN.
             10 PO-DRKI2634-VBULFDNR              PIC S9(4) COMP.
             10 PO-DRKI2634-VBUVERTRAGSNR         PIC  X(0015).
             10 PO-DRKI2634-VBUABLEHNKZ           PIC  X(0001).
             10 PO-DRKI2634-VBUAKTDAT             PIC  X(010).
             10 PO-DRKI2634-VBUAUSTRITTSDAT       PIC  X(010).
             10 PO-DRKI2634-VBUAUSWABLEHNKZ       PIC  X(0001).
             10 PO-DRKI2634-VBUAUSWAUSTRTTDAT     PIC  X(010).
             10 PO-DRKI2634-VBUAUSWDAT            PIC  X(010).
             10 PO-DRKI2634-VBUAUSWSTRUKTSTUFE    PIC S9(9) COMP.
             10 PO-DRKI2634-VBUMANDANTNR          PIC S9(9) COMP.
             10 PO-DRKI2634-VBUPARTGESID          PIC S9(4) COMP.
             10 PO-DRKI2634-VBUSTATUS             PIC  X(0001).
             10 PO-DRKI2634-VBUSTRUKTURSTUFE      PIC S9(9) COMP.
             10 PO-DRKI2634-VBUVBNRGELIEF         PIC S9(9) COMP.
             10 PO-DRKI2634-VBUVRBVBNR            PIC S9(9) COMP.
      *
      ******* ENDE COPY-MEMBER ***************************************
