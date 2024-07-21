      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2854.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2854.CPv  $
      *
      *   Rev 2.2   Apr 23 2014 07:28:06   t13ex39
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *    VERTRAG
      *
      *   Rev 2.1   Apr 14 2014 10:33:50   t13ex39
      *FKI01461:  ungültigen VBs für DeuBa-Verträge
      *
      *   Rev 2.0   Apr 03 2014 15:44:12   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2854  FUNKTIONEN LIST-OZP
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2854                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI01461!07.04.14!    ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2854
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  FETCH  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2854.
           05 PI-DRKI2854-PROGRAMM                PIC X(008).
           05 PI-DRKI2854-UFKT-CODE               PIC 9(002).
           05 PI-DRKI2854-DATEN.
              10 PI-DRKI2854-VBUVERTRAGSNR        PIC X(015).
              10 PI-DRKI2854-VBUSTATUS            PIC  X(0001).
      *
       01  PO-DRKI2854.
           05  PO-DRKI2854-RC                     PIC  X(003).
               88 PO-DRKI2854-RC-OK               VALUE 'OK '.
               88 PO-DRKI2854-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI2854-RC-EOF              VALUE 'EOF'.
               88 PO-DRKI2854-RC-LER              VALUE 'LER'.
      *
           05  PO-DRKI2854-DATEN.
             10 PO-DRKI2854-VBULFDNR              PIC S9(4) COMP.
             10 PO-DRKI2854-VBUVERTRAGSNR         PIC  X(0015).
             10 PO-DRKI2854-VBUABLEHNKZ           PIC  X(0001).
             10 PO-DRKI2854-VBUAKTDAT             PIC  X(010).
             10 PO-DRKI2854-VBUAUSTRITTSDAT       PIC  X(010).
             10 PO-DRKI2854-VBUAUSWABLEHNKZ       PIC  X(0001).
             10 PO-DRKI2854-VBUAUSWAUSTRTTDAT     PIC  X(010).
             10 PO-DRKI2854-VBUAUSWDAT            PIC  X(010).
             10 PO-DRKI2854-VBUAUSWSTRUKTSTUFE    PIC S9(9) COMP.
             10 PO-DRKI2854-VBUMANDANTNR          PIC S9(9) COMP.
             10 PO-DRKI2854-VBUPARTGESID          PIC S9(4) COMP.
             10 PO-DRKI2854-VBUSTATUS             PIC  X(0001).
             10 PO-DRKI2854-VBUSTRUKTURSTUFE      PIC S9(9) COMP.
             10 PO-DRKI2854-VBUVBNRGELIEF         PIC S9(9) COMP.
             10 PO-DRKI2854-VBUVRBVBNR            PIC S9(9) COMP.
      *
      ******* ENDE COPY-MEMBER ***************************************
