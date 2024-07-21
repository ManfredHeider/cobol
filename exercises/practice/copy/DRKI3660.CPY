      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3660.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3660.CPv  $
      *
      *   Rev 2.1   Jun 07 2019 12:24:06   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 13 2019 12:35:58   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3660                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das List-OZP ZXKI0330                    *
      *    Lebenteilschicht                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DETMAR                                           *
      *    DATUM:    13.05.2019                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01686!13.05.19!2.1 ! NEUERSTELLUNG             ! Detmar   *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3660                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI3660.
          05 PI-DRKI3660-PROGRAMM               PIC X(008).
          05 PI-DRKI3660-UFKT-CODE              PIC 9(002).
          05 PI-DRKI3660-DATEN.
             10 PI-DRKI3660-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI3660.
 
          05 PO-DRKI3660-RC                     PIC  X(003).
             88 PO-DRKI3660-RC-OK                    VALUE 'OK '.
             88 PO-DRKI3660-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI3660-RC-EOF                   VALUE 'EOF'.
 
          05 PO-DRKI3660-DATEN.
            10 PO-DRKI3660-LTSTTYPBEITR        PIC S9(11)V9(2) COMP-3.
      *
      *
      ***** Ende Copy-Member DRKI3660 *****************************
