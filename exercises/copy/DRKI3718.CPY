      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3718.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3718.CPv  $
      *
      *   Rev 2.2   Jul 30 2019 14:51:34   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple)
      *
      *   Rev 2.1   Jul 30 2019 13:56:02   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple)
      *
      *   Rev 2.0   Jul 30 2019 12:01:30   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3718                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das List-OZP ZXKI0330                    *
      *    Lebenteilschicht                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DETMAR                                           *
      *    DATUM:    30.07.2019                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01686!30.07.19!2.1 ! NEUERSTELLUNG             ! Detmar   *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3718                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI3718.
          05 PI-DRKI3718-PROGRAMM               PIC X(008).
          05 PI-DRKI3718-UFKT-CODE              PIC 9(002).
          05 PI-DRKI3718-DATEN.
             10 PI-DRKI3718-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI3718.
 
          05 PO-DRKI3718-RC                     PIC  X(003).
             88 PO-DRKI3718-RC-OK                    VALUE 'OK '.
             88 PO-DRKI3718-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI3718-RC-EOF                   VALUE 'EOF'.
      *
      *
      ***** Ende Copy-Member DRKI3718 *****************************

