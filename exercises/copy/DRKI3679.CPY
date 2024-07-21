      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3679.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3679.CPv  $
      *
      *   Rev 2.4   Jun 21 2019 11:44:56   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.3   Jun 12 2019 13:33:06   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.2   Jun 11 2019 13:08:54   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.1   Jun 11 2019 10:06:06   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.0   May 29 2019 12:46:34   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3679                                      *
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
      *    FÜR DIE FUNKTION DRKI3679                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI3679.
          05 PI-DRKI3679-PROGRAMM               PIC X(008).
          05 PI-DRKI3679-UFKT-CODE              PIC 9(002).
          05 PI-DRKI3679-DATEN.
             10 PI-DRKI3679-MANDANT             PIC S9(009).
      *
       01 PO-DRKI3679.
 
          05 PO-DRKI3679-RC                     PIC  X(003).
             88 PO-DRKI3679-RC-OK                    VALUE 'OK '.
             88 PO-DRKI3679-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI3679-RC-EOF                   VALUE 'EOF'.
      *
      *
      ***** Ende Copy-Member DRKI3679 *****************************



