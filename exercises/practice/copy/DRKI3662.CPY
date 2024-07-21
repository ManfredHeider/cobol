      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3662.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3662.CPv  $
      *
      *   Rev 2.3   Jun 28 2019 17:04:24   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.2   Jun 21 2019 11:25:48   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.1   Jun 07 2019 12:32:32   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 14 2019 15:22:34   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3662                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das Join-OZP ZJKI0039
      *    VERTRAGROLLEBEZ zu LEBENPERSONEIGEN
      *    --------------------------------------------------------   *
      *    ERSTELLT: DETMAR                                           *
      *    DATUM:    13.05.2019                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01686!20.05.19!2.1 ! NEUERSTELLUNG             ! Detmar   *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3662                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3662.
          05 PI-DRKI3662-PROGRAMM              PIC X(008).
          05 PI-DRKI3662-UFKT-CODE             PIC 9(002).
           05  PI-DRKI3662-DATEN.
               10  PI-DRKI3662-PERSID          PIC  X(13).
      *
       01  PO-DRKI3662.
           05  PO-DRKI3662-RC                  PIC  X(003).
               88 PO-DRKI3662-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3662-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI3662-RC-EOF                       VALUE 'EOF'.

           05  PO-DRKI3662-PERSONID-CNT        PIC  9(02) COMP.
           05  PO-DRKI3662-DATEN.
               10 PO-DRKI3662-NNAME            PIC  X(30).
               10 PO-DRKI3662-VNAME            PIC  X(30).
               10 PO-DRKI3662-GEBDAT           PIC  X(10).
               10 PO-DRKI3662-VERTR-BEG        PIC  X(10).
               10 PO-DRKI3662-ZAHLWEISE        PIC S9(4).
               10 PO-DRKI3662-VERTRAGSID       PIC  X(13).
               10 PO-DRKI3662-GES-BETR         PIC S9(9)v99
                                                         PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


