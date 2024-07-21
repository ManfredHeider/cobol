      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3663.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3663.CPv  $
      *
      *   Rev 2.2   Jun 17 2019 12:48:00   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.1   Jun 07 2019 12:36:38   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 20 2019 08:30:32   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3663                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das Join-OZP ZJKI0004                    *
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
      * AKI01686!20.05.19!2.1 ! NEUERSTELLUNG             ! Detmar   *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3663                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3663.
          05 PI-DRKI3663-PROGRAMM              PIC X(008).
          05  PI-DRKI3663-DATEN.
              10  PI-DRKI3663-VERTRAGSNUMMER  PIC  X(15).
              10  PI-DRKI3663-GES             PIC  X(02).
      *
       01  PO-DRKI3663.
           05  PO-DRKI3663-RC                  PIC  X(003).
               88 PO-DRKI3663-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3663-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI3663-RC-EOF                       VALUE 'EOF'.

           05  PO-DRKI3663-PERSONID-CNT        PIC  9(02) COMP.
           05  PO-DRKI3663-DATEN.
               10 PO-DRKI3663-PERSONID         PIC  X(13).
               10 PO-DRKI3663-VERTRAGSID       PIC  X(13).
               10 PO-DRKI3663-MANDANT          PIC S9(9).
               10 PO-DRKI3663-ZAHLWEISE        PIC S9(4).
               10 PO-DRKI3663-VRT-BEG-DAT      PIC X(10).
      *
      ******* ENDE COPY-MEMBER **************************************


