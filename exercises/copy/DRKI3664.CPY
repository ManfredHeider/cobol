      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3664.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3664.CPv  $
      *
      *   Rev 2.4   Jul 01 2019 16:18:54   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.3   Jun 28 2019 17:07:56   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.2   Jun 21 2019 11:30:08   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.1   Jun 07 2019 12:40:46   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 20 2019 12:16:42   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3664                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das Join-OZP ZJKI0039                    *
      *    VERTRAGROLLEBEZ zu KRANKENPERSEIGEN
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
      *    FÜR DIE FUNKTION DRKI3664                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3664.
          05 PI-DRKI3664-PROGRAMM              PIC X(008).
          05 PI-DRKI3664-UFKT-CODE             PIC 9(002).
           05  PI-DRKI3664-DATEN.
               10  PI-DRKI3664-PERSID          PIC  X(13).
      *
       01  PO-DRKI3664.
           05  PO-DRKI3664-RC                  PIC  X(003).
               88 PO-DRKI3664-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3664-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI3664-RC-EOF                       VALUE 'EOF'.

           05  PO-DRKI3664-PERSONID-CNT        PIC  9(02) COMP.
           05  PO-DRKI3664-DATEN.
               10 PO-DRKI3664-NNAME            PIC  X(30).
               10 PO-DRKI3664-VNAME            PIC  X(30).
               10 PO-DRKI3664-GEBDAT           PIC  X(10).
               10 PO-DRKI3664-VERTR-BEG        PIC  X(10).
               10 PO-DRKI3664-ZAHLWEISE        PIC S9(4).
               10 PO-DRKI3664-VERTRAGSID       PIC  X(13).
               10 PO-DRKI3664-GES-BETR         PIC S9(9)v99
                                                         PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************



