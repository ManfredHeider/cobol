      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3665.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3665.CPv  $
      *
      *   Rev 2.3   Jul 01 2019 12:12:12   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.2   Jun 17 2019 12:50:02   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.1   Jun 07 2019 12:45:00   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 20 2019 14:23:16   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3665                                      *
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
      *    FÜR DIE FUNKTION DRKI3665                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3665.
          05 PI-DRKI3665-PROGRAMM              PIC X(008).
          05 PI-DRKI3665-UFKT-CODE             PIC 9(002).
          05  PI-DRKI3665-DATEN.
              10  PI-DRKI3665-PERSID           PIC  X(13).
      *
       01  PO-DRKI3665.
           05  PO-DRKI3665-RC                  PIC  X(003).
               88 PO-DRKI3665-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3665-RC-NOT-FOUND                 VALUE 'NFD'.

           05  PO-DRKI3665-PERSONID-CNT        PIC  9(02) COMP.
           05  PO-DRKI3665-DATEN.
               10 PO-DRKI3665-VERVERTRAGSID    PIC  X(13).
               10 PO-DRKI3665-ZAHLWEISE        PIC S9(4).
               10 PO-DRKI3665-GES              PIC s9(4).
               10 PO-DRKI3665-GES-BETR         PIC S9(9)v99
                                                         PACKED-DECIMAL.
               10 PO-DRKI3665-VERTR-BEG        PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************


