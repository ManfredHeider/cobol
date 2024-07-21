      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3685.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3685.CPv  $
      *
      *   Rev 2.3   Jul 01 2019 18:52:40   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.2   Jun 21 2019 13:24:14   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.1   Jun 21 2019 12:01:14   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.0   Jun 19 2019 10:11:44   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3685                                      *
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
      *    FÜR DIE FUNKTION DRKI3685                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3685.
           05  PI-DRKI3685-DATEN.
               10  PI-DRKI3685-PERSONID        PIC  X(013).
      *
       01  PO-DRKI3685.
           05  PO-DRKI3685-RC                  PIC  X(003).
               88 PO-DRKI3685-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3685-RC-NOT-FOUND                 VALUE 'NFD'.
 
           05  PO-DRKI3685-DATEN.
               10 PO-DRKI3685-ANZAHL           PIC S9(009).
               10 PO-DRKI3685-VP.
                  15 PO-DRKI3685-AML-VP OCCURS 500 TIMES INDEXED BY
                                                         DRKI3685-IND-1.
                     20 PO-DRKI3685-VERTRAGSID    PIC  X(013).
                     20 PO-DRKI3685-ZAHLWEISE     PIC S9(004).
                     20 PO-DRKI3685-VERTRAGSBEG   PIC  X(010).
                     20 PO-DRKI3685-GES-BETR      PIC S9(009)v99
                                                         PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


