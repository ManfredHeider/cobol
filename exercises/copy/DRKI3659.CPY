      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3659.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI3659.CPv  $
      *
      *   Rev 2.7   Jul 23 2019 13:18:00   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Nac
      *harbeiten
      *
      *   Rev 2.6   Jul 22 2019 11:40:10   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Nac
      *harbeiten für August
      *
      *   Rev 2.5   Jul 22 2019 10:34:16   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Nac
      *harbeiten für August
      *
      *   Rev 2.4   Jul 19 2019 12:25:48   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Nac
      *harbeiten für August
      *
      *   Rev 2.3   Jun 14 2019 09:36:00   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.2   Jun 12 2019 13:12:22   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.1   Jun 11 2019 09:54:08   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.0   May 09 2019 15:11:26   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3659
      *    --------------------------------------------------------
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3659
      *    --------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01686!09.05.19!2.1 ! NEUERSTELLUNG             ! DETMAR
      * -------------------------------------------------------------
      * AKI01686!22.07.19!2.5 ! Anpassung Schnittstelle   ! Detmar
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3659
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: Ermitteln Zahlbeitrag AMLeben Verträge inklusive
      *              Tarifbeitrag
      *
      *****************************************************************
      *
       01  PI-DRKI3659.
           05 PI-DRKI3659-VERTRAGSID              PIC  X(013).
      *
       01  PO-DRKI3659.
           05 PO-DRKI3659-RC                      PIC  X(003).
              88 PO-DRKI3659-RC-OK                       VALUE 'OK '.
      *
           05 PO-DRKI3659-DATEN.
              10 PO-DRKI3659-TARIFBEZEICHNUNG     PIC  X(001).
                 88 PO-DRKI3659-N-GEWERT          VALUE '0'.
                 88 PO-DRKI3659-VA                VALUE '1'.
                 88 PO-DRKI3659-SP                VALUE '2'.
                 88 PO-DRKI3659-RIESTER           VALUE '3'.
                 88 PO-DRKI3659-SK                VALUE '4'.
              10 PO-DRKI3659-ZAHLBEITRAG          PIC 9(11)v99
                                                  PACKED-DECIMAL.
              10 PO-DRKI3659-TARIFBEITRAG         PIC 9(11)v99
                                                  PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER ****************************************
 








