      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3661.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3661.CPv  $
      *
      *   Rev 2.1   Jun 07 2019 12:28:20   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 14 2019 15:09:52   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *  JOIN VERTRAGROLLEBEZ LEBENPERSONEIGEN
      *  JOIN VERTRAGROLLEBEZ KRANKENPERSONEIGEN
      *   zur Ermittlung von VPs, die nicht VN sind
      *
      ****************************************************************
      *    COPY-MEMBER: DRKI0419                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZJKI0039                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.05.2019 / 13:40:28 / T13AC23
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01686!20.02.19!2.1 ! Neuerstellung             ! Detmar   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZJKI0039.
           05  PI-ZJKI0039-FUNKTION              PIC  X(008).
      *
      *
             88   PI-ZJKI0039-DRKI3662     VALUE 'DRKI3662'.
             88   PI-ZJKI0039-DRKI3664     VALUE 'DRKI3664'.
      *
      ******* ENDE COPY-MEMBER **************************************
