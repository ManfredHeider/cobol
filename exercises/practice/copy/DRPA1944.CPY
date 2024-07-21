      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1944.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1944.CPv  $
      *
      *   Rev 2.1   Apr 16 2020 15:26:40   t13ae37
      *APA02915: VDL: Neue Produktgebergesellschaft JURADIREKT - AB
      *U - PEG/PVW
      *
      *   Rev 2.0   Feb 26 2020 11:18:56   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1944 FUNKTION UWPA0051                   *
      *                 JURADIREKT - PM FORMAL PRUEFEN/NORMIEREN    *
      *    -------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02915!26.02.20!2.1 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1944: PEG JURADIREKT                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1944.
             10 PI-DRPA1944-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1944-PEG-JURADIREKT.
                COPY DRPA1945.
             10 PI-DRPA1944-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1944.
          05 PO-DRPA1944-RC                 PIC  X(003).
             88 PO-DRPA1944-RC-OK                VALUE 'OK '.
             88 PO-DRPA1944-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
