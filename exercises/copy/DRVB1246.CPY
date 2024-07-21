      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1246.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1246.CPv  $
      *
      *   Rev 2.2   Dec 17 2019 10:47:18   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club / AS-Club mbL / Kap.
      *-Club / Adm.-Club - DVAG
      *
      *   Rev 2.1   Oct 30 2019 12:46:48   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 02 2019 12:02:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1246                                     *
      *    FUNKTION AS-CLUB KANDIDATEN ERMITTELN - DVAG, WIEN        *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    11.09.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VBL-964 !11.09.19! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1246                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1246.
          05 PI-DRVB1246-PROGRAMM           PIC  X(008).
          05 PI-DRVB1246-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB1246.
          05 PO-DRVB1246-RC                 PIC  X(003).
             88 PO-DRVB1246-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1246-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1246-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1246-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1246-DATEN.
             10 PO-DRVB1246-MA-ST-NR          PIC  9(009)  COMP.

      *
      ******** ENDE COPY-MEMBER DRVB1246 *****************************


