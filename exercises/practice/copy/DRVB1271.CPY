      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1271.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1271.CPv  $
      *
      *   Rev 2.1   Jan 20 2020 13:13:26   t13ae30
      *AAS01307: 3. Anpassung Bedingungen AS-Club / AS-Club mbL / K
      *ap.-Club / Adm.-Club - DVAG, Wien
      *
      *   Rev 2.0   Nov 19 2019 13:00:54   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1271                                     *
      *    FUNKTION AS-CLUB KRITERIEN PRÜFEN - DVAG, WIEN            *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01307!11.09.19! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1271                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1271.
          05 PI-DRVB1271-PROGRAMM           PIC  X(008).
          05 PI-DRVB1271-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1271-MA-ST-NR           PIC  9(007).
          05 PI-DRVB1271-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVB1271.
          05 PO-DRVB1271-RC                 PIC  X(003).
             88 PO-DRVB1271-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1271-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1271-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1271-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1271-RC-ERR                       VALUE 'ERR'.

      *
      ******** ENDE COPY-MEMBER DRVB1271 *****************************

