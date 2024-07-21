      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1276.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1276.CPv  $
      *
      *   Rev 2.1   Dec 17 2019 10:53:52   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club / AS-Club mbL / Kap.
      *-Club / Adm.-Club - DVAG
      *
      *   Rev 2.0   Nov 28 2019 13:36:18   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1276                                     *
      *    FUNKTION ERM. ALLE P6 WG.VIRTUELLER PRÜFUNG, ALLFINANZ1   *
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
      * AAS01307!11.09.19! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1276 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1276.
          05 PI-DRVB1276-PROGRAMM             PIC  X(008).
          05 PI-DRVB1276-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB1276.
          05 PO-DRVB1276-RC                   PIC  X(003).
             88 PO-DRVB1276-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1276-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1276-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1276-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1276-DATEN.
             10 PO-DRVB1276-MA-ST-NR          PIC S9(009)  COMP.
      *
      ******** ENDE COPY-MEMBER DRVB1276 *****************************

