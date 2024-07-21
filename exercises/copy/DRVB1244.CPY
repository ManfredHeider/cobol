      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1244.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1244.CPv  $
      *
      *   Rev 2.1   Oct 30 2019 12:45:18   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.0   Oct 02 2019 12:02:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1244                                     *
      *    FUNKTION BES.LEIST AS-CLUB KANDIDATEN ERM. - ALLFINANZ1   *
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
      *    FÜR DIE FUNKTION DRVB1244                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1244.
          05 PI-DRVB1244-PROGRAMM           PIC  X(008).
          05 PI-DRVB1244-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB1244.
          05 PO-DRVB1244-RC                 PIC  X(003).
             88 PO-DRVB1244-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1244-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1244-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1244-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1244-DATEN.
             10 PO-DRVB1244-MA-ST-NR        PIC  9(007)  COMP-3.

      *
      ******** ENDE COPY-MEMBER DRVB1244 *****************************

