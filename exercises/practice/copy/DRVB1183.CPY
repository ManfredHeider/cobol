      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  18.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1183.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1183.CPv  $
      *
      *   Rev 2.1   Dec 19 2017 16:15:22   t13ae30
      *AAS01199: Siegeradler Wettbewerb 2017
      *
      *   Rev 2.0   Dec 18 2017 18:14:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1183                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Th.Willig                                        *
      *    DATUM:    18.12.2017                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01199!18.12.17! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1183                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1183.
          05 PI-DRVB1183-PROGRAMM           PIC  X(008).
          05 PI-DRVB1183-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1183-DAT-VON            PIC  X(010).
          05 PI-DRVB1183-DAT-BIS            PIC  X(010).
          05 PI-DRVB1183-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1183.
          05 PO-DRVB1183-RC                 PIC  X(003).
             88 PO-DRVB1183-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1183-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1183-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1183-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1183-RC-ERR                       VALUE 'ERR'.
          05 PO-DRVB1183-DATEN.
             10 PO-DRVB1183-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1183-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1183-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************

