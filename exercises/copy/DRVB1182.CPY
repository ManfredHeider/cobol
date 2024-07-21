      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  18.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1182.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1182.CPv  $
      *
      *   Rev 2.1   Dec 19 2017 16:15:10   t13ae30
      *AAS01199: Siegeradler Wettbewerb 2017
      *
      *   Rev 2.0   Dec 18 2017 18:14:06   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1182                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Th. Willig                                       *
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
      *    FÜR DIE FUNKTION DRVB1182                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1182.
          05 PI-DRVB1182-PROGRAMM           PIC  X(008).
          05 PI-DRVB1182-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1182-MS-NR              PIC  9(004).
          05 PI-DRVB1182-DAT-VON            PIC  X(010).
          05 PI-DRVB1182-DAT-BIS            PIC  X(010).
          05 PI-DRVB1182-ED-STUFE-AB        PIC  9(004) COMP.
          05 PI-DRVB1182-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1182.
          05 PO-DRVB1182-RC                 PIC  X(003).
             88 PO-DRVB1182-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1182-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1182-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1182-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1182-RC-ERR                       VALUE 'ERR'.
          05 PO-DRVB1182-DATEN.
             10 PO-DRVB1182-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1182-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1182-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************

