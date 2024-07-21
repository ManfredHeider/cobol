      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1058.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1058.CPv  $
      *
      *   Rev 2.3   Mar 16 2016 13:26:42   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.2   Mar 16 2016 12:49:16   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.1   May 23 2012 13:03:08   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.0   May 09 2012 18:51:04   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1058                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    09.05.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00992!09.05.12! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1058                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1058.
          05 PI-DRVB1058-PROGRAMM           PIC  X(008).
          05 PI-DRVB1058-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1058-MS-NR              PIC  9(004).
          05 PI-DRVB1058-DAT-VON            PIC  X(010).
          05 PI-DRVB1058-DAT-BIS            PIC  X(010).
          05 PI-DRVB1058-ED-GRENZE          PIC  9(004) COMP.
          05 PI-DRVB1058-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1058.
          05 PO-DRVB1058-RC                 PIC  X(003).
             88 PO-DRVB1058-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1058-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1058-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1058-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1058-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1058-DATEN.
             10 PO-DRVB1058-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1058-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1058-EH-GS-XX-GR-NEU PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************


