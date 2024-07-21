      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0662.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0662.CPv  $
      *
      *   Rev 2.5   Mar 16 2016 13:26:30   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.4   Mar 16 2016 12:49:08   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.3   May 03 2012 16:48:30   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.2   Sep 24 2004 09:25:40   t13ae27
      *AAS00494: Darstellung der EQF-elemenierten Siegeradler-Kandi
      *daten
      *
      *   Rev 2.1   Sep 03 2003 11:15:48   t13ae27
      *neue Stufen VBAS und ALP
      *
      *   Rev 2.0   Feb 17 2003 10:58:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0662                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr, P.                                         *
      *    DATUM:    12.06.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00333!12.06.03! 01 ! NEUERSTELLUNG             ! Dürr     *
      * AAS00494!13.09.04! 03 ! ED-Stufe  in PI-Bereich   ! Dürr     *
      * AAS01008!07.03.12! 04 ! SIEGERADLER 2012          ! Willig   *
      * AAS01158!12.01.16! 05 ! EH-GS-XX-GR IN            ! WILLIG   *
      *         !        !    ! EH-GS-XX-GR-NEU GEÄNDERT  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0662                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0662.
          05 PI-DRVB0662-PROGRAMM           PIC  X(008).
          05 PI-DRVB0662-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0662-MS-NR              PIC  9(004).
          05 PI-DRVB0662-DAT-VON            PIC  X(010).
          05 PI-DRVB0662-DAT-BIS            PIC  X(010).
          05 PI-DRVB0662-ED-GRENZE          PIC  9(004) COMP.
          05 PI-DRVB0662-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB0662.

          05 PO-DRVB0662-RC                 PIC  X(003).
             88 PO-DRVB0662-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0662-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0662-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0662-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0662-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0662-DATEN.
             10 PO-DRVB0662-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB0662-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB0662-EH-GS-XX-GR-NEU PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************


