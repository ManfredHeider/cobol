      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1126.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1126.CPv  $
      *
      *   Rev 2.2   Mar 16 2016 13:25:30   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.1   Mar 16 2016 12:48:36   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.0   Jan 12 2016 13:54:06   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1126                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Th.Willig                                        *
      *    DATUM:    12.01.2016                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01158!12.01.016 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1126                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1126.
          05 PI-DRVB1126-PROGRAMM           PIC  X(008).
          05 PI-DRVB1126-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1126-DAT-VON            PIC  X(010).
          05 PI-DRVB1126-DAT-BIS            PIC  X(010).
          05 PI-DRVB1126-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1126.

          05 PO-DRVB1126-RC                 PIC  X(003).
             88 PO-DRVB1126-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1126-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1126-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1126-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1126-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1126-DATEN.
             10 PO-DRVB1126-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1126-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1126-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************


