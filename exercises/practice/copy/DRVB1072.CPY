      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1072.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1072.CPv  $
      *
      *   Rev 2.1   Feb 17 2014 14:43:08   t13ae30
      *AAS01093: DVAG-Wien: Siegeradler - Änderung Wertungsgruppen
      *
      *   Rev 2.0   Jan 31 2014 15:10:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1072                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    31.01.2014                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01091!31.01.14! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1072                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1072.
          05 PI-DRVB1072-PROGRAMM           PIC  X(008).
          05 PI-DRVB1072-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1072-MS-NR              PIC  9(004).
          05 PI-DRVB1072-DAT-VON            PIC  X(010).
          05 PI-DRVB1072-DAT-BIS            PIC  X(010).
          05 PI-DRVB1072-ED-STUFE-AB        PIC  9(004) COMP.
          05 PI-DRVB1072-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1072.
          05 PO-DRVB1072-RC                 PIC  X(003).
             88 PO-DRVB1072-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1072-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1072-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1072-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1072-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1072-DATEN.
             10 PO-DRVB1072-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1072-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1072-EH-GS-XX-GR     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
