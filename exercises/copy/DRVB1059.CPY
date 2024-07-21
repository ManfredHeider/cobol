      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1059.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1059.CPv  $
      *
      *   Rev 2.1   May 11 2012 15:43:06   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.0   May 11 2012 14:17:00   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1059                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.05.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01008!11.05.12! 01 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1059                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1059.
          05 PI-DRVB1059-PROGRAMM           PIC  X(008).
          05 PI-DRVB1059-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1059-DAT-VON            PIC  X(010).
          05 PI-DRVB1059-DAT-BIS            PIC  X(010).
          05 PI-DRVB1059-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1059.

          05 PO-DRVB1059-RC                 PIC  X(003).
             88 PO-DRVB1059-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1059-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1059-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1059-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1059-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1059-DATEN.

             10 PO-DRVB1059-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1059-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1059-EH-GS-XX-GR-NEU PIC  S9(009)V99 COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************
