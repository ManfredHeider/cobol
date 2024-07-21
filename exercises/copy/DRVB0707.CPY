      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0707.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0707.CPv  $
      *
      *   Rev 2.1   May 18 2004 13:41:02   t13ae30
      *Verkaufsförderung Riesterprodukte
      *
      *   Rev 2.0   May 05 2004 17:29:52   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0707                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0021                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    05.05.2004                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00467!05.05.04! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0707                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0707.
          05 PI-DRVB0707-ST-NR              PIC S9(009) COMP.
          05 PI-DRVB0707-BV-TYP             PIC S9(004) COMP.
      *
       01 PO-DRVB0707.

          05 PO-DRVB0707-RC                 PIC  X(003).
             88 PO-DRVB0707-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0707-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0707-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0707-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ***** ENDE COPY-MEMBER DRVB0707 *****************************
