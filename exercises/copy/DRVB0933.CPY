      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0933.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0933.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 10:30:26   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - VB29
      *
      *   Rev 2.0   Mar 24 2009 12:21:08   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0933                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0003                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    24.03.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01029!24.03.09!2.1 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0933                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0933.
          05 PI-DRVB0933-ED-ZWEIG           PIC S9(009) COMP.
      *
       01 PO-DRVB0933.

          05 PO-DRVB0933-RC                 PIC  X(003).
             88 PO-DRVB0933-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0933-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0933-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0933-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0933-DATEN.
             10 PO-DRVB0933-ED-ZWEIG-ANZ    PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0933 *****************************
