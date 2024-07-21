      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0450.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0450.CPv  $
      *
      *   Rev 2.1   Jan 11 2000 16:26:46   T13AE30
      *AAS00051 - Stammdaten-DTA -> Aachen
      *
      *   Rev 2.0   Aug 04 1999 10:30:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0450                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0019                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    26.10.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!26.10.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0450                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0450.
          05 PI-DRVB0450-DL-BEZUGS-NR       PIC S9(009) COMP.
      *
       01 PO-DRVB0450.

          05 PO-DRVB0450-RC                 PIC  X(003).
             88 PO-DRVB0450-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0450-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0450-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0450-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0450-DATEN.
             10 PO-DRVB0450-DIR-NR          PIC S9(004)  COMP.
      *
      ***** Ende Copy-Member DRVB0450 *****************************
