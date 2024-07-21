      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0394.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0394.CPv  $
      *
      *   Rev 2.1   Jun 02 1999 14:25:52   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 11:00:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0394                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZWVB0050                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    05.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!05.05.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0394                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0394.
          05 PI-DRVB0394-SDNR-ST-NR              PIC S9(009) COMP.
      *
       01 PO-DRVB0394.

          05 PO-DRVB0394-RC                    PIC  X(003).
             88 PO-DRVB0394-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0394-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0394-RC-NFD                       VALUE 'NFD'.

          05 PO-DRVB0394-DATEN.
             10 PO-DRVB0394-SDNR-BEZ             PIC  X(040).

      *
      ***** ENDE COPY-MEMBER DRVB0392 *****************************
      *---------------------------------------------------------------*
