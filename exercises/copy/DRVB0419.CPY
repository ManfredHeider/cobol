      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0419.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0419.CPv  $
      *
      *   Rev 2.1   Jul 13 1999 10:02:52   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.0   May 27 1999 13:58:38   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0419                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZWVB0018                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    13.07.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!13.07.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0419                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0419.
          05 PI-DRVB0419-DZ-ST-NR              PIC S9(009) COMP.
      *
       01 PO-DRVB0419.

          05 PO-DRVB0419-RC                    PIC  X(003).
             88 PO-DRVB0419-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0419-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0419-RC-NFD                       VALUE 'NFD'.

          05 PO-DRVB0419-DATEN.
             10 PO-DRVB0419-DZ-DIR-NR          PIC S9(004) COMP.

      *
      ***** ENDE COPY-MEMBER DRVB0392 *****************************
