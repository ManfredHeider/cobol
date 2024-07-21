      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0392.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRVB0392.CPv  $
      *
      *   Rev 2.3   Jun 25 1999 08:20:54   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.2   Jun 25 1999 08:11:44   T13AE30
      * 
      *
      *   Rev 2.1   Jun 02 1999 14:25:48   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 11:00:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0392                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZWVB0002                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    04.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!04.05.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0392                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0392.
          05 PI-DRVB0392-GK-ST-NR              PIC S9(009) COMP.
      *
       01 PO-DRVB0392.

          05 PO-DRVB0392-RC                    PIC  X(003).
             88 PO-DRVB0392-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0392-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0392-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0392-DATEN.
             10 PO-DRVB0392-GK-BEZ             PIC  X(040).

      *
      ***** ENDE COPY-MEMBER DRVB0392 *****************************
