      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0385.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0385.CPv  $
      *
      *   Rev 2.1   Jun 01 1999 11:18:58   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0385                                     *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    23.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!23.04.99! 01 ! NEUERSTELLUNG             !WILLIG    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0385                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0385.
          05 PI-DRVB0385-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0385.

          05 PO-DRVB0385-RC                 PIC  X(003).
             88 PO-DRVB0385-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0385-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0385-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0385-DATEN.
             10 PO-DRVB0385-KNR-ZUSTELL      PIC S9(004)  COMP.
             10 PO-DRVB0385-KNR-GRND         PIC S9(004)  COMP.
      *
      ***** Ende Copy-Member DRVB0385 *****************************

      *---------------------------------------------------------------*
