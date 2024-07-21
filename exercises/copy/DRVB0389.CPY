      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0389.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0389.CPv  $
      *
      *   Rev 2.1   May 10 1999 10:17:10   T13AE30
      *AAS00051 - Neuerstellung wg. Stammdaten-DTA
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0389                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT,B.                                       *
      *    DATUM:    07.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!07.05.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0389                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0389.
          05 PI-DRVB0389-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0389.

          05 PO-DRVB0389-RC                 PIC  X(003).
             88 PO-DRVB0389-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0389-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0389-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0389-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0389-DATEN.
             10 PO-DRVB0389-MAT-DAT-VON      PIC  X(026).
             10 PO-DRVB0389-MAT-DAT-BIS      PIC  X(010).

      *
      ***** ENDE COPY-MEMBER DRVB0389 *****************************

