      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0204.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0204.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Apr 1998 16:29:58   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:15:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0204                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0031                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    19.12.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!19.12.97! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0204                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0204.
          05 PI-DRVB0204-VBNR               PIC S9(009) COMP.
      *
       01 PO-DRVB0204.

          05 PO-DRVB0204-RC                 PIC  X(003).
             88 PO-DRVB0204-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0204-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0204-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0204-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0204-DATEN.
             10 PO-DRVB0204-PCZ-ANZAHL      PIC S9(004)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0204 *****************************
