      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0196.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0196.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Apr 1998 16:29:30   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:14:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0196                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0031                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    04.12.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!04.12.97! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0196                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0196.
          05 PI-DRVB0196-VBNR               PIC S9(009) COMP.
          05 PI-DRVB0196-PCZ-TYP            PIC S9(004) COMP.
      *
       01 PO-DRVB0196.

          05 PO-DRVB0196-RC                 PIC  X(003).
             88 PO-DRVB0196-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0196-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0196-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0196-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0196-DATEN.
             10 PO-DRVB0196-PCZ-NR          PIC S9(009)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0196 *****************************
