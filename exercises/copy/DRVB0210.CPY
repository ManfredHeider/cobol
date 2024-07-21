      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0210.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0210.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 May 1998 15:21:20   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:16:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0210                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    18.02.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!18.02.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0210                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0210.
          05 PI-DRVB0210-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0210.

          05 PO-DRVB0210-RC                 PIC  X(003).
             88 PO-DRVB0210-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0210-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0210-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0210-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0210-DATEN.
             10 PO-DRVB0210-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0210-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0210-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0210-KNR-AUSL         PIC S9(004)  COMP.
             10 PO-DRVB0210-KTY-AUSL         PIC S9(004)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0210 *****************************
