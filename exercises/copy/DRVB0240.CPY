      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0240.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0240.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Jun 1998 12:05:20   T13EX30
      *AAS00042 - Neuerstellung wegen DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   23 Jul 1997 12:20:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0240                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0006                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, BARBARA                                 *
      *    DATUM:    11.05.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASS00042!11.05.98! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0240                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0240.
          05 PI-DRVB0240-PROGRAMM           PIC  X(008).
          05 PI-DRVB0240-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0240-DIR                OCCURS 8.
             10 PI-DRVB0240-DIR-NR          PIC S9(4) USAGE COMP.
      *
       01 PO-DRVB0240.

          05 PO-DRVB0240-RC                 PIC  X(003).
             88 PO-DRVB0240-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0240-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0240-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0240-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0240-DATEN.
             10 PO-DRVB0240-ST-NR           PIC  9(7)    COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
