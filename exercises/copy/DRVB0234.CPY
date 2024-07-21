      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0234.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0234.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Jun 1998 13:39:40   T13EX30
      *AAS00042 - Neuerstellung wegen DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   23 Jul 1997 12:19:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0234                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.05.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASS00042!06.05.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0234                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0234.
          05 PI-DRVB0234-PROGRAMM           PIC  X(008).
          05 PI-DRVB0234-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0234.

          05 PO-DRVB0234-RC                 PIC  X(003).
             88 PO-DRVB0234-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0234-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0234-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0234-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0234-DATEN.
             10 PO-DRVB0234-ST-NR           PIC  9(7)    COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
