      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0445.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0445.CPv  $
      *
      *   Rev 2.1   Sep 17 1999 08:35:14   T13EX19
      *aki00048 - neuerstellung
      *
      *   Rev 2.0   Aug 04 1999 10:30:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0445                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0019                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGU                                              *
      *    DATUM:    03.09.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Aki00048!06.09.99! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0445                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0445.
          05 PI-DRVB0445-PROGRAMM           PIC  X(008).
          05 PI-DRVB0445-UFKT-CODE          PIC  9(002).

          05 PI-DRVB0445-KEY.
             10 PI-DRVB0445-DIR-NR          PIC  S9(004)  COMP.
      *
       01 PO-DRVB0445.

          05 PO-DRVB0445-RC                 PIC  X(003).
             88 PO-DRVB0445-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0445-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0445-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0445-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0445-DATEN.
             10 PO-DRVB0445-datum-von       PIC  x(10).
             10 PO-DRVB0445-datum-bis       PIC  x(10).
             10 PO-DRVB0445-last-update     PIC  x(26).
      *
      ***** Ende-Copy-Member *****************************************
