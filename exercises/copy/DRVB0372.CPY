      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0372.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0372.CPv  $
      *
      *   Rev 2.1   May 19 1999 16:13:08   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 17 1999 10:59:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0372                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    19.03.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00127!19.03.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0372                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0372.
          05 PI-DRVB0372-PROGRAMM           PIC  X(008).
          05 PI-DRVB0372-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0372-DAT-VON            PIC  X(010).
          05 PI-DRVB0372-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0372.

          05 PO-DRVB0372-RC                 PIC  X(003).
             88 PO-DRVB0372-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0372-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0372-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0372-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0372-DATEN.
             10 PO-DRVB0372-MA-ST-NR        PIC  S9(009)  COMP.
             10 PO-DRVB0372-AV-DAT-VON      PIC  X(010).
             10 PO-DRVB0372-AV-DAT-BIS      PIC  X(010).
      *
      ***** Ende-Copy-Member *****************************************
