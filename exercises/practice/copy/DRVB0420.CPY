      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0420.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0420.CPv  $
      *
      *   Rev 2.2   Aug 10 1999 09:54:40   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Jul 21 1999 10:00:48   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.0   May 27 1999 13:58:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0420                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    19.07.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!19.07.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0420                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0420.
          05 PI-DRVB0420-PROGRAMM           PIC  X(008).
          05 PI-DRVB0420-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0420-GESELL-TYP         PIC S9(004)  COMP.
      *
       01 PO-DRVB0420.

          05 PO-DRVB0420-RC                 PIC  X(003).
             88 PO-DRVB0420-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0420-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0420-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0420-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0420-DATEN.
             10 PO-DRVB0420-ST-NR           PIC S9(009)    COMP.
             10 PO-DRVB0420-ST-PRUEFZIFFER  PIC  X(001)        .
             10 PO-DRVB0420-KNR-ST-TYP      PIC S9(004)    COMP.
             10 PO-DRVB0420-KTY-ST-TYP      PIC S9(004)    COMP.
      *
      ***** Ende-Copy-Member *****************************************
