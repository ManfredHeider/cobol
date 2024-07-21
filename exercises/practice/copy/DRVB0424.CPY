      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0424.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0424.CPv  $
      *
      *   Rev 2.4   May 03 2012 16:48:10   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.3   Sep 24 2004 09:25:28   t13ae27
      *AAS00494: Darstellung der EQF-elemenierten Siegeradler-Kandi
      *daten
      *
      *   Rev 2.2   Sep 03 2003 11:14:14   t13ae27
      *neue Stufen VBAS und ALP
      *
      *   Rev 2.1   Dec 21 1999 09:21:08   T13AE27
      *neue Funktion für ZJVB0012
      *
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0424                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr, P.                                         *
      *    DATUM:    22.07.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!22.07.99! 01 ! NEUERSTELLUNG             ! Dürr     *
      * AAS00333!12.06.03! 02 ! Stufennr. in PO-Bereich   ! Dürr     *
      * AAS00494!13.09.04! 03 ! ED-Stufe  in PI-Bereich   ! Dürr     *
      * AAS01008!07.03.12! 04 ! SIEGERADLER 2012          ! Willig   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0424                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0424.
          05 PI-DRVB0424-PROGRAMM           PIC  X(008).
          05 PI-DRVB0424-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0424-MS-NR              PIC  9(004).
          05 PI-DRVB0424-DAT-VON            PIC  X(010).
          05 PI-DRVB0424-DAT-BIS            PIC  X(010).
130904    05 PI-DRVB0424-ED-STUFE-AB        PIC  9(004) COMP.
070312    05 PI-DRVB0424-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB0424.

          05 PO-DRVB0424-RC                 PIC  X(003).
             88 PO-DRVB0424-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0424-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0424-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0424-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0424-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0424-DATEN.
             10 PO-DRVB0424-VBNR            PIC  S9(009)    COMP.
120603       10 PO-DRVB0424-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB0424-EH-GS-XX-GR     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
