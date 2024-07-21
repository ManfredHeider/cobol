      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0519.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0519.CPv  $
      *
      *   Rev 2.3   May 03 2012 16:48:16   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.2   Sep 03 2003 11:15:08   t13ae27
      *neue Stufen VBAS und ALP
      *
      *   Rev 2.1   Jan 03 2002 11:04:52   t13ae27
      *Netto-Neu-Gesamt statt Netto-Gesamt, 
      *40-%-Regel auf akt. Stufe
      *
      *   Rev 2.0   07 Sep 2000 11:32:26   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0519                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr, P.                                         *
      *    DATUM:    09.02.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00266!09.02.01! 01 ! NEUERSTELLUNG             ! Dürr     *
      * AAS00333!12.06.03! 02 ! Stufennr. in PO-Bereich   ! Dürr     *
      * AAS01008!07.03.12! 03 ! SIEGERADLER 2012          ! Willig   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0519                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0519.
          05 PI-DRVB0519-PROGRAMM           PIC  X(008).
          05 PI-DRVB0519-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0519-MS-NR              PIC  9(004).
          05 PI-DRVB0519-DAT-VON            PIC  X(010).
          05 PI-DRVB0519-DAT-BIS            PIC  X(010).
070312    05 PI-DRVB0519-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB0519.

          05 PO-DRVB0519-RC                 PIC  X(003).
             88 PO-DRVB0519-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0519-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0519-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0519-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0519-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0519-DATEN.
             10 PO-DRVB0519-VBNR            PIC  S9(009)    COMP.
120603       10 PO-DRVB0519-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB0519-EH-GS-XX-GR     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************

