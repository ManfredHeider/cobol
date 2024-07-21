      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0791.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0791.CPv  $
      *
      *   Rev 2.2   May 03 2012 16:48:24   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.1   Nov 04 2005 11:55:22   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.0   Sep 02 2005 14:38:00   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0791                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr, P.                                         *
      *    DATUM:    12.06.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00333!12.06.03! 01 ! NEUERSTELLUNG             ! Dürr     *
      * AAS00494!13.09.04! 03 ! ED-Stufe  in PI-Bereich   ! Dürr     *
      * AAS01008!07.03.12! 04 ! SIEGERADLER 2012          ! Willig   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0791                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0791.
          05 PI-DRVB0791-PROGRAMM           PIC  X(008).
          05 PI-DRVB0791-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0791-MS-NR              PIC  9(004).
          05 PI-DRVB0791-DAT-VON            PIC  X(010).
          05 PI-DRVB0791-DAT-BIS            PIC  X(010).
130904    05 PI-DRVB0791-ED-STUFE-AB        PIC  9(004) COMP.
070312    05 PI-DRVB0791-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB0791.

          05 PO-DRVB0791-RC                 PIC  X(003).
             88 PO-DRVB0791-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0791-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0791-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0791-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0791-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0791-DATEN.
             10 PO-DRVB0791-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB0791-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB0791-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
