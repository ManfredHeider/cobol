      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0529.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0529.CPv  $
      *
      *   Rev 2.1   20 Mar 2001 10:06:48   T13AE13
      *AAS000264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.0   22 Feb 2001 16:59:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0529                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    14.03.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!14.03.01! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0529                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0529.
          05 PI-DRVB0529-PROGRAMM           PIC  X(008).
          05 PI-DRVB0529-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0529-MA-ST-NR           PIC  S9(009) COMP.
      *
       01 PO-DRVB0529.

          05 PO-DRVB0529-RC                 PIC  X(003).
             88 PO-DRVB0529-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0529-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0529-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0529-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0529-DATEN.
             10 PO-DRVB0529-AV-DAT-VON          PIC  X(010).
             10 PO-DRVB0529-AV-DAT-BIS          PIC  X(010).
             10 PO-DRVB0529-AV-UMSCHREIB-DAT    PIC  X(010).
             10 PO-DRVB0529-NI-AV-UMSCHR-DAT    PIC  S9(004) COMP.
             10 PO-DRVB0529-AV-TYP              PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0529 *****************************
