      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0370.CPv  $
      *
      *   Rev 2.1   May 19 1999 16:11:58   T13AE42
      *AAS00127 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jan 1999 13:41:14   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0370                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    18.03.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00127!18.03.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0370                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0370.
          05 PI-DRVB0370-PROGRAMM           PIC  X(008).
          05 PI-DRVB0370-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0370-MA-ST-NR           PIC  S9(009) COMP.
          05 PI-DRVB0370-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0370.

          05 PO-DRVB0370-RC                 PIC  X(003).
             88 PO-DRVB0370-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0370-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0370-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0370-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0370-DATEN.
             10 PO-DRVB0370-AV-DAT-VON          PIC  X(010).
             10 PO-DRVB0370-AV-DAT-BIS          PIC  X(010).
             10 PO-DRVB0370-AV-UMSCHREIB-DAT    PIC  X(010).
             10 PO-DRVB0370-NI-AV-UMSCHR-DAT    PIC  S9(004) COMP.
             10 PO-DRVB0370-AV-TYP              PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0370 *****************************
