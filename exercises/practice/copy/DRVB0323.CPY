      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0323.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0323.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   18 Nov 1998 12:16:16   T13AE42
      *AAS00078 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0323                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    02.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00078!02.10.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0323                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0323.
          05 PI-DRVB0323-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0323-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0323.

          05 PO-DRVB0323-RC                 PIC  X(003).
             88 PO-DRVB0323-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0323-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0323-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0323-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0323-DATEN.
             10 PO-DRVB0323-VBP-NR           PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0323 *****************************
