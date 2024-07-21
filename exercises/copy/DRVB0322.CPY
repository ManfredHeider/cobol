      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0322.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0322.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   18 Nov 1998 12:15:42   T13AE42
      *AAS00078 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0322                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    07.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00078!07.10.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0322                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0322.
          05 PI-DRVB0322-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0322-MS-NR-VON          PIC S9(004) COMP.
          05 PI-DRVB0322-MS-NR-BIS          PIC S9(004) COMP.
          05 PI-DRVB0322-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0322.

          05 PO-DRVB0322-RC                 PIC  X(003).
             88 PO-DRVB0322-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0322-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0322-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0322-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0322-DATEN.
             10 PO-DRVB0322-MA-ST-NR        PIC S9(009)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0322 *****************************
