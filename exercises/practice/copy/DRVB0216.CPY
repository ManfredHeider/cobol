      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0216.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0216.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 May 1998 15:22:52   T13EX30
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:17:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0216                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    05.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!05.03.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0216                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0216.
          05 PI-DRVB0216-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0216-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0216.

          05 PO-DRVB0216-RC                 PIC  X(003).
             88 PO-DRVB0216-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0216-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0216-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0216-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0216-DATEN.
             10 PO-DRVB0216-MS-NR           PIC S9(004)  COMP.
             10 PO-DRVB0216-BS-NR           PIC S9(004)  COMP.
             10 PO-DRVB0216-MSBZ-DAT-VON    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0216 *****************************
