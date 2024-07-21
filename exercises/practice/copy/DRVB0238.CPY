      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0238.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0238.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Aug 1998 12:13:02   T13AE35
      *AAS00049 - Neuerstellung, PI/PO fuer ZJVB0005
      *
      *   Rev 1.0   23 Jul 1997 12:20:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0238                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    11.05.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!11.05.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0238                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0238.
          05 PI-DRVB0238-PROGRAMM           PIC  X(008).
          05 PI-DRVB0238-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0238-ZEITPUNKT-ISO      PIC  X(010).
      *
       01 PO-DRVB0238.

          05 PO-DRVB0238-RC                 PIC  X(003).
             88 PO-DRVB0238-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0238-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0238-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0238-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0238-DATEN.
             10 PO-DRVB0238-MA-ST-NR        PIC  S9(009)  COMP.
             10 PO-DRVB0238-MS-NR           PIC  S9(004)  COMP.
             10 PO-DRVB0238-MSBZ-DAT-VON    PIC  X(010).
      *
      ***** Ende-Copy-Member *****************************************
