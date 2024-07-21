      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0231.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0231.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jun 1998 15:46:06   T13EX30
      *AAS00049 - Neuerstellung fuer DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   23 Jul 1997 12:19:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0231                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0019                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    02.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!02.06.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0231                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0231.
          05 PI-DRVB0231-PROGRAMM           PIC  X(008).
          05 PI-DRVB0231-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0231.

          05 PO-DRVB0231-RC                 PIC  X(003).
             88 PO-DRVB0231-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0231-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0231-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0231-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0231-DATEN.
             10 PO-DRVB0231-DIR-NR          PIC  S9(004)  COMP.
             10 PO-DRVB0231-DL-BEZUGS-NR    PIC  S9(009)  COMP.
             10 PO-DRVB0231-KNR-BEZUGS-TYP  PIC  S9(004)  COMP.
      *
      ***** Ende-Copy-Member *****************************************
