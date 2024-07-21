      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0299.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0299.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Aug 1998 16:54:42   T13AE35
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:28:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0299                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0014                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    13.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!13.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0299                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0299.
          05 PI-DRVB0299-MAN-BEZUGS-NR      PIC S9(009) COMP.
      *
       01 PO-DRVB0299.

          05 PO-DRVB0299-RC                 PIC  X(003).
             88 PO-DRVB0299-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0299-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0299-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0299-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0299-DATEN.
             10 PO-DRVB0299-MAN-NAME         PIC  X(040).
             10 PO-DRVB0299-MAN-VORNAME      PIC  X(020).
             10 PO-DRVB0299-MAN-ANREDE       PIC  9(004).
             10 PO-DRVB0299-MAN-TITEL-AKAD   PIC  X(010).
             10 PO-DRVB0299-MAN-TITEL-ADEL   PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0299 *****************************
