      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0291.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0291.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 Jul 1998 12:44:40   T13AE35
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:27:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0291                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0006                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    01.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!01.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0291                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0291.
          05 PI-DRVB0291-DIR-NR             PIC S9(004) COMP.
      *
       01 PO-DRVB0291.

          05 PO-DRVB0291-RC                 PIC  X(003).
             88 PO-DRVB0291-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0291-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0291-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0291-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0291-DATEN.
             10 PO-DRVB0291-DIR-NAME         PIC  X(020).
      *
      ***** ENDE COPY-MEMBER DRVB0291 *****************************
