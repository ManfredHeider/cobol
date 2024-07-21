      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0296.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0296.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Aug 1998 16:53:54   T13AE35
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:27:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0296                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!06.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0296                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0296.
          05 PI-DRVB0296-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0296.

          05 PO-DRVB0296-RC                 PIC  X(003).
             88 PO-DRVB0296-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0296-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0296-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0296-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0296-DATEN.
             10 PO-DRVB0296-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0296-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0296-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0296-AUSL             PIC  X(004).
             10 PO-DRVB0296-KNR-GRUND        PIC  9(004).
      *
      ***** ENDE COPY-MEMBER DRVB0296 *****************************
