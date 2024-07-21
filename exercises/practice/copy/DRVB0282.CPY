      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0282.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0282.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Aug 1998 16:53:34   T13AE35
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:26:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0282                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    20.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!20.06.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0282                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0282.
          05 PI-DRVB0282-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0282.

          05 PO-DRVB0282-RC                 PIC  X(003).
             88 PO-DRVB0282-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0282-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0282-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0282-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0282-DATEN.
             10 PO-DRVB0282-MAT-DAT-BIS      PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0282 *****************************
