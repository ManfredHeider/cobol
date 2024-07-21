      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0358.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0358.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 15:46:32   T13AE35
      *AAS00096 neu
      *
      *   Rev 1.0   22 Jan 1999 13:40:42   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0358                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0003                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    19.02.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00096!19.02.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0358                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0358.
          05 PI-DRVB0358-ED-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0358.

          05 PO-DRVB0358-RC                 PIC  X(003).
             88 PO-DRVB0358-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0358-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0358-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0358-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0358-DATEN.
             10 PO-DRVB0358-ED-BEZ          PIC X(020).
             10 PO-DRVB0358-MA-ST-NR        PIC S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0358 *****************************
