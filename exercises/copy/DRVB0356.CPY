      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0356.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0356.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 12:33:48   T13AE35
      *AAS00095- neu
      *
      *   Rev 1.0   22 Jan 1999 13:40:36   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0356                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0023                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    10.02.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00095!10.02.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0356                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0356.
          05 PI-DRVB0356-EDD-BEZUGS-NR      PIC S9(009) COMP.
      *
       01 PO-DRVB0356.

          05 PO-DRVB0356-RC                 PIC  X(003).
             88 PO-DRVB0356-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0356-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0356-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0356-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0356-DATEN.
             10 PO-DRVB0356-ED-ST-NR        PIC S9(009) COMP.
             10 PO-DRVB0356-EDD-DAT-VON     PIC X(026).
      *
      ***** ENDE COPY-MEMBER DRVB0356 *****************************
