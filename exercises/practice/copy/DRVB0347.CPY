      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0347.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0347.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 14:25:14   T13AE35
      *AAS00095 - neu
      *
      *   Rev 1.0   27 Oct 1998 16:56:52   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0347                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.12.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00095!08.12.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0347                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0347.
          05 PI-DRVB0347-BVH-UNTST-NR       PIC S9(009) COMP.
      *
       01 PO-DRVB0347.

          05 PO-DRVB0347-RC                 PIC  X(003).
             88 PO-DRVB0347-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0347-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0347-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0347-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0347-DATEN.
             10 PO-DRVB0347-BVH-BETR-NR      PIC S9(009) COMP.
             10 PO-DRVB0347-BVH-DAT-VON      PIC X(026).
      *
      ***** ENDE COPY-MEMBER DRVB0347 *****************************
