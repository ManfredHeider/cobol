      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0351.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0351.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 14:32:42   T13AE35
      *AAS00096 - neu
      *
      *   Rev 1.0   22 Jan 1999 13:40:22   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0351                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    26.01.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00096!26.01.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0351                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0351.
          05 PI-DRVB0351-BVH-UNTST-NR       PIC S9(009) COMP.
      *
       01 PO-DRVB0351.

          05 PO-DRVB0351-RC                 PIC  X(003).
             88 PO-DRVB0351-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0351-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0351-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0351-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0351-DATEN.
             10 PO-DRVB0351-BVH-BETR-NR      PIC S9(009) COMP.
             10 PO-DRVB0351-BVH-DAT-VON      PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0351 *****************************
