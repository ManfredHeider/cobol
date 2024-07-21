      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0732.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0732.CPv  $
      *
      *   Rev 2.1   Oct 11 2004 12:05:52   t13ae27
      *ASL00183: MAP - Manageraufbauplan Ermittlung Ansprüche für D
      *-Leiter (Fiktiv)
      *
      *   Rev 2.0   Aug 26 2004 11:22:02   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0732                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.08.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00183!26.08.04! 01 ! NEUERSTELLUNG             ! P. Duerr *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0732                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0732.
          05 PI-DRVB0732-BVH-UNTST-NR       PIC S9(009) COMP.
          05 PI-DRVB0732-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0732.

          05 PO-DRVB0732-RC                 PIC  X(003).
             88 PO-DRVB0732-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0732-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0732-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0732-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0732-DATEN.
             10 PO-DRVB0732-BVH-BETR-NR     PIC S9(009) COMP.
             10 PO-DRVB0732-BVH-DAT-VON     PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0732 *****************************
