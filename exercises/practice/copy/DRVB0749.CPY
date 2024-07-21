      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0749.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0749.CPv  $
      *
      *   Rev 2.1   Feb 11 2005 11:40:32   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Dec 13 2004 14:41:18   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0749                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    05.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!05.03.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0749                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0749.
          05 PI-DRVB0749-BETR               PIC  S9(009) COMP.
          05 PI-DRVB0749-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0749-KNR-WERTUNGS-TYP   PIC  S9(004) COMP.
          05 PI-DRVB0749-DAT-VON            PIC  X(010).
          05 PI-DRVB0749-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0749.

          05 PO-DRVB0749-RC                 PIC  X(003).
             88 PO-DRVB0749-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0749-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0749-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0749-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0749-DATEN.
             10 PO-DRVB0749-PARTEH-EH      PIC S9(009)V9 COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0748 *****************************
