      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0756.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0756.CPv  $
      *
      *   Rev 2.1   Feb 24 2005 10:51:20   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Feb 23 2005 10:34:24   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0756                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    21.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00062!21.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0756                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0756.
          05 PI-DRVB0756-DZ-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0756-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0756.

          05 PO-DRVB0756-RC                 PIC  X(003).
             88 PO-DRVB0756-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0756-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0756-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0756-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0756-DATEN.
             10 PO-DRVB0756-DZ-DIR-NR        PIC S9(004) COMP.
             10 PO-DRVB0756-DZ-DAT-VON       PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0756 *****************************
