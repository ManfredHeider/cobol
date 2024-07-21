      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0841.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0841.CPv  $
      *
      *   Rev 2.1   Jan 10 2007 11:00:30   t13ae27
      *AAS00667: JWB 2007 AIDAdiva - ERG-Ermittlung
      *
      *   Rev 2.0   Dec 28 2006 11:58:50   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0841                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0038                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    23.01.2006                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00595!23.01.06! 01 ! NEUERSTELLUNG             ! Duerr    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0841                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0841.
          05 PI-DRVB0841-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0841-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0841-KNR-WERTUNGS-TYP   PIC  S9(004) COMP.
          05 PI-DRVB0841-PARTEH-BETR        PIC  S9(009) COMP.
          05 PI-DRVB0841-DATUM-VON          PIC  X(010).
          05 PI-DRVB0841-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0841.

          05 PO-DRVB0841-RC                 PIC  X(003).
             88 PO-DRVB0841-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0841-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0841-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0841-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0841-DATEN.
             10 PO-DRVB0841-PARTEH-UNTST    PIC  S9(009) COMP.
             10 PO-DRVB0841-PARTEH-EH       PIC  S9(009)V9 COMP-3.
             10 PO-DRVB0841-ANZ-MONATE      PIC  S9(003) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0841 *****************************
