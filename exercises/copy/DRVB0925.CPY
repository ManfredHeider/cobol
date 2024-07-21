      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0925.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0925.CPv  $
      *
      *   Rev 2.1   Nov 28 2008 10:21:40   t13ae27
      *AAS00793: JWB2009 Begegnungsstätten - Ermittlung
      *
      *   Rev 2.0   Nov 13 2008 13:16:32   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0925                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
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
      *    FÜR DIE FUNKTION DRVB0925                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0925.
          05 PI-DRVB0925-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0925-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0925-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0925-DATUM-VON          PIC  X(010).
          05 PI-DRVB0925-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0925.

          05 PO-DRVB0925-RC                 PIC  X(003).
             88 PO-DRVB0925-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0925-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0925-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0925-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0925-DATEN.
             10 PO-DRVB0925-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0925-MSBZ-DAT-VON    PIC  X(010).
             10 PO-DRVB0925-MSBZ-DAT-BIS    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0925 *****************************
