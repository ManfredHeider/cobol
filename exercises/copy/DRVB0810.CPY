      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0810.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0810.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:47:08   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 27 2006 15:27:52   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0810                                      *
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
      *    FÜR DIE FUNKTION DRVB0810                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0810.
          05 PI-DRVB0810-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0810-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0810-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0810-DATUM-VON          PIC  X(010).
          05 PI-DRVB0810-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0810.

          05 PO-DRVB0810-RC                 PIC  X(003).
             88 PO-DRVB0810-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0810-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0810-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0810-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0810-DATEN.
             10 PO-DRVB0810-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0810-MSBZ-DAT-VON    PIC  X(010).
             10 PO-DRVB0810-MSBZ-DAT-BIS    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0810 *****************************
