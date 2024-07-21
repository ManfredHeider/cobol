      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0898.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0898.CPv  $
      *
      *   Rev 2.1   Apr 15 2008 15:41:36   t13ae27
      *AAS00747: Wettbewerb 2008 der Allfinanz 
      *
      *   Rev 2.0   Apr 01 2008 15:34:10   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0898                                      *
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
      *    FÜR DIE FUNKTION DRVB0898                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0898.
          05 PI-DRVB0898-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0898-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0898-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0898-DATUM-VON          PIC  X(010).
          05 PI-DRVB0898-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0898.

          05 PO-DRVB0898-RC                 PIC  X(003).
             88 PO-DRVB0898-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0898-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0898-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0898-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0898-DATEN.
             10 PO-DRVB0898-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0898-MSBZ-DAT-VON    PIC  X(010).
             10 PO-DRVB0898-MSBZ-DAT-BIS    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0898 *****************************
