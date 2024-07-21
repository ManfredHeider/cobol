      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.12.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0963.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0963.CPv  $
      *
      *   Rev 2.1   Feb 04 2010 15:51:18   t13ae27
      *AAS00872: Allfinanz Wettbewerbe 2010 - Progr.
      *
      *   Rev 2.0   Dec 29 2009 13:51:02   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0963                                      *
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
      * AAS00872!29.12.09! 01 ! NEUERSTELLUNG             ! Duerr    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0963                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0963.
          05 PI-DRVB0963-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0963-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0963-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0963-DATUM-VON          PIC  X(010).
          05 PI-DRVB0963-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0963.

          05 PO-DRVB0963-RC                 PIC  X(003).
             88 PO-DRVB0963-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0963-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0963-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0963-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0963-DATEN.
             10 PO-DRVB0963-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0963-MSBZ-DAT-VON    PIC  X(010).
             10 PO-DRVB0963-MSBZ-DAT-BIS    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0925 *****************************
