      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0748.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0748.CPv  $
      *
      *   Rev 2.2   Feb 14 2005 10:52:26   t13ex13
      *AAS00532: Basiszahlen Einheiten neuer VB ab 01.07.2003 je Re
      *gion
      *
      *   Rev 2.1   Feb 01 2005 11:08:22   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Dec 07 2004 11:37:28   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0748                                      *
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
      * AAS00532!31.01.05!2.1 ! EINFÜGEN 'STUFE-AB'       !GUTFLEISCH*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0748                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0748.
          05 PI-DRVB0748-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0748-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0748-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0748-DATUM-VON          PIC  X(010).
          05 PI-DRVB0748-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0748.

          05 PO-DRVB0748-RC                 PIC  X(003).
             88 PO-DRVB0748-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0748-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0748-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0748-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0748-DATEN.
             10 PO-DRVB0748-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0748-MSBZ-DAT-VON    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0748 *****************************
