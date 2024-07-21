      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0880.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0880.CPv  $
      *
      *   Rev 2.1   13 Dec 2007 16:06:30   t13ae27
      *AAS00725: Jahreswettbewerb 2008/AIDAdiva/Elite-Club Unterneh
      *mensaufbau
      *
      *   Rev 2.0   07 Dec 2007 11:05:48   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0880                                      *
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
      *    FÜR DIE FUNKTION DRVB0880                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0880.
          05 PI-DRVB0880-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0880-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0880-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0880-DATUM-VON          PIC  X(010).
          05 PI-DRVB0880-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0880.

          05 PO-DRVB0880-RC                 PIC  X(003).
             88 PO-DRVB0880-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0880-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0880-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0880-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0880-DATEN.
             10 PO-DRVB0880-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0880-MSBZ-DAT-VON    PIC  X(010).
             10 PO-DRVB0880-MSBZ-DAT-BIS    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0880 *****************************
