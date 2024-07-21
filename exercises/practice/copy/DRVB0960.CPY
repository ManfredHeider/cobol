      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0960.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0960.CPv  $
      *
      *   Rev 2.1   Nov 18 2009 15:46:30   t13ae36
      *AAS00829: Erweiterung Access-DB für Ö - Sperre Abrechnung
      *
      *   Rev 2.0   Nov 17 2009 15:51:54   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0960                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZJVB0034                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Franke                                           *
      *    DATUM:    18.11.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00829!05.05.09! 01 ! NEUERSTELLUNG             ! R.Franke *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0960                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0960.
          05 PI-DRVB0960-ST-NR                 PIC S9(009) COMP.
          05 PI-DRVB0960-BV-TYP                PIC S9(004) COMP.
      *
       01 PO-DRVB0960.

          05 PO-DRVB0960-RC                    PIC  X(003).
             88 PO-DRVB0960-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0960-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0960-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0960-DATEN.
             10 PO-DRVB0960-BV-KTO-STATUS-SEIT PIC  X(010).
             10 PO-DRVB0960-BV-KTO-SPERRE-TEXT PIC  X(040).
             10 PO-DRVB0960-BV-KTO-SPERRE-KNR  PIC  9(002).
      *
      ***** ENDE COPY-MEMBER DRVB0960 *****************************



