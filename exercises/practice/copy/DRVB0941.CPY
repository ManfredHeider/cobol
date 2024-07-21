      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0941.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0941.CPv  $
      *
      *   Rev 2.1   May 27 2009 12:29:44   t13ex57
      *AVB01035: Kündigungsfristen neuer VB-Vertrag
      *
      *   Rev 2.0   May 13 2009 13:13:10   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0941                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOLL,U.                                          *
      *    DATUM:    12.05.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01035!12.05.09!2.1 ! Kuendigungsfristen Neuer  ! U. Boll  *
      *         !        !    ! VB-VERTRAG                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0941                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0941.
          05 PI-DRVB0941-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0941-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0941.

          05 PO-DRVB0941-RC                 PIC  X(003).
             88 PO-DRVB0941-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0941-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0941-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0941-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0941-DATEN.
             10 PO-DRVB0941-VBP-NR           PIC S9(004) COMP.
