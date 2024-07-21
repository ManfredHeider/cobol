      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0812.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0812.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:51:52   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 30 2006 12:20:00   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0812                                      *
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
      *    FÜR DIE FUNKTION DRVB0812                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0812.
          05 PI-DRVB0812-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0812-PARTNER            PIC  S9(009) COMP.
          05  PI-DRVB0812-KNR-WB-TYP        PIC  S9(004)    COMP.
          05  PI-DRVB0812-KNR-WERTUNGS-TYP  PIC  S9(004)    COMP.
      *
       01 PO-DRVB0812.

          05 PO-DRVB0812-RC                 PIC  X(003).
             88 PO-DRVB0812-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0812-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0812-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0812-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0812-DATEN.
           10  PO-DRVB0812-PARTEH-TUPEL     PIC  X(091).
      *
      ***** ENDE COPY-MEMBER DRVB0812 *****************************
