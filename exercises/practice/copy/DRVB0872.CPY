      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0872.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0872.CPv  $
      *
      *   Rev 2.2   05 Dec 2007 11:22:44   t13ae30
      *AVB00905: Allfinanz - Weitere SST  --> AC V2  TWI
      *
      *   Rev 2.1   13 Nov 2007 15:38:00   t13ae30
      *FVB00704: Abbruch im VB498225
      *
      *   Rev 2.0   13 Nov 2007 14:23:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0872                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    13.11.2007                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FVB00704!13.11.07! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AVB00905!22.11.07! 02 ! EINFÜHREN ALLFINANZ       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0872                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0872.
          05 PI-DRVB0872-PROGRAMM           PIC  X(008).
          05 PI-DRVB0872-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0872.

          05 PO-DRVB0872-RC                 PIC  X(003).
             88 PO-DRVB0872-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0872-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0872-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0872-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0872-DATEN.
             10 PO-DRVB0872-ST-NR           PIC  9(7)    COMP-3.
             10 PO-DRVB0872-KNR-ST-TYP      PIC S9(004)  BINARY.
221107       10 PO-DRVB0872-KNR-GESELL-TYP  PIC S9(004)  BINARY.
      *
      ***** Ende Copy-Member DRVB0872 *****************************
