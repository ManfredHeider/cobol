      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0858.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0858.CPv  $
      *
      *   Rev 2.1   31 Oct 2007 14:29:08   t13ae30
      *AVB00905: Allfinanz - Bausteine, Stammdaten-DTA TWI
      *
      *   Rev 2.0   29 Jun 2007 12:05:56   t13ae30
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0858                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      *    Beschreibung: ERM. OB EIN MA IN EINEM VORGEGEBENEN        *
      *                  ZEITRAUM EINGESTIEGEN IST                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: TH.WILLIG                                       *
      *    DATUM:    18.06.2007                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!18.06.07! 2.1! NEU                       ! Hering   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0858                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0858.
          05 PI-DRVB0858-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0858-ANF-ZEITRAUM       PIC  X(010).
          05 PI-DRVB0858-ENDE-ZEITRAUM      PIC  X(010).
      *
       01 PO-DRVB0858.

          05 PO-DRVB0858-RC                 PIC  X(003).
             88 PO-DRVB0858-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0858-RC-NOT-FOUND                 VALUE 'NFD'.

      *
      ***** ENDE COPY-MEMBER DRVB0858 *****************************
