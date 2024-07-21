      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0838.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0838.CPv  $
      *
      *   Rev 2.1   08 Nov 2006 16:52:12   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 16:28:42   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0838                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0103                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *                                                               *
      *    Buchtyp aus BUKA                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    24.10.2006                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AIS00060!24.10.02! 2.1! Neu, Buchungstyp          ! Hering   *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0838                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB0838.
          05 PI-DRVB0838-GEGEN-KONTO        PIC X(012).
          05 PI-DRVB0838-BELEG-ART          PIC X(003).
          05 PI-DRVB0838-KONTO-TYP          PIC X(001).
          05 PI-DRVB0838-SHKZ               PIC X(001).
          05 PI-DRVB0838-KTY-GESELL-TYP     PIC S9(004) BINARY.
          05 PI-DRVB0838-KNR-GESELL-TYP     PIC S9(004) BINARY.
      *
       01 PO-DRVB0838.

          05 PO-DRVB0838-RC                 PIC  X(003).
             88 PO-DRVB0838-RC-OK                VALUE 'OK '.
             88 PO-DRVB0838-RC-LEER              VALUE 'LER'.
             88 PO-DRVB0838-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB0838-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB0838-DATEN.
             10 PO-DRVB0838-BUKA-DAT-VON    PIC X(010).
             10 PO-DRVB0838-KTY-BUCH-TYP    PIC S9(004) BINARY.
             10 PO-DRVB0838-KNR-BUCH-TYP    PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0838 *****************************

