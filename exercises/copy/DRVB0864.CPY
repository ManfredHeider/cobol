      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0864.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0864.CPv  $
      *
      *   Rev 2.1   03 Sep 2007 16:56:08   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:39:28   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0864                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0109                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Hering                                           *
      *    DATUM:    28.08.2007                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!28.08.07! 2.1! NEUERSTELLUNG             !Hering    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0864                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0864.
           05  PI-DRVB0864-SWNA-KEY.
               10 PI-DRVB0864-ST-NR            PIC S9(007) BINARY.
               10 PI-DRVB0864-STICHTAG-ISO     PIC X(10).
      *
       01  PO-DRVB0864.
           05  PO-DRVB0864-RC                  PIC  X(003).
               88 PO-DRVB0864-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0864-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0864-SWNA-PRO-SATZ       PIC S9(03)V99 COMP-3.
           05  PO-DRVB0864-NI-SWNA-PRO-SATZ    PIC  S9(4) BINARY.
           05  PO-DRVB0864-DECKEL-BETRAG       PIC S9(09)V99 COMP-3.
           05  PO-DRVB0864-NI-DECKEL-BETRAG    PIC  S9(4) BINARY.
           05  PO-DRVB0864-PAUSCHAL-BETRAG     PIC S9(09)V99 COMP-3.
           05  PO-DRVB0864-NI-PAUSCHAL-BETRAG  PIC  S9(4) BINARY.
           05  PO-DRVB0864-MINIMAL-BETRAG      PIC S9(09)V99 COMP-3.
           05  PO-DRVB0864-NI-MINIMAL-BETRAG   PIC  S9(4) BINARY.
      *
      ***** Ende Copy-Member DRVB0864 *****************************

