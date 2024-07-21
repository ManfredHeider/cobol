      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1143.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1143.CPv  $
      *
      *   Rev 2.2   Jun 06 2016 16:21:42   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.1   May 31 2016 07:45:42   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   May 30 2016 08:31:32   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1143                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0100                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. Wölflik                                       *
      *    DATUM:    30.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!30.05.16!2.1 ! NEU                       ! H.Wölflik *
      * AVB01400!06.06.16!2.3 ! FIBUZ-INFO hinzu          ! H.Wölflik *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB1143                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1143.
          05 PI-DRVB1143-KNR-GESELL-TYP     PIC S9(004) BINARY.
          05 PI-DRVB1143-FIBUZ-KONTO        PIC  X(012).
      *
       01 PO-DRVB1143.
          05 PO-DRVB1143-RC                 PIC  X(003).
             88 PO-DRVB1143-RC-OK                VALUE 'OK '.
             88 PO-DRVB1143-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1143-DATEN.
             10 PO-DRVB1143-KTY-LEISTUNGSART     PIC S9(004) BINARY.
             10 PO-DRVB1143-KNR-LEISTUNGSART     PIC S9(004) BINARY.
             10 PO-DRVB1143-FIBUZ-STAB-RELEVAN   PIC X(001).
             10 PO-DRVB1143-FIBUZ-UST-STATUS     PIC X(001).
             10 PO-DRVB1143-FIBUZ-ZUGEH-KONTO    PIC X(012).
             10 PO-DRVB1143-FIBUZ-INFO           PIC X(030).
      *
      ***** ENDE COPY-MEMBER DRVB1143 *****************************
      *
 

