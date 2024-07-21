      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.06.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1144.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1144.CPv  $
      *
      *   Rev 2.1   Jun 08 2016 10:39:36   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.0   Jun 06 2016 16:03:38   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1144                                      *
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
      * AVB01400!07.06.16!2.1 ! NEU                       ! H.Wölflik *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB1144                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1144.
          05 PI-DRVB1144-KNR-GESELL-TYP       PIC S9(004) BINARY.
          05 PI-DRVB1144-KTY-LEISTUNGSART     PIC S9(004) BINARY.
          05 PI-DRVB1144-KNR-LEISTUNGSART     PIC S9(004) BINARY.
          05 PI-DRVB1144-FIBUZ-STAB-RELEVAN   PIC X(001).
          05 PI-DRVB1144-FIBUZ-UST-STATUS     PIC X(001).
      *
       01 PO-DRVB1144.
          05 PO-DRVB1144-RC                   PIC  X(003).
             88 PO-DRVB1144-RC-OK                  VALUE 'OK '.
             88 PO-DRVB1144-RC-NOT-FOUND           VALUE 'NFD'.
             88 PO-DRVB1144-RC-MULT-ROWS           VALUE 'MUR'.
          05 PO-DRVB1144-DATEN.
             10 PO-DRVB1144-FIBUZ-KONTO       PIC  X(012).
             10 PO-DRVB1144-FIBUZ-ZUGEH-KONTO PIC  X(012).
             10 PO-DRVB1144-FIBUZ-INFO        PIC  X(030).
      *
      ***** ENDE COPY-MEMBER DRVB1144 *****************************
      *
 
 

