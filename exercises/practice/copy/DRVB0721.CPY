      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0721.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0721.CPv  $
      *
      *   Rev 2.3   Jun 08 2016 10:39:32   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.2   May 31 2016 07:45:46   t13ex30
      *AVB01400: Projekt Lucy2 - Module für die Views VIVBB026, VIV
      *BB064, VIVBA102. VIVBA100
      *
      *   Rev 2.1   Sep 27 2006 09:29:14   t13ex43
      *AIS00060: PGM - Geldleistungen konfigurierbar machen
      *
      *   Rev 2.0   Jul 02 2004 15:58:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0721                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0100                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.08.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AIS00060!08.08.06!2.1 ! NEU                       ! KLEEMEIER *
300516* AVB01400!30.05.16!2.2 ! Neuer Zugriff DRVB1143    ! H.Wölflik *
070616* AVB01400!07.06.16!2.3 ! Neuer Zugriff DRVB1144    ! H.Wölflik *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0100.
           05  PI-ZWVB0100-FUNKTION            PIC  X(008).
               88 PI-ZWVB0100-DRVB0723              VALUE 'DRVB0723'.
300516         88 PI-ZWVB0100-DRVB1143              VALUE 'DRVB1143'.
070616         88 PI-ZWVB0100-DRVB1144              VALUE 'DRVB1144'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
 

