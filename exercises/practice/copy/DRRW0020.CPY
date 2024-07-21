      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0020.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRRW0020.CPv  $
      *
      *   Rev 2.3   Jul 22 2016 11:13:36   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:47:42   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004
      *
      *   Rev 2.1   May 31 2016 14:38:14   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004
      *
      *   Rev 2.0   May 19 2016 11:52:52   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0020                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPRW0004                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    19.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!19.05.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *****************************************************************
 
       01  PI-UPRW0004.
           05  PI-UPRW0004-FUNKTION            PIC  X(008).
               88 PI-UPRW0004-DRRW0021              VALUE 'DRRW0021'.
      ***  Steuerinfos
           05  PI-UPRW0004-STEUER-INFO         PIC  X(001).
               88 PI-UPRW0004-ERSTAUFRUF            VALUE '1'.
               88 PI-UPRW0004-FOLGAUFRUF            VALUE '2'.
               88 PI-UPRW0004-ENDEAUFRUF            VALUE '3'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRRW0020   --------------------------*
      *---------------------------------------------------------------*
 
 

