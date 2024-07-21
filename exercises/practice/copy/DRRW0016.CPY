      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0016.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRRW0016.CPv  $
      *
      *   Rev 2.3   Jul 22 2016 11:11:54   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW00
      *05 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:45:38   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW0005
      *
      *   Rev 2.1   May 31 2016 14:36:38   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW0005
      *
      *   Rev 2.0   May 17 2016 09:55:56   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0016                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPRW0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    17.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!17.05.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *****************************************************************
 
       01  PI-UPRW0005.
           05  PI-UPRW0005-FUNKTION            PIC  X(008).
               88 PI-UPRW0005-DRRW0017              VALUE 'DRRW0017'.
      ***  Steuerinfos
           05  PI-UPRW0005-STEUER-INFO         PIC  X(001).
               88 PI-UPRW0005-ERSTAUFRUF            VALUE '1'.
               88 PI-UPRW0005-FOLGAUFRUF            VALUE '2'.
               88 PI-UPRW0005-ENDEAUFRUF            VALUE '3'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRRW0016   --------------------------*
      *---------------------------------------------------------------*
 
 

