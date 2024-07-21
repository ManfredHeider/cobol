      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0018.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRRW0018.CPv  $
      *
      *   Rev 2.3   Jul 22 2016 11:12:44   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:46:50   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003
      *
      *   Rev 2.1   May 31 2016 14:37:26   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003
      *
      *   Rev 2.0   May 19 2016 11:49:34   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0018                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPRW0003                        *
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
 
       01  PI-UPRW0003.
           05  PI-UPRW0003-FUNKTION            PIC  X(008).
               88 PI-UPRW0003-DRRW0019              VALUE 'DRRW0019'.
      ***  Steuerinfos
           05  PI-UPRW0003-STEUER-INFO         PIC  X(001).
               88 PI-UPRW0003-ERSTAUFRUF            VALUE '1'.
               88 PI-UPRW0003-FOLGAUFRUF            VALUE '2'.
               88 PI-UPRW0003-ENDEAUFRUF            VALUE '3'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRRW0018   --------------------------*
      *---------------------------------------------------------------*
 
 

