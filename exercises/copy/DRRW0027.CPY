      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRRW0027.CPv  $
      *
      *   Rev 2.1   Jul 19 2016 12:09:54   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Korrektur STAB UPRW0006/UPRW0
      *007 (HWO)
      *
      *   Rev 2.0   Jul 14 2016 11:56:20   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0026                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPRW0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H.WÖLFLIK                                        *
      *    DATUM:    14.07.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!14.07.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *****************************************************************
 
       01  PI-UPRW0007.
           05  PI-UPRW0007-FUNKTION            PIC  X(008).
               88 PI-UPRW0007-DRRW0026              VALUE 'DRRW0026'.
      ***  Steuerinfos
           05  PI-UPRW0007-STEUER-INFO         PIC  X(001).
               88 PI-UPRW0007-ERSTAUFRUF            VALUE '1'.
               88 PI-UPRW0007-FOLGAUFRUF            VALUE '2'.
               88 PI-UPRW0007-ENDEAUFRUF            VALUE '3'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRRW0027   --------------------------*
      *---------------------------------------------------------------*
 
 

