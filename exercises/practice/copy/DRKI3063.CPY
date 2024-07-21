      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3063.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3063.CPv  $
      *
      *   Rev 2.3   Aug 27 2018 10:17:22   t13ex78
      *AKI01598: Mandantenprüfung und erw. Protokoll
      *
      *   Rev 2.2   Aug 24 2018 13:51:06   t13ex78
      *AKI01598: Mandantenprüfung und erw. Protokoll
      *
      *   Rev 2.1   Apr 28 2016 11:48:08   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UPKI0255
      *
      *   Rev 2.0   Mar 02 2016 17:52:30   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3062                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0255                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: IFR                                              *
      *    DATUM:    02.03.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI01400!02.03.16!2.1 ! NEU                       ! IFR       *
      * --------+--------+----+---------------------------+-----------*
210818* AKI01598!21.08.18!2.2 ! Neue Funktion DRKI3380,wie! IFR       *
210818*         !        !    ! DRKI2015 aber mit Steuer- !           *
210818*         !        !    ! möglichkeit RDTAERLKZ     !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-UPKI0255.
           05  PI-UPKI0255-FUNKTION            PIC  X(008).
               88 PI-UPKI0255-DRKI2015              VALUE 'DRKI2015'.
               88 PI-UPKI0255-DRKI2593              VALUE 'DRKI2593'.
               88 PI-UPKI0255-DRKI3047              VALUE 'DRKI3047'.
               88 PI-UPKI0255-DRKI3048              VALUE 'DRKI3048'.
210818         88 PI-UPKI0255-DRKI3380              VALUE 'DRKI3380'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *
 
 

