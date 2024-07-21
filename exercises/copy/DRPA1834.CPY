      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1834.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1834.CPv  $
      *
      *   Rev 2.2   Oct 19 2015 10:07:36   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      * PA04
      *
      *   Rev 2.1   Aug 10 2015 08:12:52   T13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Jul 28 2015 08:38:08   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1834                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!28.07.15! 2.0! NEU                       ! STRAUSS   *
      *         !        ! 2.1! WEITERE NEUE FUNKTION     ! STORCH    *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZWPA0035.
           05  PI-ZWPA0035-FUNKTION            PIC  X(008).
               88 PI-ZWPA0035-DRPA1835-SEL          VALUE 'DRPA1835'.
               88 PI-ZWPA0035-DRPA1839-SEL-2        VALUE 'DRPA1839'.
      *
      ******* ENDE COPY-MEMBER ****************************************



