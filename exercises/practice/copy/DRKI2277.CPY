      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2277.CPY  $
 
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2277.CPv  $
      *
      *   Rev 2.8   Jun 28 2016 12:13:20   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.7   Jun 28 2016 11:47:00   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.6   Jun 28 2016 10:40:44   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.5   Apr 28 2016 08:06:06   t13ex78
      *AKI01427: OZP's & DCLGEN's
      *
      *   Rev 2.4   Nov 21 2013 10:13:38   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.3   Nov 12 2013 12:02:56   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.2   Nov 24 2010 11:34:28   t13ae31
      *AKI00995: KFZ dynamische Vertragsanzeige
      *
      *   Rev 2.1   Oct 31 2008 09:39:24   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung; OZPs
      *
      *   Rev 2.0   Oct 27 2008 10:59:22   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2277                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2277                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.06.2016 / 12:10:36 / T13EX78
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2277
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2277.
           05  PI-DRKI2277-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2277-KFZVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2277-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2277-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2277-KFZBEITRANPASSAM   PIC  X(010).
             10 PI-DRKI2277-KFZBEITRGESAMALT   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI2277-KFZBEITRGESAMNEU   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI2277-KFZBERECHNETAM   PIC  X(010).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2277-NI1.
           05 PI-DRKI2277-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2277-NI7.
           05 PI-DRKI2277-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2277.
           05  PO-DRKI2277-RC                     PIC  X(003).
               88 PO-DRKI2277-RC-OK               VALUE 'OK '.
               88 PO-DRKI2277-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

