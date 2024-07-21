      * P13SP99   Stand: 19/09/98    16:30:58
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0534.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0534.CPv  $
      *
      *   Rev 2.8   Jun 12 2017 18:04:26   t13ex27
      *AKI01519: Bereinigung OZP für Tabelle KUNDENUEBERTRAGUNG
      *
      *   Rev 2.7   Jan 31 2017 13:17:34   t13ex78
      *AKI01481: Korrektur ZTKI0077
      *
      *   Rev 2.6   Jan 06 2017 09:47:50   t13ex78
      *AKI01481: OZPs und Funktionen
      *
      *   Rev 2.5   Dec 15 2016 12:12:48   t13ex78
      *AKI01481: OZPs und Funktionen
      *
      *   Rev 2.4   Nov 22 2015 13:10:04   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect.
      *
      *   Rev 2.3   Oct 28 2015 19:31:14   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.2   Oct 28 2015 09:11:32   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.1   Apr 16 1999 12:40:04   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:46:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0534                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0534                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.05.2017 / 10:10:26 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI0534
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0534.
           05  PI-DRKI0534-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0534-KDULAUFENDENUMMER   PIC S9(4) COMP.
             10 PI-DRKI0534-VERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI0534-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI0534-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0534-DTAERLEDIGTKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0534-NI1.
           05 PI-DRKI0534-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI0534-NI7.
           05 PI-DRKI0534-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI0534.
           05  PO-DRKI0534-RC                     PIC  X(003).
               88 PO-DRKI0534-RC-OK               VALUE 'OK '.
               88 PO-DRKI0534-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

