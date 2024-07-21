      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2516.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2516.CPv  $
      *
      *   Rev 2.1   Sep 21 2011 10:39:18   t13ex27
      *FKI00911: Verträge löschen-LV
      *
      *   Rev 2.0   Jul 14 2011 10:28:32 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2516                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2516                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.07.2011 / 10:28:32 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2516
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2516.
           05  PI-DRKI2516-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2516-LTSVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2516.
           05  PO-DRKI2516-RC                     PIC  X(003).
               88 PO-DRKI2516-RC-OK               VALUE 'OK '.
               88 PO-DRKI2516-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2516-DATEN.
             10   PO-DRKI2516-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
