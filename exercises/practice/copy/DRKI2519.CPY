      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2519.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2519.CPv  $
      *
      *   Rev 2.1   Sep 21 2011 10:39:38   t13ex27
      *FKI00911: Verträge löschen-LV
      *
      *   Rev 2.0   Sep 20 2011 13:53:08 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2519                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2519                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.09.2011 / 13:53:08 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2519
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2519.
           05  PI-DRKI2519-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2519-LPEVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2519.
           05  PO-DRKI2519-RC                     PIC  X(003).
               88 PO-DRKI2519-RC-OK               VALUE 'OK '.
               88 PO-DRKI2519-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2519-DATEN.
             10   PO-DRKI2519-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
