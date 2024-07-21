      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2684.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2684.CPv  $
      *
      *   Rev 2.1   Oct 17 2012 16:10:04   t13ex27
      *AKI01135: Kundendaten physisch löschen-Nacharbeiten
      *
      *   Rev 2.0   Sep 19 2012 13:04:15 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2684                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2684                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.09.2012 / 13:04:15 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2684
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2684.
           05  PI-DRKI2684-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2684-BAJVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2684.
           05  PO-DRKI2684-RC                     PIC  X(003).
               88 PO-DRKI2684-RC-OK               VALUE 'OK '.
               88 PO-DRKI2684-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2684-DATEN.
             10   PO-DRKI2684-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
