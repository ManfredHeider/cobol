      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2692.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2692.CPv  $
      *
      *   Rev 2.2   Nov 12 2013 12:03:08   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   Oct 17 2012 16:11:00   t13ex27
      *AKI01135: Kundendaten physisch löschen-Nacharbeiten
      *
      *   Rev 2.0   Sep 21 2012 11:23:16 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2692                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2692                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 15:30:30 / T13EX52
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
      *    FÜR DIE FUNKTION DRKI2692
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2692.
           05  PI-DRKI2692-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2692-KFZTARVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2692.
           05  PO-DRKI2692-RC                     PIC  X(003).
               88 PO-DRKI2692-RC-OK               VALUE 'OK '.
               88 PO-DRKI2692-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2692-DATEN.
             10   PO-DRKI2692-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
