      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2687.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2687.CPv  $
      *
      *   Rev 2.1   Oct 17 2012 16:10:24   t13ex27
      *AKI01135: Kundendaten physisch löschen-Nacharbeiten
      *
      *   Rev 2.0   Sep 19 2012 10:18:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2687                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2687                    *
      *    --------------------------------------------------------   *
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
      *    FÜR DIE FUNKTION DRKI2687
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2687.
           05  PI-DRKI2687-KEY.

             10 PI-DRKI2687-VERTRAGSID   PIC  X(0013).

      *
       01  PO-DRKI2687.
           05  PO-DRKI2687-RC                     PIC  X(003).
               88 PO-DRKI2687-RC-OK               VALUE 'OK '.
               88 PO-DRKI2687-RC-NOK              VALUE 'NOK'.

           05  PO-DRKI2687-DATEN.

             10   PO-DRKI2687-COUNTER-338       PIC S9(8) COMP.
             10   PO-DRKI2687-COUNTER-440       PIC S9(8) COMP.
             10   PO-DRKI2687-COUNTER-441       PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
