      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2606.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2606.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:18:02   t13ex27
      *AKI01135: Kundendaten physisch löschen-Ticket-HP
      *
      *   Rev 2.0   May 08 2012 10:03:10   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2606                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2606                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01135!08.05.12! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2606
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2606.
           05  PI-DRKI2606-KEY.
             10 PI-DRKI2606-NAME                 PIC  X(030).

       01  PO-DRKI2606.
      *    Daten
           05  PO-DRKI2606-NAME                  PIC  X(030).
           05  PO-DRKI2606-CREATOR               PIC  X(006).

      *    Returncode
           05  PO-DRKI2606-RC                    PIC  X(003).
               88 PO-DRKI2606-RC-OK              VALUE 'OK '.
               88 PO-DRKI2606-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2606-RC-NOK             VALUE 'NOK'.

      ******* ENDE COPY-MEMBER **************************************
