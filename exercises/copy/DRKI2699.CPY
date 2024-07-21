      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2699.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2699.CPv  $
      *
      *   Rev 2.1   Nov 22 2012 14:58:48   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.0   Sep 27 2012 18:20:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2699                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2699                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOK00051!24.09.12!2.1 ! neu                       ! I.Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2699
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    Steuersatz für Kundenübertragung
      *
      ****************************************************************
      *
       01  PI-DRKI2699.

           05  PI-DRKI2699-DATEN.

             10 PI-DRKI2699-OTHOSTVERTRAGID       PIC X(13).
             10 PI-DRKI2699-OTTICKETID            PIC S9(018) COMP-3.

       01  PO-DRKI2699.
           05  PO-DRKI2699-RC                     PIC  X(003).
               88 PO-DRKI2699-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2699-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRKI2699-RC-NOK                 VALUE 'NOK'.

           05  PO-DRKI2699-DATEN.

               10  PO-DRKI2699-VERTRAGSDATEN      PIC  X(087).

           05  PO-DRKI2699-NFD-DATEN.
               10  PO-DRKI2699-TABELLE            PIC  X(030).
               10  PO-DRKI2699-TABELLE-ID         PIC  X(013).

      *
      ******* ENDE COPY-MEMBER **************************************
