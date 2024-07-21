      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2697.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2697.CPv  $
      *
      *   Rev 2.1   Nov 22 2012 14:58:34   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.0   Sep 27 2012 10:55:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2697                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2697                    *
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
      *    FÜR DIE FUNKTION drki2697
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    Steuersatz(Cloud) für Vertragsdatenänderung
      *
      ****************************************************************
      *
       01  PI-DRKI2697.

           05  PI-DRKI2697-DATEN.

             10 PI-DRKI2697-VERTRAGSID            PIC  X(0013).
             10 PI-DRKI2697-VERTRAGSID-D          PIC  X(0026).
             10 PI-DRKI2697-CHANGE-TYPE           PIC  X(0001).

       01  PO-DRKI2697.
           05  PO-DRKI2697-RC                     PIC  X(003).
               88 PO-DRKI2697-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2697-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRKI2697-RC-NOK                 VALUE 'NOK'.

           05  PO-DRKI2697-DATEN.

               10  PO-DRKI2697-VERTRAGSDATEN      PIC  X(046).


           05  PO-DRKI2697-NFD-DATEN.
               10  PO-DRKI2697-TABELLE            PIC  X(030).
               10  PO-DRKI2697-TABELLE-ID         PIC  X(013).

      *
      ******* ENDE COPY-MEMBER **************************************
