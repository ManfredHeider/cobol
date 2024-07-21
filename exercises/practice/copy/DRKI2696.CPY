      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2696.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2696.CPv  $
      *
      *   Rev 2.2   Mar 27 2014 16:48:44   t13ex27
      *AKI01253: Aktualiserung der Vertragsübersicht
      *
      *   Rev 2.1   Nov 22 2012 14:58:26   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.0   Sep 24 2012 14:43:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2696                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2696                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOK00051!24.09.12!2.1 ! neu                       ! I.Frenzel*
      * AKI01253!11.02.14!2.2 ! Korrektur Cloud-VN-Wechsel! I.Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2696
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    Steuersatz(Cloud) für VN-Wechsel
      *
      ****************************************************************
      *
       01  PI-DRKI2696.

           05  PI-DRKI2696-DATEN.

             10 PI-DRKI2696-HNAKUNDENID           PIC  X(0013).
             10 PI-DRKI2696-HNAPARTGESELLSCHID    PIC S9(4) COMP.
             10 PI-DRKI2696-HNAVERTRAGSNUMMER     PIC  X(0015).
             10 PI-DRKI2696-HNAKUNDENID-Z         PIC  X(0013).

       01  PO-DRKI2696.
           05  PO-DRKI2696-RC                     PIC  X(003).
               88 PO-DRKI2696-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2696-RC-NOT-FOUND           VALUE 'NFD'.
               88 PO-DRKI2696-RC-NOK                 VALUE 'NOK'.

           05  PO-DRKI2696-DATEN.

               10  PO-DRKI2696-VN-WECHSEL         PIC  X(105).


           05  PO-DRKI2696-NFD-DATEN.
               10  PO-DRKI2696-TABELLE            PIC  X(030).
               10  PO-DRKI2696-TABELLE-ID         PIC  X(013).

      *
      ******* ENDE COPY-MEMBER **************************************
