      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2598.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2598.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:06:36   t13ex27
      *AKI01135: Kundendaten physisch löschen-UP für Vertragsdaten
      *
      *   Rev 2.0   Apr 25 2012 11:47:58   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2598                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2598                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01135!26.04.12!2.1 ! neu                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2598
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2598.
           05  PI-DRKI2598-DATEN.

             10 PI-DRKI2598-VERVERTRAGSID       PIC  X(013).
             10 PI-DRKI2598-VERPARTGESELLSCHID  PIC S9(004) COMP.
             10 PI-DRKI2598-VERVERTRAGSNUMMER   PIC  X(015).
             10 PI-DRKI2598-PERSONENID          PIC  X(013).
             10 PI-DRKI2598-TAGESDATUM-EUR      PIC  X(010).
      *
       01  PO-DRKI2598.
           05  PO-DRKI2598-DATEN.
             10 PO-DRKI2598-ANZ-PERS-ROLL-LOE        PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-ADR-UPD              PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-VRB-FEHL             PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-GELVERT-LOE          PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-PERS-LOE-BZ          PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-PERS-LOE-VP          PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-VESOBJEKT-DEL        PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-VRB-DEL-BZ           PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-327                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-328                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-329                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-330                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-331                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-403                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-442                  PIC S9(8) COMP.
             10 PO-DRKI2598-ANZ-346                  PIC S9(8) COMP.

           05  PO-DRKI2598-RC                     PIC  X(003).
               88 PO-DRKI2598-RC-OK               VALUE 'OK '.
               88 PO-DRKI2598-RC-UNV              VALUE 'UNV'.
               88 PO-DRKI2598-RC-NOT-OK           VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************
