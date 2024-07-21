      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2691.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2691.CPv  $
      *
      *   Rev 2.2   Dec 02 2013 17:35:36   t13ex27
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen-Löschen Verträge
      *
      *   Rev 2.1   Oct 17 2012 16:10:52   t13ex27
      *AKI01135: Kundendaten physisch löschen-Nacharbeiten
      *
      *   Rev 2.0   Sep 20 2012 17:34:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2691                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2691                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI01434!12.11.13!2.2 ! Ablösung Objektid         !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2691
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2691.
           05  PI-DRKI2691-KEY.

             10 PI-DRKI2691-VERTRAGSID            PIC  X(0013).

      *
       01  PO-DRKI2691.
           05  PO-DRKI2691-RC                     PIC  X(003).
               88 PO-DRKI2691-RC-OK               VALUE 'OK '.
               88 PO-DRKI2691-RC-NOK              VALUE 'NOK'.

           05  PO-DRKI2691-DATEN.

             10   PO-DRKI2691-COUNTER-327       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-328       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-329       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-330       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-331       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-403       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-442       PIC S9(8) COMP.
             10   PO-DRKI2691-COUNTER-346       PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
