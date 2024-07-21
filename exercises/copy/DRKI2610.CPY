      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2610.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2610.CPv  $
      *
      *   Rev 2.1   Jul 20 2012 10:07:04   t13ex27
      *AKI01135: Kundendaten physisch löschen-UP für Vertragsdaten
      *
      *   Rev 2.0   May 09 2012 15:22:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2610                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2610                    *
      *                                                               *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!13.09.07!2.1 ! neu                       !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2610
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2610.
           05  PI-DRKI2610-KEY.
               10   PI-DRKI2610-VRBPERSONENROLLEKZ   PIC  X(001).
               10   PI-DRKI2610-VRBVERTRAGSID        PIC  X(013).
      *
       01  PO-DRKI2610.
           05  PO-DRKI2610-RC                 PIC  X(003).
               88 PO-DRKI2610-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2610-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2610-DATEN.
               10   PO-DRKI2610-COUNTER       PIC S9(4) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
