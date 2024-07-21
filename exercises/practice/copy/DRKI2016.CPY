      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2016.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2016.CPv  $
      *
      *   Rev 2.2   Mar 02 2018 21:17:18   t13ex27
      *AKI01574: Tuning für Frischer Wind
      *
      *   Rev 2.1   Feb 27 2007 12:52:54   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   Feb 05 2007 14:52:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2016 FUNKTION:VERTRAGSDATEN ERMITTELN =  *
      *                                   LIST                       *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!05.02.07! 01 !NEU                        !I. Frenzel*
020317* AKI01574!02.03.17!2.2 ! Vertragsnummer entfernt   !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI2016                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VERTRAGSDATENERMITTELN LIST                     *
      *              - ALLE VERTRAEGE EINES VNS ERMITTELN            *
      ****************************************************************
       01 PI-DRKI2016.
           05  PI-DRKI2016-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI2016-DATEN.
             10  PI-DRKI2016-VRBPERSONENID        PIC  X(013).

      ***
       01 PO-DRKI2016.
          05 PO-DRKI2016-RC                     PIC  X(003).
             88 PO-DRKI2016-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2016-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2016-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI2016-DATEN.
             10 PO-DRKI2016-VRBPERSONENID         PIC  X(013).
             10 PO-DRKI2016-VERVERTRAGSID         PIC  X(013).
020317*      10 PO-DRKI2016-VERVERTRAGSNUMMER     PIC  X(015).
             10 PO-DRKI2016-VERVERTRZUSTANDID     PIC S9(004) COMP-4.
      *
      ******** Ende Copy-Member **************************************

