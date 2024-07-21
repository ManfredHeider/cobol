      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2984.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2984.CPv  $
      *
      *   Rev 2.2   Apr 27 2015 10:20:30   t13ex15
      *AKI01337: Änderung Sonderleistungen 2015 3 neue Felder
      *
      *   Rev 2.1   Mar 12 2015 13:32:46   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.0   Feb 17 2015 15:23:10   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2984                                    *
      *                                                              *
      *    FUNKTION: COPY FUER AUSGABESATZ SIEHE UNTEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: S. WARZ                                         *
      *    DATUM:    17.02.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01337!17.02.15! 01 !NEU                        ! SWZ      *
      * -------------------------------------------------------------*
      * AKI01337!24.04.15! 02 !3 neue Felder              ! SWZ      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * COBOLCOPY FÜR DIE AUSGABESATZART DER AS-CLUB-VERARBEITUNG
      * --> auf Vertragsnummer-Ebene
      *   (MONATSLAUF) -> STANDARDCOPY
      *     SATZLAENGE: 110 STELLEN
      *     FORMAT:     F
      *---------------------------------------------------------------*

      *    VERTRAGSNUMMER
           05 'XX'-VERTRAGSNUMMER   PIC  X(15).

      *    KUNDENNUMMER
           05 'XX'-KDNR             PIC  S9(9) COMP-4.

      *    GESELLSCHAFT
           05 'XX'-GES              PIC  S9(4) COMP-5.

      *    Erstellungsdatum JHJJ-MM-TT mit immer dem 15.
           05 'XX'-ERSTELL-DATUM    PIC  X(10).

      *    VBNR
           05 'XX'-VBNR             PIC  S9(9) COMP-4.

      *    Kommissarisch K oder N
           05 'XX'-KOMMISSARISCH    PIC  X(01).

      *    Eigenvermittelt J oder N
           05 'XX'-EIGENVERMITTELT  PIC  X(01).

      *    Potential J oder N
           05 'XX'-POTENTIAL        PIC  X(01).

      *    Anzahl Produkte
           05 'XX'-ANZ-PROD         PIC  S9(9) COMP-4.

      *    Guthabenanteil (Investment)
           05 'XX'-PRAEMIE-INV      PIC  S9(9)V9(2) COMP-3.

      *    Topzinskonto
           05 'XX'-PRAEMIE-TOPZINS
                                    PIC  S9(9)V9(2) COMP-3.
      *    AML-VEP komplett
           05 'XX'-PRAEMIE-AML-VEP  PIC  S9(9)V9(2) COMP-3.

      *    AML-Prämie ohne EZ
           05 'XX'-PRAEMIE-OEZ      PIC  S9(9)V9(2) COMP-3.

      *    AML-Prämie nur EZ
           05 'XX'-PRAEMIE-EZ       PIC  S9(9)V9(2) COMP-3.

      *    Anzahl Produkte mit Volumen 0
240415     05 'XX'-ANZ-PROD-V0      PIC  S9(9) COMP-4.

      *    ID des Vertrags
240415     05 'XX'-VNRID            PIC  X(13).

      *    Mandant
240415     05 'XX'-MANDANT          PIC  S9(9) COMP-4.

      *    ID des Kunden
           05 'XX'-VNID             PIC  X(13).

240415*    05 FILLER                PIC  X(19).
240415     05 FILLER                PIC  X(44).



