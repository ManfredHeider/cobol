      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3009.CPv  $
      *
      *   Rev 2.1   Apr 29 2015 09:05:12   t13ex15
      *AKI01337: Änderung Sonderleistungen 2015 3 neue Felder
      *
      *   Rev 2.0   Apr 27 2015 14:23:04   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3009                                    *
      *                                                              *
      *    FUNKTION: COPY FUER AUSGABESATZ SIEHE UNTEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: S. WARZ                                         *
      *    DATUM:    27.04.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01337!27.04.15! 01 !NEU                        ! SWZ      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * COBOLCOPY FÜR DIE AUSGABESATZART DER AS-CLUB-VERARBEITUNG
      * --> auf Vertragsnummer-Ebene
      *   (MONATSLAUF) -> STANDARDCOPY
      *     SATZLAENGE: 009 STELLEN
      *     FORMAT:     F
      *---------------------------------------------------------------*

      *    KUNDENNUMMER
           05 'XX'-KDNR             PIC  S9(9) COMP-4.

240415     05 FILLER                PIC  X(01).

      *    Mandant
240415     05 'XX'-MANDANT          PIC  S9(9) COMP-4.





