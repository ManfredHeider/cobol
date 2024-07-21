      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0456.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0456.CPv  $
      *
      *   Rev 2.5   Feb 04 2016 10:33:44   t13ex78
      *AKI01338: Wartungsauftrag für Abgleich TBAT0042 - Sysin Memb
      *er - neue Abfragelogik inkl. neuer Meldungen
      *
      *   Rev 2.4   Jul 25 2014 10:39:32   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE - EDI anders behandeln
      *
      *
      *   Rev 2.3   Jan 07 2014 08:48:00   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe ü
      *ber ABLAUFK
      *ONTROLLE - Programme
      *
      *   Rev 2.2   Jan 07 2014 08:26:48   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.1   Feb 24 2009 13:45:04   t13ex14
      *AKI00787: Maschinelle Info über Aktualisierungsjobs - OZP-Ob
      *jekte
      *
      *   Rev 2.0   Jan 29 2009 11:25:50   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: ZWAT0042                                   *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0042                     *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00787!29.01.09!    ! Neu                       ! WMI      *
      * -------------------------------------------------------------*
      * AKI01259!06.11.13!    ! Aufnahme Funktion DRAT0520! TFL      *
      * -------------------------------------------------------------*
      * AKI01259!25.07.14!    ! Aufnahme Funktion DRAT0559! T.Flierl *
      * -------------------------------------------------------------*
      * -------------------------------------------------------------*
      * AKI01338!03.02.16!    ! Aufnahme Funktion DRAT0605! T.Flierl *
      * -------------------------------------------------------------*
 
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWAT0042.
           05  PI-ZWAT0042-FUNKTION            PIC  X(008).
 
 
             88   PI-ZWAT0042-DRAT0457     VALUE 'DRAT0457'.
             88   PI-ZWAT0042-DRAT0520     VALUE 'DRAT0520'.
250714       88   PI-ZWAT0042-DRAT0559     VALUE 'DRAT0559'.
030216       88   PI-ZWAT0042-DRAT0605     VALUE 'DRAT0605'.
 
      *
      ******* ENDE COPY-MEMBER **************************************

