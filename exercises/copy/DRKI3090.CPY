      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3090.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3090.CPv  $
      *
      *   Rev 2.2   Nov 28 2016 10:08:16   t13ex27
      *AKI01462: Korrespondenz-Makler AML
      *
      *   Rev 2.1   Sep 20 2016 17:31:38   t13ex48
      *AKI01462: Korrespondenz-Makler AML: DTA-Kontrolle neue Datei
      *
      *   Rev 2.0   Sep 20 2016 09:42:52   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER                                                 *
      *        - EINGABE DTA Storno von maklerbetreuten Vertraegen     *
      *                                                                *
      *    SATZLÄNGE 36                                               *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01462!01.09.16!1.00! Neuer DTA                 ! HSE      *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-SATZ.
      *
               20  'XXX'-SA            PIC X(01).
      *            SATZART        = 'C'
               20  'XXX'-VNR           PIC 9(10).
      *            ORDNUNGSBEGRIFF = VERSICHERUNGSNUMMER
               20  'XXX'-ZUST          PIC 9(04).
      *       Vertragszustand bei AML
      *       Findet ein Übergang zu einem der folgenden Zustaende statt,
      *       wird der Vertrag geliefert
      *       0220 Historisch ohne Leistung nach Kündigung
      *       0224 Man. geführter Rentenvertrag nach Künd.
      *       0227 Keine Auszahlung bei Rückkauf
      *       0228 Vereinnahmt nach Kündigung
      *       0229 Ausgezahlt nach Kündigung
      *       0414 Widerruf nach Policierung
      *       0914 Widerruf vor Policierung
      *       8615 Auszahlung/Btfr. nach Kündigung steht an
               20  FILLER              PIC X(021).
      *
      ******************************************************************
      *                                                                *
      *         VORSATZ                                                *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-VORSATZ   REDEFINES 'XXX'-SATZ.
      *
               20  'XXX'-V-SA          PIC X(001).
      *        SATZART = 'A'
               20  'XXX'-V-ABSENDER    PIC X(003).
      *        Absender = 'AML'
               20  'XXX'-V-DATUM       PIC 9(008).
      *        ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  'XXX'-V-EMPFAENGER  PIC X(004).
      *        EMPFAENGER = 'DVAG'
               20  'XXX'-V-TEXT        PIC X(020).
      *        AENDERUNGSBESTAND = 'MAKLER-STORNO       '
      *        KOMPLETTBESTAND ist nicht vorgesehen
      *
      ******************************************************************
      *                                                                *
      *         NACHSATZ                                               *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-NACHSATZ  REDEFINES 'XXX'-SATZ.
      *
               20  'XXX'-N-SA          PIC X(001).
      *        SATZART = 'E'
               20  'XXX'-N-ANZ         PIC 9(009).
      *        ANZAHL SAETZE INKL. VOR- UND NACHSATZ
               20  FILLER              PIC X(026).
 


