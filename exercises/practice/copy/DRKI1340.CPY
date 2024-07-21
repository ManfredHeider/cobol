      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1340.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1340.CPv  $
      *
      *   Rev 2.5   Mar 09 2016 16:19:14   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.4   Mar 01 2016 14:32:10   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.3   Feb 16 2016 14:09:36   t13ex73
      *AKI01419: Copystrecken Erweiterung
      *
      *   Rev 2.2   Oct 13 2004 14:08:12   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.1   Oct 24 2002 18:49:00   T13EX39
      *AKI00301 DEUTSCHE BANK DEPOTTRANSAKTIONEN
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTTRANSAKTION KOMPLETTBESTAND DB24       *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    Länge = 150 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!        !01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04!02  ! DB-STUECKE-ODER-WERT      !W.Hornung *
      *         !        !    ! jetzt mit 4 Nachkommastel.!          *
      * -------------------------------------------------------------*
      * AKI01419!12.02.16!03  ! ZASZ-BETRAG erweitern     !DDY       *
      *         !        !    !Umbau auf Replacing by XX  !          *
      *         !        !    !- ohne Vermerk in AREA A - !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1340.
           10  'XX'-SATZ.
      *
               20 'XX'-SA                PIC  X(001).
      *           SATZART        = '6'
               20 'XX'-GES               PIC  9(002).
      *           GESELLSCHAFT   = '48'
               20  'XX'-VNR.
                   25 'XX'-FILIAL-NR      PIC 9(03).
                   25 'XX'-KDNR           PIC 9(07).
                   25 'XX'-UNTER-DEP-NR   PIC 9(02).
                   25 'XX'-WKZ            PIC 9(03).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  'XX'-WP-KENN-NR        PIC  X(06).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-WP-KENN-ISIN      PIC  X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  'XX'-ART               PIC  9(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  'XX'-STUECKE-ODER-WERT PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  'XX'-STUECKE-ODER-WERT-R
                   REDEFINES 'XX'-STUECKE-ODER-WERT PIC X(16).
               20  'XX'-ORG-WKZ           PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  'XX'-BUCH-BETRAG       PIC  9(10)V99.
      *            BETRAG LETZTE BUCHUNG; BRUTTOUMSATZ
               20  'XX'-BUCH-BETRAG-R REDEFINES 'XX'-BUCH-BETRAG
                                          PIC  X(12).
               20  'XX'-BUCH-DAT          PIC  9(08).
      *            BUCHUNGSDATUM; SCHLUSSTAG
               20  'XX'-BUCH-SCHL         PIC  X(02).
      *            BUCHUNGSSCHLÜSSEL; ART DES GESCHÄFTS
      *        20  'XX'-ZASZ-BETRAG       PIC  9(05)V99.
               20  'XX'-ZASZ-BETRAG       PIC  9(09)V99.
      *            BETRAG ZINSABSCHALGSTEUER
      *        20  'XX'-ZASZ-BETRAG-R REDEFINES 'XX'-ZASZ-BETRAG
      *                                   PIC  X(07).
               20  'XX'-ZASZ-BETRAG-R REDEFINES 'XX'-ZASZ-BETRAG
                                          PIC  X(11).
               20  'XX'-STORNO-KZ         PIC  X(01).
      *            STORNO-KENNZEICHEN = 1 FÜR STORNO
      *        20  FILLER                 PIC X(64).
               20  FILLER                 PIC X(60).
      *
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-V-SA          PIC X(001).
      *            SATZART = '1'
               20  'XX'-V-ABSENDER    PIC X(008).
      *            ABSENDER = 'DB AG'
               20  'XX'-V-DATUM       PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER  PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT        PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTTRANSAKTIONENE TÄGLICH           ‘BUCHT'
      *            DEPOTTRANSAKTIONENE KOMPLETT          ‘BUCHK'
               20  'XX'-V-WKZ         PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(118).
      *            RESERVE
 
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-N-SA           PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ          PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(142).
      *            RESERVE
 
 

