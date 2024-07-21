      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2929.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2929.CPv  $
      *
      *   Rev 2.2   Mar 02 2015 16:31:32   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen
      *
      *   Rev 2.1   Sep 01 2014 10:42:06   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen
      *
      *   Rev 2.0   Jul 17 2014 14:32:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  DRKI2929.
 
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-SATZ.
      *
               20  AML-SA               PIC X(01).
      *            SATZART        = 'C'
               20  AML-FONDSKUERZEL     PIC X(02).
      *            Fondskürzel
               20  AML-FONDSBEZEICHNUNG PIC X(40).
      *            Fondsbezeichnung
               20  AML-WKN              PIC X(06).
      *            Wertpapiernummer
               20  AML-ISIN             PIC X(12).
      *            International Securities Identification Number
               20  AML-GUELTIG-AB       PIC X(10).
      *            Datum, ab wann der Fonds g}ltig ist
               20  AML-GUELTIG-BIS      PIC X(10).
      *            Datum, bis wann der Fonds g}ltig ist
      *
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  AML-VORSATZ      REDEFINES      AML-SATZ.
      *
               20  AML-V-SA             PIC X(001).
      *            SATZART        = 'A'
               20  AML-V-ABSENDER       PIC X(003).
      *            ABSENDER = 'AML'
               20  AML-V-DATUM          PIC 9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AML-V-TEXT           PIC X(020).
      *            KOMPLETTBESTAND  = 'FONDS'
      *
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-NACHSATZ    REDEFINES     AML-SATZ.
      *
               20  AML-N-SA             PIC X(001).
      *            SATZART        = 'E'
               20  AML-N-ANZ            PIC 9(015).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ

