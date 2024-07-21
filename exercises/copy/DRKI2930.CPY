      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2930.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2930.CPv  $
      *
      *   Rev 2.2   Mar 02 2015 16:31:36   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen
      *
      *   Rev 2.1   Sep 01 2014 10:42:12   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen
      *
      *   Rev 2.0   Jul 17 2014 14:32:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01315!16.07.14!2.1 ! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2930.
 
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
               20  AML-PALETTENNAME     PIC X(08).
      *            Fondsbezeichnung
               20  AML-FONDSKUERZEL     PIC X(02).
      *            Fondskürzel
               20  FILLER               PIC X(14).
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
               20  AML-V-TEXT           PIC X(013).
      *            KOMPLETTBESTAND  = 'FONDSPALETTEN'
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

