      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2287.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2287.CPv  $
      *
      *   Rev 2.6   Aug 30 2011 12:48:46   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.5   May 23 2011 11:27:24   t13ex15
      *FKI01344: Santander PGM und Copys Namensanpassungen u. Neu-C
      *opy
      *
      *   Rev 2.4   Feb 10 2011 09:45:02   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.3   Aug 18 2010 07:28:16   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.2   Aug 13 2010 08:51:46   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.1   Aug 13 2010 08:30:00   t13ex15
      *AKI00948: Erweiterung Datenspiegel G66 Programme neu/ändern
      *wegen neuem DTA WKUNDEN
      *
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für zusätzliche Kontoinhaber von G66          *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00948!14.06.10! Neuerstellung                    !SWZ        *
      *--------!-------------------------------------------!-----------*
      *FKI01344!20.05.11! Name Neutral  (DB-... raus)      !SWZ        *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
200511*01  DRKI2287.
       01  'XX'-SATZ.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC X(002).
      *            SATZART = '01'
               15  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'SEB BANK'
               15  'XX'-V-DATUM         PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            Tägliche Lieferung'KUNDENTW'
      *            Komplettbestand   'KUNDENKW'

               15  'XX'-V-WKZ           PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
               15  FILLER             PIC X(267).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-HAUPTSATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                 PIC  X(002).
      *            SATZART        = '03'
               15  'XX'-GES                PIC  9(002).
      *            GESELLSCHAFT   = '66'
200511         15  'XX'-VNR                PIC  9(15).
200511*        15  FILLER REDEFINES 'XX'-VNR.
      *            20 F                  PIC  X(004).
      *            0000
      *            20 FILIALNUMMER       PIC  9(004).
      *            erste 4 Stellen BLZ
      *            20 KUNDENNUMMER       PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (STAMMNUMMER)
               15  'XX'-PERSON-LFDNR       PIC  9(002).
      *            ab '02' für zusätzliche Kontoinhaber
               15  'XX'-NNAME              PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               15  'XX'-VNAME              PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               15  'XX'-GEBDAT             PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               15  'XX'-GESCHLECHT         PIC  X(001).
               15  'XX'-ANREDE             PIC  X(001).
      *                ANREDE...........*1 : HERRN
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*9 : Keine Angabe
               15  'XX'-STR-HAUSNR         PIC  X(028).
      *                STRASSE + HAUSNR
               15  'XX'-PLZ                PIC  X(009).
      *                POSTLEITZAHL
               15  'XX'-ORT                PIC  X(028).
      *                ORT
               15  'XX'-AUSL-KZ            PIC  X(002).
      *                AUSLANDSKENNZEICHEN
               15  'XX'-DEL-KZ             PIC  X(001).
      *              Kennz. = D, weiterer Kontoinhaber gelöscht

               15  'XX'-RESERVE            PIC  X(122).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(002).
      *            SATZART             '99'
               15  'XX'-N-ANZ              PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                PIC  X(291).
      *            RESERVE

