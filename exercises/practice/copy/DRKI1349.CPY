      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1349.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1349.CPv  $
      *
      *   Rev 2.4   Jan 30 2019 11:34:36   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.3   Aug 23 2004 12:22:14   t13ex12
      *AKI00430: Löschen von weiteren Kontoinh.
      *
      *   Rev 2.2   Feb 25 2003 15:47:24   t13ex12
      *AKI00328 'XX'-Kunden ohne Anlagedepot
      *
      *   Rev 2.1   Jul 30 2002 16:48:38   t13ex12
      *AKI00274 Einführung 'XX'24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:22:30   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für zusätzliche Kontoinhaber von 'XX'24          
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!16.03.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      *AKI00328!08.01.03! Neues Kennz. für Kunden ohne     !v.d.Goltz  *
      *        !        ! Depot.                           !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI00430!06.08.04! Neues Kennz. für Löschungen      !v.d.Goltz  *
      *        !        ! doch verarbeiten.                !           *
      * ---------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI1349.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC X(001).
      *            SATZART = '2'
               15  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = ''XX' AG'
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
               15  FILLER             PIC X(268).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                 PIC  X(001).
      *            SATZART        = '2'
               15  'XX'-GES                PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-VNR.
                   20 FILIALNUMMER       PIC  9(003).
                   20 KUNDENNUMMER       PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
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
      *                ANREDE...........*3 : FRAEULEIN ??
      *                ANREDE...........*4 : HERRN und FRAU ??
      *                ANREDE...........*5 : HERREN ??
      *                ANREDE...........*6:  AN ??
               15  'XX'-STR                PIC  X(028).
      *                STRASSE
               15  'XX'-HAUSNR             PIC  X(009).
      *                Haus-Nummer
               15  'XX'-POSTFACH           PIC  X(009).
      *                POSTFACH
               15  'XX'-PLZ                PIC  X(009).
      *                POSTLEITZAHL
               15  'XX'-ORT                PIC  X(028).
      *                ORT
               15  'XX'-AUSL-KZ            PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               15  'XX'-VORWAHL-TEL        PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
               15  'XX'-TEL                PIC  9(009).

               15  'XX'-DEPOT-AKTIV        PIC  9(001).
...   *              Kennz. = 1, Kunde hat aktives Depot
...   *              Kennz. = 0, Kunde hat kein Depot

               15  'XX'-DEL-KZ             PIC  X(001).
      *              Kennz. = D, weiterer Kontoinhaber gelöscht

               15  'XX'-RESERVE            PIC  X(102).
      *        15  'XX'-RESERVE            PIC  X(103).
      *        15  'XX'-RESERVE            PIC  X(104).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(001).
      *            SATZART             'E'
               15  'XX'-N-ANZ              PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                PIC  X(292).
      *            RESERVE


