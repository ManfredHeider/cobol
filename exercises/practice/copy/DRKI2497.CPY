      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2497.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2497.CPv  $
      *
      *   Rev 2.2   Mar 27 2017 09:29:16   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.1   Jul 11 2011 16:17:00   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 09 2011 14:52:46   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    DB-Änderungsbestand DWS-Zulagen                             *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *Auftrag ! Datum  ! Text                             ! Name      *
      *--------!-------------------------------------------!-----------*
      *AKI01040!08.06.11! Neuerstellung                    ! MDI       *
      *----------------------------------------------------------------*
      *fKI01544!24.03.17! FVD entfernt                     ! HHU       *
      * ---------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI2497.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  DWS-VORSATZ.
      *
               15  DWS-V-SA            PIC X(002).
      *            SATZART = '00'
               15  DWS-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DWS'
               15  DWS-V-DATUM         PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  DWS-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG', 'ALLF'
               15  DWS-V-TEXT          PIC X(008).
      *            tägliche Lieferung'ZULAGET'
      *            Komplettbestand   'ZULAGEK'
 
               15  DWS-V-WKZ           PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
               15  FILLER              PIC X(067).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  DWS-SATZ     REDEFINES     DWS-VORSATZ.
      *
               15  DWS-SA                 PIC  X(002).
      *            Satzart = '20'
               15  DWS-GES                PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  DWS-KEY.
                   20  DWS-VNR.
                       25 FILIALNUMMER        PIC  9(003).
                       25 KUNDENNUMMER        PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   20  DWS-VNR-ORDNUNGSNR     PIC  9(002).
      *            Ordnungskennzeichen für interne DVAG-Verarbeitung:
      *            laufende Nummer beginnend mit 00.
                   20  DWS-PORTFOLIONR        PIC  X(010).
      *            Portfolio-Nummer
                   20  DWS-BEITRJAHR          PIC  9(004).
      *            Beitragsjahr (JHJJ)
 
               15  DWS-ZULAGEN-BEITRJAHR  PIC  9(011)V9(002).
      *            Zulagen für Beitragsjahr
 
               15  DWS-DEL-KZ             PIC  X(001).
      *            D = Satz löschen
      *            S = Datenschutzsperre aktiviert
 
               15  DWS-RESERVE            PIC  X(056).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  DWS-NACHSATZ    REDEFINES  DWS-VORSATZ.
      *
               15  DWS-N-SA               PIC  X(002).
      *            SATZART = '99'
               15  DWS-N-ANZ              PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                 PIC  X(091).
      *            RESERVE
 

