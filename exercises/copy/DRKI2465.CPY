      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2465.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2465.CPv  $
      *
      *   Rev 2.3   Sep 25 2015 12:35:22   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.2   Sep 15 2015 15:07:26   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.1   Sep 20 2012 10:28:22   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   Aug 30 2011 16:18:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!01.09.11! 2.1! NEU                       !I. Frenzel*
      * -------------------------------------------------------------*
010915* AKI01378!01.09.15! 2.2! Neue Felder:              ! DDY      *
010915*         !        !    ! Neuer VN / VNWECHSEL FLAG !          *
010915*         !        !    ! Interne Vertragsnummer    !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2465.
           05  PI-DATEN.
 
 
               10  PI-DRKI2465-VERVERTRAGSID          PIC X(013).
      *        PersonenId des alten Kunden
               10  PI-DRKI2465-KNDPERSONENID          PIC X(013).
               10  PI-DRKI2465-ADRESSEID              PIC X(013).
               10  PI-DRKI2465-VRBPERSONENID-V        PIC X(013).
 
               10  PI-DRKI2465-VERPARTGESELLSCHID     PIC  9(002).
               10  PI-DRKI2465-VERVERTRAGSNUMMER      PIC  9(015).
               10  PI-DRKI2465-VERVERTRZUSTANDID      PIC S9(004) COMP.
 
               10  PI-DRKI2465-VERTRAGSBEGINN         PIC  X(010).
               10  PI-DRKI2465-VERTRAGSAENDER         PIC  X(010).
               10  PI-DRKI2465-VERTRAG-NEU-ALT        PIC  9(002).
               10  PI-DRKI2465-VERMANDANTNR           PIC S9(009) COMP.
               10  PI-DRKI2465-TIMESTAMP              PIC  X(026).
010915         10  PI-DRKI2465-VERINTERNEVNR          PIC S9(9) COMP.
 
010915         10  PI-DRKI2465-VN-ART                 PIC  X(003).
010915             88 PI-DRKI2465-VN-NEU                VALUE 'NEU'.
010915             88 PI-DRKI2465-VN-WECHSEL            VALUE 'VNW'.
010915             88 PI-DRKI2465-VN-MEHRALS1           VALUE 'VNM'.
               10  PI-DRKI2465-ROLLE-VERTRAG          PIC  X(001).
               10  PI-DRKI2465-ROLLE-TARIF            PIC  X(001).
               10  PI-DRKI2465-VTRBEZ-ARTKZ           PIC  X(001).
               10  PI-DRKI2465-KPEPERSONLFDNR         PIC S9(004) COMP.
 
      *        Kundendaten des neuen Kunden
               10  PI-DRKI2465-NNAME                  PIC  X(030).
               10  PI-DRKI2465-VNAME                  PIC  X(030).
               10  PI-DRKI2465-GEBDAT                 PIC  X(010).
               10  PI-DRKI2465-ANREDE                 PIC S9(002).
               10  PI-DRKI2465-GESCHL                 PIC  X(001).
               10  PI-DRKI2465-PLZ                    PIC  X(009).
               10  PI-DRKI2465-STR                    PIC  X(040).
               10  PI-DRKI2465-ORT                    PIC  X(040).
               10  PI-DRKI2465-AUSL-KZ                PIC  X(003).
               10  PI-DRKI2465-KNDGEANDERTPGM         PIC  X(008).
 
               10  PI-DRKI2465-INSTITUT-NAME          PIC  X(050).
               10  PI-DRKI2465-BLZ                    PIC  9(009).
               10  PI-DRKI2465-KONTONR                PIC  X(015).
               10  PI-DRKI2465-KONTOART               PIC S9(001).
 
               10  P-AKTUAL-KZ                      PIC  X(001).
 
       01  PO-DRKI2465.
           05  PO-DRKI2465-RC               PIC  X(003).
               88 PO-DRKI2465-RC-OK              VALUE 'OK '.
               88 PO-DRKI2465-RC-NOK             VALUE 'NOK'.
 
           05  PO-DATEN.
               10  PO-DRKI2465-KNDPERSONENID          PIC X(013).
 
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2465-FEHLERSATZ.
               10 PO-DRKI2465-FUNKTION           PIC  X(10).
               10 PO-DRKI2465-TABELLE            PIC  X(20).
               10 PO-DRKI2465-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2465-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2465-KEY.
                  15 PO-DRKI2465-GES             PIC  9(002).
                  15 PO-DRKI2465-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************

