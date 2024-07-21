      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2466.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2466.CPv  $
      *
      *   Rev 2.3   Nov 13 2013 12:05:34   t13ex20
      *AOK00054: Vertragsber: Ticketwerte erw. um Interessentendaten
      *
      *   Rev 2.2   Nov 11 2013 11:57:44   t13ex20
      *AOK00054: Vertragsber: Ticketwerte erw. um Interessentendaten
      *
      *   Rev 2.1   Sep 20 2012 10:28:32   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   Aug 30 2011 16:19:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Vertragsbereinigung
      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!01.09.11! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2466.
           05  PI-DATEN.

               10  PI-DRKI2466-VERPARTGESELLSCHID    PIC  9(002).
               10  PI-DRKI2466-VERVERTRAGSID         PIC  X(013).
               10  PI-DRKI2466-VERVERTRAGSNUMMER     PIC  9(015).
               10  PI-DRKI2466-VRBPERSONENID-V       PIC  X(013).
               10  PI-DRKI2466-KNDPERSONENID-ALT     PIC  X(013).
               10  PI-DRKI2466-KNDPERSONENID-NEU     PIC  X(013).
               10  PI-DRKI2466-KNDNUMMER-NEU         PIC S9(009).
               10  PI-DRKI2466-KNDSTATUSID-ALT       PIC  X(001).
               10  PI-DRKI2466-KNDSTATUSID-NEU       PIC  X(001).
               10  PI-DRKI2466-KNDNACHNAME           PIC  X(030).
               10  PI-DRKI2466-KNDVORNAME            PIC  X(030).
               10  PI-DRKI2466-KNDANREDE             PIC S9(004) COMP.
               10  PI-DRKI2466-KNDGEBDAT             PIC  X(010).
               10  PI-DRKI2466-KNDGESCHLECHT         PIC  X(001).

               10  PI-DRKI2466-VERMANDANTNR          PIC S9(009) COMP.
               10  PI-DRKI2466-KNDGEANDERTPGM        PIC  X(008).

       01  PO-DRKI2466.
           05  PO-DRKI2466-RC               PIC  X(003).
               88 PO-DRKI2466-RC-OK              VALUE 'OK '.
               88 PO-DRKI2466-RC-NOK             VALUE 'NOK'.

           05  PO-DATEN.

               10  PO-DRKI2466-KNDPERSONENID          PIC X(013).
               10  PO-DRKI2466-RC-TICKET              PIC S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2466-FEHLERSATZ.
               10 PO-DRKI2466-FUNKTION           PIC  X(10).
               10 PO-DRKI2466-TABELLE            PIC  X(20).
               10 PO-DRKI2466-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2466-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2466-KEY.
                  15 PO-DRKI2466-GES             PIC  9(002).
                  15 PO-DRKI2466-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
