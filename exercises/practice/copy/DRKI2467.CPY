      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2467.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2467.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:28:42   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   Aug 30 2011 16:19:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Kundenzusammenführung
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
       01  PI-DRKI2467.
           05  PI-DATEN.

               10  PI-DRKI2467-VERPARTGESELLSCHID    PIC  9(002).
               10  PI-DRKI2467-VERVERTRAGSID         PIC  X(013).
               10  PI-DRKI2467-VERVERTRAGSNUMMER     PIC  9(015).
               10  PI-DRKI2467-ADRESSEID             PIC  X(013).
               10  PI-DRKI2467-VRBPERSONENID-V       PIC  X(013).
               10  PI-DRKI2467-KNDPERSONENID-ALT     PIC  X(013).
               10  PI-DRKI2467-KNDPERSONENID-NEU     PIC  X(013).
               10  PI-DRKI2467-KNDSTATUSID-ALT       PIC  X(001).
               10  PI-DRKI2467-KNDSTATUSID-NEU       PIC  X(001).

               10  PI-DRKI2467-VERMANDANTNR          PIC S9(009) COMP.
               10  PI-DRKI2467-KNDGEANDERTPGM        PIC  X(008).

               10  PI-DRKI2467-VRBGUELTIGKEITSDAT    PIC  X(010).
               10  PI-DRKI2467-VRBABLAUFDATUM        PIC  X(010).

       01  PO-DRKI2467.
           05  PO-DRKI2467-RC                        PIC  X(003).
               88 PO-DRKI2467-RC-OK                       VALUE 'OK '.
               88 PO-DRKI2467-RC-NOK                      VALUE 'NOK'.

           05  PO-DRKI2467-RC-TICKET                 PIC  S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2467-FEHLERSATZ.
               10 PO-DRKI2467-FUNKTION               PIC  X(10).
               10 PO-DRKI2467-TABELLE                PIC  X(20).
               10 PO-DRKI2467-SQL-FEHLER             PIC  X(04).
               10 PO-DRKI2467-KEY                    PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2467-KEY.
                  15 PO-DRKI2467-GES                 PIC  9(002).
                  15 PO-DRKI2467-VNR                 PIC  X(015).
