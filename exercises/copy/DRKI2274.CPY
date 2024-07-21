      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2274.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2274.CPv  $
      *
      *   Rev 2.1   Nov 19 2008 13:56:36   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz zukünftiger Beitrag
      *
      *   Rev 2.0   Oct 09 2008 09:59:28   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *      Copymember DRKI2274
      *                 AMV KFZ zukünftige Beiträge DTA
      *                 Satzlänge 100
      *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00731!09.10.08!    ! Neu                       ! WMI       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************

       01  DRKI2274.

           05  AMV-ZKB-VORSATZ.
      *
               10  AMV-ZKB-V-SA                  PIC  X(001).
      *            SATZART = 'A'
               10  AMV-ZKB-V-ABSENDER            PIC  X(003).
      *            ABSENDER = 'AMV'
               10  AMV-ZKB-V-DATUM               PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               10  AMV-ZKB-V-EMPFAENGER          PIC  X(004).
      *            EMPFAENGER = 'DVAG'
               10  AMV-ZKB-V-TEXT                PIC  X(026).
      *            INHALTSANGABE = 'zukuenftige KFZ-BTG-Felder'
               10  FILLER                        PIC  X(058).
      *            RESERVE

           05  AMV-ZKB-SATZ REDEFINES AMV-ZKB-VORSATZ.

               10  AMV-ZKB-VNR                   PIC  9(014).
               10  AMV-ZKB-BEITRA-ANPAS-AM       PIC  X(010).
               10  AMV-ZKB-BERECHNET-AM          PIC  X(010).
               10  AMV-ZKB-BEITR-GESAMT-NEU      PIC S9(005)V99.
               10  AMV-ZKB-BEITR-GESAMT-ALT      PIC S9(005)V99.
               10  AMV-ZKB-BEITR-TAB             OCCURS 3.
                   15  AMV-ZKB-SPARTE            PIC  X(006).
                   15  AMV-ZKB-BEITRAG-NEU       PIC S9(005)V99.
                   15  AMV-ZKB-SFJ-ANZAHL-NEU    PIC  9(002).
                   15  AMV-ZKB-SFJ-ANZAHL-ALT    PIC  9(002).
               10  FILLER                        PIC  X(001).
      *            RESERVE

           05  AMV-ZKB-NACHSATZ REDEFINES AMV-ZKB-VORSATZ.
      *
               10  AMV-ZKB-N-SA                  PIC X(001).
      *            SATZART = 'E'
               10  AMV-ZKB-N-SATZANZAHL          PIC 9(009).
      *            SATZANZAHL OHNE VOR- UND NACHSATZ
               10  FILLER                        PIC X(090).
      *            RESERVE
