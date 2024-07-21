      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2225.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2225.CPv  $
      *
      *   Rev 2.3   Jan 17 2008 16:16:58   t13ex14
      *AKI00694: Vorstandsauswertung
      *
      *   Rev 2.2   Jan 17 2008 16:11:14   t13ex14
      *AKI00694: Vorstandsauswertung
      *
      *   Rev 2.1   Jan 15 2008 10:46:44   t13ex14
      *AKI00694: Vorstandsauswertung
      *
      *   Rev 2.0   Jan 10 2008 15:42:54   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AKI00694!11.01.08! 1.0! NEU                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2225.

           05  VB-DATEN-AUSGABE.
      *
               10  VB-NUMMER                     PIC  9(007).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-VORNAME                    PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-NAME                       PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-STUFENBEZEICH-ALT          PIC  X(004).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-STUFENBEZEICH-AKT          PIC  X(004).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-KUENDDAT                   PIC  X(010).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-KUNDENNUMMER               PIC  9(009).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-KUNDENVORNAME              PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-KUNDENNAME                 PIC  X(030).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-GESELLSCHAFT               PIC  9(003).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-VERTRAGSNUMMER             PIC  X(015).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-VTRBEGINNDAT               PIC  X(010).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-VTRSTATUS                  PIC  9(002).
               10  FILLER                        PIC  X(001) VALUE ','.
               10  VB-VTRABLAUFDAT               PIC  X(010).
      ************************* Ende DRKI2225 ************************
