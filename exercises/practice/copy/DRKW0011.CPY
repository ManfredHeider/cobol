      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0011.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKW0011.CPv  $
      *
      *   Rev 2.6   Oct 11 2019 10:07:28   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.5   May 06 2019 13:49:46   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.4   Nov 11 2010 11:15:42   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.3   Oct 08 2010 09:01:24   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.2   Sep 28 2010 13:53:06   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:30:12   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.0   Jul 14 2010 14:00:02   t13ex14
      *Archiverstellung
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = DTA                                             *
      *    Länge = 400 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *18.08.10!NEUERSTELLUNG FÜR DVAG WIEN             !SWZ      ! 01 *
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *18.08.10!Probleme mit MFE, keine Änderung        !SWZ      ! 2.1*
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *07.10.10!DVW-BNK geändert in DTA-BNK-            !SWZ      ! 2.2*
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      * AKI01683!24.04.19!2.5 ! Neue Attribute            ! IFR        *
      *         !        !    ! Risikoklasse und Wieder-  !            *
      *         !        !    ! anlagedatum               !            *

       01  DRKW0011.

           05  DTA-BNK-DATEN.

               10 DTA-BNK-SATZART                     PIC  X.
      *           'D'
               10 DTA-BNK-KEY.
                  15 DTA-BNK-GESELLSCHAFT             PIC  9(002).
      *              Produktgeber
                  15 DTA-BNK-VERTRAGSNUMMER           PIC  9(015).
      *              Vertragsnummer
                  15 DTA-BNK-UNTERNUMMER              PIC  9(003).
      *              Unternummer
                  15 DTA-BNK-ISIN                     PIC  X(012).
      *              ISIN

               10 DTA-BNK-PRODSPARTE                  PIC  9(002).
      *            SCHLUESSEL DES PRODUKTS lt. Spartenaufschlüsselung
               10 DTA-BNK-DAT-DEPOTEROEFF             PIC  9(008).
      *           'JJJJMMTT'
               10 DTA-BNK-ANZ-ANTEILE                 PIC  9(006)V9999.
      *           ANZAHL DER ANTEILE
               10 DTA-BNK-GUTHABEN                    PIC  9(009)V99.
      *           RUECKNAHMEPREIS GESAMT
               10 DTA-BNK-DAT-GUTHABEN                PIC  9(008).
      *           DATUM DES RUECKNAHMEWERTES 'JJJJMMTT'
               10 DTA-BNK-SPARBETRAG                  PIC  9(009)V99.
      *           Sparbetrag
               10 DTA-BNK-ZAHLWEISE                   PIC  9(002).
      *           ZAHLWEISE DES SPARBETRAGS
               10 DTA-BNK-FONDSBEZEICHNUNG            PIC  X(040).
      *           Fondsbezeichnung
               10 DTA-BNK-RISIKOKLASSE                PIC  9(001).
      *           Risikoklasse
               10 DTA-BNK-WIEDERANLAGE                PIC  9(008).
      *           Wiedernalagedatum 'JJJJMMTT'
               10 Filler                              PIC  X(266).

           05  DTA-BNK-VORSATZ REDEFINES DTA-BNK-DATEN.

               10 DTA-BNK-V-SATZART                   PIC  X.
      *           'A'
               10 DTA-BNK-ABSENDER                    PIC  X(005).
      *           'DWIEN'
               10 DTA-BNK-DTADATUM                    PIC  9(008).
      *           JJJJMMTT  (alle DTAs von einem Tag müssen in
      *           diesem Feld den gleichen Wert haben)
               10 DTA-BNK-EMPFAENGER                  PIC  X(004).
      *           'DVAG'
               10 DTA-BNK-DATENTEXT                   PIC  X(025).
      *           'DEPOT'
               10 DTA-BNK-VERABKZ                     PIC  X(004).
      *           'INIT'

           05  DTA-BNK-NACHSATZ REDEFINES DTA-BNK-VORSATZ.

               10 DTA-BNK-N-SATZART             PIC X(001).
      *           'E'
               10 DTA-BNK-ANZ                   PIC 9(009).
      *           Satzanzahl incl. Vor- und Nachsatz

      ******************************************************************
      *          E n d e    Copy-Member DRKW0011                       *
      ******************************************************************


