      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3271.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3271.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:30:22   t13ae31
      *AKI01812: G21 Leben Basis Viridium Programme
      *
      *   Rev 2.1   Apr 13 2018 10:13:40   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 16 2018 16:03:34   t13ex19
      *Archiverstellung
      *
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEM ERMITTLUNGSPROGRAMM PMKI1374            *
      *     - Eingabedatei GEN-Vertragsdaten mit Vorsatz              *
      *                                                               *
      *    SATZLÄNGE   300                                            *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.02.18!NEUERSTELLUNG                          !BST      !2.0  *
      *--------!---------------------------------------!---------!-----*
      *18.11.20!Änderung zu Viridium neues Feld        !HHU      !     *
      *AKI01812! MIG-KZ ersetzt AKZ und Wagnis         !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *****************************************************************

       01  DRKI3271.

           05  'XX'-VORSATZ.
      *
               10  'XX'-V-SA                       PIC  X(001).
      *            SATZART = 'A'
                   88 'XX'-SATZART-VORSATZ         VALUE 'A'.
                   88 'XX'-SATZART-ENDESATZ        VALUE 'E'.
               10  'XX'-V-ABSENDER                 PIC  X(003).
      *            ABSENDER = 'GEN'
               10  'XX'-V-DATUM                    PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *            EMPFAENGER = 'ALAG'
                   88 'XX'-V-EMPFAENGER-ALAG       VALUE 'ALAG'.
               10  'XX'-V-TEXT                     PIC  X(016).
      *            INHALTSANGABE = 'Basis Vertrag '
               10  'XX'-V-KENNZ                    PIC  X(004).
      *            INHALTSANGABE = 'AEND' für Änderungsbestand
               10  FILLER                          PIC  X(264).
      *            RESERVE

           05  'XX'-DATENSATZ REDEFINES 'XX'-VORSATZ.

               10  'XX'-D-SCHLUESSEL.
                 15  'XX'-D-GESELLSCHAFT           PIC  X(002).
                 15  'XX'-D-VERTRNR                PIC  X(015).
                 15  'XX'-D-LFDNR                  PIC  9(004).
               10  'XX'-D-AENDKZ                   PIC  9(001).
               10  'XX'-D-VBNR                     PIC  9(007).
               10  'XX'-D-BEGINN-DAT               PIC  9(008).
               10  'XX'-D-ENDE-DAT                 PIC  9(008).
               10  'XX'-D-ZUSTAND                  PIC  9(002).
               10  'XX'-D-BEITRAG                  PIC  9(008)V99.
               10  'XX'-D-JNBEITRAG                PIC  9(008)V99.
               10  'XX'-D-ZAHLWEISE                PIC  X(002).
               10  'XX'-D-SPARTE                   PIC  X(003).
               10  'XX'-D-PRODUKT                  PIC  X(050).
               10  'XX'-D-TARIF                    PIC  X(050).
               10  'XX'-D-SUMME                    PIC  9(008)V99.
               10  'XX'-D-MIG-KZ                   PIC  X(001).
               10   FILLER                         PIC X(11).
               10   FILLER-WKZ                     PIC 9(003).
               10  'XX'-D-GUTHABEN                 PIC  9(008)V99.
               10  'XX'-D-VERTRWERT                PIC  9(008)V99.
               10  'XX'-D-RENTENBEZUG              PIC  X(001).
               10  'XX'-D-VORAUS-RENTE             PIC  9(006).
               10  'XX'-D-GARANT-RENTE             PIC  9(006).
               10  'XX'-D-BERECH-DAT               PIC  9(008).
               10  'XX'-D-GESCHAEFTSZWEIG          PIC  X(002).
               10  'XX'-D-FLAECHE                  PIC  9(005).
               10  'XX'-D-PRODUKT-KOMBI-NR         PIC  9(005).
               10  FILLER                          PIC  X(050).


           05  'XX'-NACHSATZ REDEFINES 'XX'-VORSATZ.
      *
               10  'XX'-N-SA                       PIC X(001).
      *            SATZART = 'E'
               10  'XX'-N-SATZANZAHL               PIC 9(009).
300915         10  FILLER                          PIC X(290).
      *            RESERVE

      ******************************************************************
      *          E n d e    Copy-Member DRKI3271                       *
      ******************************************************************

      *---------------------------------------------------------------*


