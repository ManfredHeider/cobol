      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3386.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3386.CPv  $
      *
      *   Rev 2.4   Nov 01 2018 11:48:12   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.3   Nov 01 2018 09:25:14   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.2   Oct 31 2018 17:28:02   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.1   Oct 30 2018 11:03:24   t13ae52
      *AKI01615: Erm./Aufb./Akt. KONTO (DSC)
      *
      *   Rev 2.0   Aug 21 2018 16:23:42   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER FÜR KONTO VON GENERALI INVEST                 *
      *                                                                *
      *    SATZLÄNGE   200                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO      *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       05  DRKI3386.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GENERALI INVESTMENTS'
               15  'XX'-V-DATUM         PIC  9(008).
      *            DATUM = 'JJJJMMTT' ERSTELLUNG
               15  'XX'-V-DATUM-STAND   PIC  9(008).
      *            DATUM = 'JJJJMMTT' STAND
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT          PIC  X(020).
      *            'KONTENDATEN'
               15  'XX'-V-ART           PIC  X(001).
      *            K = KOMPLETTBESTAND, D FÜR DELTA
               15  FILLER               PIC  X(108).
      *            RESERVE
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ   REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                        PIC  X(001).
      *            SATZART        = 'C'
 
               15  'XX'-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '42'
 
               15  'XX'-KEY.
                   20  'XX'-VNR                   PIC  9(015).

               15  'XX'-DATUM-EROEFFNUNG.
                   20 'XX'-JAHR-EROEFFNUNG        PIC  9(004).
                   20 'XX'-MONAT-EROEFFNUNG       PIC  9(002).
                   20 'XX'-TAG-EROEFFNUNG         PIC  9(002).
 
               15  'XX'-DATUM-SCHLIESSUNG.
                   20 'XX'-JAHR-SCHLIESSUNG       PIC  9(004).
                   20 'XX'-MONAT-SCHLIESSUNG      PIC  9(002).
                   20 'XX'-TAG-SCHLIESSUNG        PIC  9(002).
 
               15  'XX'-EXTERNES-LIMIT            PIC  S9(012)V99.
               15  'XX'-EXTERNES-LIMIT-R REDEFINES
                   'XX'-EXTERNES-LIMIT            PIC  X(014).
 
               15  'XX'-SALDO                     PIC  S9(012)V99.
               15  'XX'-SALDO-R REDEFINES
                   'XX'-SALDO                     PIC  X(014).
      *
               15  'XX'-KONTO-ART                 PIC  X(002).
      *             01 = Kontoflex;
      *             02 = Tagesgeld;
      *             03 = Festgeld
 
               15  'XX'-KUMHABENAM                PIC  S9(012)V99.
               15  'XX'-KUMHABENAM-R REDEFINES
                   'XX'-KUMHABENAM                PIC  X(014).
 
               15  'XX'-KUMSOLLAM                 PIC  S9(012)V99.
               15  'XX'-KUMSOLLAM-R REDEFINES
                   'XX'-KUMSOLLAM                 PIC  X(014).
 
               15  'XX'-KUMHABENVM                PIC  S9(012)V99.
               15  'XX'-KUMHABENVM-R REDEFINES
                   'XX'-KUMHABENVM                PIC  X(014).
 
               15  'XX'-KUMSOLLVM                 PIC  S9(012)V99.
               15  'XX'-KUMSOLLVM-R REDEFINES
                   'XX'-KUMSOLLVM                 PIC  X(014).
 
               15  'XX'-ZRAM                      PIC  X(005).
 
               15  'XX'-ZRVM                      PIC  X(005).
 
               15  'XX'-SOLLZINS                  PIC  S9(02)V9999.
               15  'XX'-SOLLZINS-R REDEFINES
                   'XX'-SOLLZINS                  PIC  X(006).
 
               15  'XX'-UEBERZZINS                PIC  S9(02)V9999.
               15  'XX'-UEBERZZINS-R REDEFINES
                   'XX'-UEBERZZINS                PIC  X(006).
 
               15  'XX'-HABENZINS                 PIC  S9(02)V9999.
               15  'XX'-HABENZINS-R REDEFINES
                   'XX'-HABENZINS                 PIC  X(006).
 
               15  'XX'-BUCHUNGS-DAT              PIC  9(008).
 
               15  'XX'-RESERVE                   PIC  X(044).
      *            RESERVE
      *
      ******************************************************************
      *                                                                *
      *          NACHSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-NACHSATZ  REDEFINES     'XX'-VORSATZ.
               15  'XX'-N-SA                      PIC  X(001).
      *            SATZART 'E'
               15  'XX'-N-ANZ                     PIC  9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VORSATZ
               15  FILLER                         PIC  X(142).



