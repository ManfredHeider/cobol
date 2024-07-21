      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2316.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2316.CPv  $
      *
      *   Rev 2.8   Dec 13 2016 04:50:38   t13ex39
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.7   Jan 18 2016 08:06:28   t13ex15
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.6   Dec 20 2011 14:37:50   t13ex15
      *AKI01020: SEB Invest : Umstellung Datenspiegel Neue Kontart
      *VA und VS
      *
      *   Rev 2.5   Aug 19 2011 07:42:56   t13ex15
      *AKI01057: AGI/FodB: neue Kontoart "DF" im Datenspiegel Progr
      *amme
      *
      *   Rev 2.4   Mar 16 2010 13:57:16   t13ex50
      *AKI00930: AGI/FodB: neue Kontoart WA auf dem HOST einrichten
      *
      *   Rev 2.3   Mar 11 2010 10:40:06   t13ex50
      *AKI00930: AGI/FodB: neue Kontoart WA auf dem HOST einrichten
      *
      *   Rev 2.2   Mar 11 2010 10:28:10   t13ex50
      *AKI00930: AGI/FodB: neue Kontoart WA auf dem HOST einrichten
      *
      *   Rev 2.1   Jun 15 2009 16:45:06   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik
      *
      *   Rev 2.0   Apr 23 2009 14:46:08   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 3 1 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER die Zuordnung von Unterkontoart und          *
      *    lfd. Nr. GURHABENANTEIL                                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.01.08!NEUERSTELLUNG                          !WHO      ! 1.0 *
      *AKI00740!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
220310*22.03.10!NEUE UNTER-KONTO-ART WA (POSITION 32)  !PSC      ! 2.2 *
      *AKI00930!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
150811*15.08.11!NEUE UNTER-KONTO-ART DF (POSITION 32)  !SWZ      ! 2.5 *
      *AKI01057!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
201211*20.12.11!NEUE UNTER-KONTO-ART VS und VA         !SWZ      ! 2.6 *
      *AKI01057!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
120116*12.01.16!NEUE UNTER-KONTO-ART GS Goldsparplan   !SWZ      ! 2.7 *
      *AKI01410!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      ******************************************************************
       01  Z-UNTER-KONTO-VALUE.

           05 FILLER             PIC  X(005) VALUE 'A1' .
           05 FILLER             PIC  X(005) VALUE 'A2' .
           05 FILLER             PIC  X(005) VALUE 'AP' .
           05 FILLER             PIC  X(005) VALUE 'AS' .
           05 FILLER             PIC  X(005) VALUE 'AV'  .
           05 FILLER             PIC  X(005) VALUE 'BP'  .
           05 FILLER             PIC  X(005) VALUE 'BV'  .
           05 FILLER             PIC  X(005) VALUE 'DE'   .
           05 FILLER             PIC  X(005) VALUE 'F1'  .
           05 FILLER             PIC  X(005) VALUE 'F2'  .
           05 FILLER             PIC  X(005) VALUE 'FP'  .
           05 FILLER             PIC  X(005) VALUE 'IF'  .
           05 FILLER             PIC  X(005) VALUE 'PM'  .
           05 FILLER             PIC  X(005) VALUE 'PP'  .
           05 FILLER             PIC  X(005) VALUE 'SP'  .
           05 FILLER             PIC  X(005) VALUE 'ST'  .
           05 FILLER             PIC  X(005) VALUE 'SV'  .
           05 FILLER             PIC  X(005) VALUE 'V1'  .
           05 FILLER             PIC  X(005) VALUE 'VL'  .
           05 FILLER             PIC  X(005) VALUE 'VP'  .
           05 FILLER             PIC  X(005) VALUE 'VW'  .
           05 FILLER             PIC  X(005) VALUE 'FF'  .
           05 FILLER             PIC  X(005) VALUE 'IE'  .
           05 FILLER             PIC  X(005) VALUE 'PE'  .
           05 FILLER             PIC  X(005) VALUE 'SE'  .
           05 FILLER             PIC  X(005) VALUE 'IA'  .
           05 FILLER             PIC  X(005) VALUE 'PA'  .
           05 FILLER             PIC  X(005) VALUE 'SA'  .
           05 FILLER             PIC  X(005) VALUE 'FE' .
           05 FILLER             PIC  X(005) VALUE 'OV' .
           05 FILLER             PIC  X(005) VALUE 'FA' .
220310     05 FILLER             PIC  X(005) VALUE 'WA' .
150811     05 FILLER             PIC  X(005) VALUE 'DF' .
201211     05 FILLER             PIC  X(005) VALUE 'VA' .
201211     05 FILLER             PIC  X(005) VALUE 'VS' .
120116     05 FILLER             PIC  X(005) VALUE 'GS' .

       01  Z-UNTER-KONTO-ARR REDEFINES Z-UNTER-KONTO-VALUE.
120116     05 Z-UNTER-KONTO-OCC                OCCURS 36.
              10  Z-UNTER-KONTO-DIT   PIC  X(005).

120116 01  Z-UNTER-KONTO-MAX          PIC S9(009) VALUE 36.

       01  I-UNTER-IDX                PIC  S9(004) COMP VALUE 0.



