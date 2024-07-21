      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2897.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI2897.CPv  $
      *
      *   Rev 2.11   Feb 02 2017 15:58:44   t13ex39
      *AKI01410: FodB Goldsparplan Upro überarbeiten
      *
      *   Rev 2.10   Jan 10 2017 10:53:40   t13ex39
      *AKI01410: FodB Goldsparplan Upro überarbeiten
      *
      *   Rev 2.9   Dec 13 2016 04:49:10   t13ex39
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.8   Nov 21 2016 11:28:12   t13ex39
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.7   Nov 21 2016 11:19:46   t13ex39
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.6   Jan 18 2016 08:06:38   t13ex15
      *AKI01410: FodB Goldsparplan Programme und Copy
      *
      *   Rev 2.5   Aug 03 2015 12:52:16   t13ex15
      *AKI01365: Harmonisierung 2015 AGI Source
      *
      *   Rev 2.4   Jun 16 2014 13:51:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.3   May 28 2014 10:48:58   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.2   May 23 2014 11:21:06   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.1   May 13 2014 16:26:20   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.0   May 07 2014 10:12:10   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2897                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2897 UPKI0247           *
      *    --------------------------------------------------------   *
      ****************************************************************



      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01167!25.07.12!    ! NEU                       ! B. Stoldt*
      * -------------------------------------------------------------*
      * AKI01365!22.07.15!2.4 ! VA und VS gelten für alle ! SWZ      *
      * -------------------------------------------------------------*
120116* AKI01410!12.01.16!2.5 ! GS Goldsparplan           ! SWZ      *
      * -------------------------------------------------------------*
221216*         !22.12.16!    ! Copy grundlegend geändert ! WHO      *
      *         !        !    ! Vorgängerversion s.2.9    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI2897.
           05  PI-DRKI2897-DATEN.
               10 PI-DRKI2897-GES              PIC   X(02).
               10 PI-DRKI2897-KTO-ART          PIC   X(02).
      *        * aus PMKI0093
      *        *
                  88 S-KTO-ART-EA-PM              VALUES 'PE' 'PA'.
                  88 S-KTO-ART-EA-IF              VALUES 'IE' 'IA'.
                  88 S-KTO-ART-EA-FF              VALUES 'FE' 'FA'.
                  88 S-KTO-ART-EA-ST              VALUES 'SE' 'SA'.

       01  PO-DRKI2897.
           05  PO-DRKI2897-RC                  PIC  X(003).
               88 PO-DRKI2897-RC-OK                 VALUE 'OK '.
               88 PO-DRKI2897-RC-NFD                VALUE 'NFD'.
           05  PO-DRKI2897-ERGEBNISZEILE.
               10 PO-DRKI2897-ANTEIL-LFDNR     PIC  9(04).
               10 PO-DRKI2897-VL-KZ            PIC  X(01).
               10 PO-DRKI2897-KTO-ART          PIC  X(02).
      *        *
      *        * aus PMKI0093
      *        * Die spezielle KTO-ART z.B. 'PA' muss wegen Fehler
      *        * auf diezugehoerige allgemeine KTO-ART gemappt werden
      *        *
                 88  S-KTO-ART-PM                VALUE 'PM'.
                 88  S-KTO-ART-IF                VALUE 'IF'.
                 88  S-KTO-ART-FF                VALUE 'FF'.
                 88  S-KTO-ART-ST                VALUE 'ST'.

      ******* ENDE COPY-MEMBER **************************************







