      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2858.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2858.CPv  $
      *
      *   Rev 2.3   Jun 11 2014 13:52:14   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.2   May 23 2014 11:21:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.1   May 13 2014 16:26:12   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.0   Apr 04 2014 10:15:58   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 5 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G40 OIF Kuendigung Aufbereitung Änderung     *
      *
      *    SATZLÄNGE   100 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *04.04.14!NEUERSTELLUNG                          !SWZ      ! 01  *
      ******************************************************************
       01  DRKI2858.
      *
               10  'XX'-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40','46','47'
               10  'XX'-VNR                 PIC  9(015).
               10  'XX'-GHP-LFDNR           PIC  9(004).
               10  'XX'-ANTEIL-LFDNR        PIC  9(004).
      *            LFDNR
               10  'XX'-ISIN                PIC  X(12).
      *            ISIN
               10  'XX'-RUECKZAHL-TERMIN    PIC  X(10).
      *            voraussichtlicher Rückzahlungstermin

      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
               10  'XX'-ANZ-ANTEILE         PIC  9(13)V9999.
      *            Anzahl Anteile
      *        *****************************************************
               10  'XX'-AKTUAL-KZ           PIC  X(01).
      *            Insert, Update, Delete
               10  FILLER                   PIC  X(35).
      *            RESERVE

