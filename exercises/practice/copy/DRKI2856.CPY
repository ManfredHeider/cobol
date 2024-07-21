      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2856.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2856.CPv  $
      *
      *   Rev 2.2   May 23 2014 11:20:46   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.1   May 13 2014 16:25:58   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 K
      *ündigung PGM Cpy
      *
      *   Rev 2.0   Apr 04 2014 10:15:44   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 8 5 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G40 OIF Kuendigung KSDS                      *
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
       01  DRKI2856.
      *
               20  'XX'-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  'XX'-KEY.
                   25  'XX'-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES 'XX'-VNR.
                      30  'XX'-VNR-1-13         PIC  9(013).
                      30  'XX'-UNTER-NR         PIC  9(002).
      *            UNTERDEPOTNUMMER)

                   25  'XX'-ISIN                PIC  X(12).
      *            ISIN

                   25  'XX'-KTO-ART             PIC  X(02).
      *            Kontoart

                   25  'XX'-RUECKZAHL-TERMIN    PIC  9(08).
      *            voraussichtlicher Rückzahlungstermin

      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
               20  'XX'-ANZ-ANTEILE         PIC  9(13)V9999.
      *            Anzahl Anteile
      *        *****************************************************
               20  FILLER                  PIC  X(43).
      *            RESERVE


