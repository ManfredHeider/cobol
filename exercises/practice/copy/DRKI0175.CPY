      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0175.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0175.CPv  $
      *
      *   Rev 2.5   Jun 29 2009 12:18:40   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.4   Jun 29 2009 12:07:32   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.3   Jun 23 2009 10:33:32   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.2   Oct 16 2008 13:43:26   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.1   Oct 13 2004 15:25:34   t13ex48
      *AKI00386: Akt. DIT-Performance, kum. Ein- und Auszahlungen
      *
      *   Rev 2.0   Nov 14 2002 15:22:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER fuer aufbereitete Aenderungen der kumulierten     *
      *  Ein- und Auszahlungen                                         *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *02.07.04!NEUERSTELLUNG                          !Sebbach  ! 01  *
      *AKI00386!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.08.08! NEUE STRUKTUR                         !WHO      ! 02  *
      *AKI00735!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.01.09! AGI Unterkonten                       !WHO      ! 03  *
      *AKI00740!                                       !         !     *
      ******************************************************************
       01  DRKI0175.
           05  BNK-KUMEA-SA                  PIC X(001).
      *            SATZART        = 'C'
           05  BNK-KUMEA-GES                 PIC  9(02).
      *            GESELLSCHAFT   = '40'
           05  BNK-KUMEA-VNR                 PIC  9(13).
           05  BNK-KUMEA-GHP-LFDNR           PIC  9(02).

           05  BNK-KUMEA-ANTEIL-LFDNR        PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER

           05  BNK-KUMEA-ISIN                PIC  X(12).
      *        International Securities Identification Number

           05  BNK-KUMEA-WKN                 PIC  X(06).
      *        WERTPAPIERKENNNUMMER

           05  BNK-KUMEA-EINZ-KUM            PIC  S9(11)V99.
      *        KUMULIERTE EINZAHLUNGEN

           05  BNK-KUMEA-AUSZ-KUM            PIC  S9(11)V99.
      *        KUMULIERTE AUSZAHLUNGEN

           05  BNK-AUSS-KUM                  PIC  S9(11)V99.
      *        KUMULIERTE AUSschüttung

           05  BNK-KUMEA-VTG-ID              PIC  X(13).

           05  BNK-GHP-VL-KZ                 PIC  X(01).
           05  FILLER                        PIC  X(82).
      *            RESERVE

