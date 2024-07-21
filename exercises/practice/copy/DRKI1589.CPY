      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1589.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1589.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:23:04   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 15 2003 16:29:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1589   VARIABLEN  FÜR UPRO UPKI0163      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:    10.09.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00367!10.09.03!1.0 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI1589.
           05  PI-DRKI1589-DATEN.
             10 PI-DRKI1589-PERSID           PIC X(13).
             10 PI-DRKI1589-KND-STAT-ALT     PIC X(01).
             10 PI-DRKI1589-KND-STAT-NEU     PIC X(01).
             10 PI-DRKI1589-BEWERTUNG-DVAG   PIC X(02).
             10 PI-DRKI1589-BEWERTUNG-VB     PIC X(02).
             10 PI-DRKI1589-EREIGNIS         PIC 9(01).
             10 PI-DRKI1589-PROGRAMM         PIC X(08).
             10 PI-DRKI1589-PRODUKT          PIC X(10).
             10 PI-DRKI1589-VNR              PIC X(15).
             10 PI-DRKI1589-VBID             PIC X(13).
             10 PI-DRKI1589-GES              PIC S9(04) COMP.
      *



       01  PO-DRKI1589.
           05  PO-DRKI1589-RC             PIC  X(003).
               88 PO-DRKI1589-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1589-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI1589-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

