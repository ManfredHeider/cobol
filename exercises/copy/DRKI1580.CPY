      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1580.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1580.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:20:46   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 15 2003 16:29:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1580   VARIABLEN  FÜR UPRO UPKI0163      *
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

       01  PI-DRKI1580.
           05  PI-DRKI1580-DATEN.
             10 PI-DRKI1580-VERTID           PIC X(13).
             10 PI-DRKI1580-PERSID           PIC X(13).
             10 PI-DRKI1580-KND-STAT-ALT     PIC X(01).
             10 PI-DRKI1580-KND-STAT-NEU     PIC X(01).
             10 PI-DRKI1580-BEWERTUNG-DVAG   PIC X(02).
             10 PI-DRKI1580-BEWERTUNG-VB     PIC X(02).
             10 PI-DRKI1580-EREIGNIS         PIC 9(01).
             10 PI-DRKI1580-PROGRAMM         PIC X(08).
             10 PI-DRKI1580-PRODUKT          PIC X(10).
      *



       01  PO-DRKI1580.
           05  PO-DRKI1580-RC             PIC  X(003).
               88 PO-DRKI1580-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1580-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI1580-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

