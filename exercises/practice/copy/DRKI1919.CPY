      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1919.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1919.CPv  $
      *
      *   Rev 2.3   Sep 03 2013 11:09:28   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.2   Aug 22 2013 08:25:56   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.1   Jul 25 2006 17:30:56   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en Kontokorrent
      *
      *   Rev 2.0   Jun 14 2006 13:16:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                    #####################                       *
      *           ########    D R K I 1 9 1 9    ########              *
      *                    #####################                       *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DBUMSATZ                                       *
      *        - AUSGABE: ALLGEMEIN DBUMSATZ FUER DEUTSCHE BANK        *
      *                                                                *
      *    SATZLÄNGE    80                                             *
      *                                                                *
      ******************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                   ! NAME     *
      * ---------------------------------------------------------------*
      * AKI00587!16.06.06!2.1 ! NEU                         !I. Frenzel*
      *         !        !    !                             !          *
      *         !        !    !                             !          *
      *         !        !    !                             !          *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  DRKI1919.

           05  DBU-GES                             PIC  9(02).
           05  DBU-VNR                             PIC  X(015).
           05  DBU-GHP-LFDNR                       PIC  9(002).
      *        NUR Unterkontonummer
           05  DBU-GHP-PRODUKT-ID                  PIC  S9(004).
      *        Produkt-ID
           05  DBU-DBPROD-LFDNR                    PIC  S9(04).
      *        lfd. Nr. von DBPRODUKT
           05  DBU-JAHR                            PIC  9(004).
      *        Jahr
           05  DBU-MONAT                           PIC  9(002).
      *        Monat

           05  DBU-KUMHABEN                        PIC  S9(012)V99.
           05  DBU-AEND-KUMHABEN-KZ                PIC  X(001).
      *        J = Änderung Habenumsätze
           05  DBU-KUMSOLL                         PIC  S9(012)V99.
           05  DBU-AEND-KUMSOLL-KZ                 PIC  X(001).
      *        J = Änderung Sollumsätze

           05  DBU-AKTUAL-KZ                       PIC  X(01).

      *        I = INSERT
      *        U = UPDATE

           05  FILLER                              PIC  X(16).
      ******************************************************************
