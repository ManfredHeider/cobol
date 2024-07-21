      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3010.CPv  $
      *
      *   Rev 2.1   Jul 24 2015 13:35:24   t13ex23
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen-PGMs und Copies
      *
      *   Rev 2.0   Jun 12 2015 10:02:16   t13ex23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER                                                 *
      *        - EINGABE DTA GELEISTETE RENTEN AML-                    *
      *                                                                *
      *    SATZLÄNGE 48                                               *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01372!12.06.15!1.00! NEU                       ! MKE      *
      *         !17.06.15!1.01! RNTART VON 1 AUF 2 BYTE   ! HSE      *
      *         !        !    ! VERGROESSERT              !          *
      *         !18.06.15!1.02! FILLER IN VOR- UND NACH-  ! MKE      *
      *         !        !    ! SATZ EINGEFUEGT           !          *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  'XXX'-DRKI3010.
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-SATZ.
      *
               20  'XXX'-SA              PIC X(01).
      *            SATZART        = 'C'
               20  'XXX'-VNR             PIC 9(10).
      *            ORDNUNGSBEGRIFF = VERSICHERUNGSNUMMER
               20  'XXX'-RNTBEG          PIC X(10).
      *            BEGINN DER RENTE
               20  'XXX'-RNTART          PIC 9(02).
      *            ART DER RENTE (AUS RENTENBESTANDSFUEHRUNG)
               20  'XXX'-RNTABLAUF       PIC X(10).
      *            ABLAUF DER RENTE
               20  'XXX'-RNTZAHLW        PIC 9(02).
      *            RENTENZAHLWEISE (BEIM VEP IMMER 12 = MONATLICH)
               20  'XXX'-RNTGSMT         PIC 9(11)V99.
      *            GESAMTE HOEHE DER RENTE
      *
      ******************************************************************
      *                                                                *
      *         VORSATZ                                                *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-VORSATZ   REDEFINES 'XXX'-SATZ.
      *
               20  'XXX'-V-SA            PIC X(001).
      *        SATZART = 'A'
               20  'XXX'-V-ABSENDER      PIC X(003).
      *        SATZART = 'AML'
               20  'XXX'-V-DATUM         PIC 9(008).
      *        ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  'XXX'-V-EMPFAENGER    PIC X(004).
      *        EMPFAENGER = 'DVAG'
               20  'XXX'-V-TEXT          PIC X(020).
      *        AENDERUNGSBESTAND = 'VERTRAG GELRNT'
      *        KOMPLETTBESTAND = 'VERTRAG GELRNT INIT'
               20  FILLER                PIC X(012).
      *
      ******************************************************************
      *                                                                *
      *         NACHSATZ                                               *
      *                                                                *
      ******************************************************************
      *
           10  'XXX'-NACHSATZ  REDEFINES 'XXX'-SATZ.
      *
               20  'XXX'-N-SA          PIC X(001).
      *        SATZART = 'E'
               20  'XXX'-N-ANZ         PIC 9(009).
      *        ANZAHL SAETZE INKL. VOR- UND NACHSATZ
               20  FILLER              PIC X(038).

