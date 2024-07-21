      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0222.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0222.CPv  $
      *
      *   Rev 2.3   Feb 22 2016 13:52:10   t13ae23
      *AAN01247: Anpassung DTA eingereichtes G. Badenia
      *
      *   Rev 2.2   Mar 23 2010 08:53:38   t13ae34
      *AAN00916: Zukünftige Erfassung ImmoKonstant
      *
      *   Rev 2.1   Mar 18 2009 08:12:06   t13ae34
      *AAN00868: autom. PIN-Vergabe bei der Badenia
      *
      *   Rev 2.0   Mar 06 2009 11:01:30   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *  BADENIA SST AUTOMATISCHE PIN-VERGABE DURCH BADENIA
      ****************************************************************
      *    COPY-MEMBER: DRAN0222                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00868!11.03.09!2.1 !NEU                        ! HEIDER   *
      * AAN00916!08.02.10!2.2 !VM                         ! HEIDER   *
      * AAN01247!01.02.16!2.3 !VERGRÖßERN AUF 200         ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRAN0222-SATZ.

      **** PIN
           05 DRAN0222-PIN                          PIC 9(08).
      **** BAUSPARVERTRAGSNUMMER
           05 DRAN0222-BSV-NR                       PIC 9(10).
      **** ABSCHLUSSVERMITTLER
           05 DRAN0222-AVERM                        PIC 9(07).
      **** DVAG-PRODUKTSCHLÜSSEL 1
           05 DRAN0222-PROD-PROD-SCHL-1             PIC 9(02).
      **** DVAG-VERTRAGSSUMME-1
           05 DRAN0222-SUMME-1                      PIC 9(07).
      **** DVAG-PRODUKTSCHLÜSSEL 2 (OPTIONAL)
           05 DRAN0222-PROD-PROD-SCHL-2             PIC 9(02).
      **** DVAG-VERTRAGSSUMME-2 (OPTIONAL)
           05 DRAN0222-SUMME-2                      PIC 9(07).
      **** KUNDE NAME
           05 DRAN0222-KUNDE-NAME                   PIC X(26).
      **** KUNDE VORNAME
           05 DRAN0222-KUNDE-VNAME                  PIC X(26).
      **** KUNDE PLZ
           05 DRAN0222-PLZ                          PIC X(09).
      **** KUNDE ORT
           05 DRAN0222-ORT                          PIC X(21).
      **** KUNDE AUSLANDSKENNZEICHEN (SIEHE KTY=17)
           05 DRAN0222-AUSL-KENNZ                   PIC X(03).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
           05 DRAN0222-DATUM-BEREITSTELLUNG         PIC X(10).
      **** ANTRAGSART (SIEHE KTY=217)
           05 DRAN0222-ANTRAGS-ART                  PIC 9(01).
080210**** VM
081002     05 DRAN0222-VM                           PIC 9(07).
010216**** 34I ZEICHNER WIRD NUR GELIEFERT, NICHT BENUTZT
010216     05 DRAN0222-34I-ZEICHNER                 PIC 9(07).
      *
081002*    05 FILLER                                PIC X(11).
010216*    05 FILLER                                PIC X(04).
010216     05 FILLER                                PIC X(47).


      *
      ******** ENDE COPY-MEMBER **************************************

