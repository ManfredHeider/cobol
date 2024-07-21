      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0158.cpy  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:\MF\CPY\drat0158.cpv  $
      *
      *   Rev 1.1   19 Sep 2000 16:15:02   T13EX25
      *APA00105, Ausprägung anpassen
      *
      *   Rev 1.0   18 Aug 1997 10:27:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0158  KONSTANTEN FÜR     KTY = 122       *
      *                 Konditions-Regel Kunden-Betreuung            *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! Auerbach *
      *         !        !    !                           !          *
      * APA00105!19.09.00! 02 ! Anpassung Ausprägung 3    ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  REGEL                                                  *
      *  ----------------------------------                          *
      *    1  Keine Kundenbetreungsprovision                         *
      *    2  Kunden-Betreuungsprovision aus der Grundprovision      *
      *    3  Kunden-Betreuungsprovision aus der Gesamtprovision     *
      *                                                              *
      ****************************************************************

       01  FILLER.
           05 K-KNR-K-REGEL-KD-KEINE         PIC S9(004) BINARY VALUE 1.
           05 K-KNR-K-REGEL-KD-GP-BEZUG      PIC S9(004) BINARY VALUE 2.
190900*    05 K-KNR-K-REGEL-KD-NP-BEZUG      PIC S9(004) BINARY VALUE 3.
190900     05 K-KNR-K-REGEL-KD-GS-BEZUG      PIC S9(004) BINARY VALUE 3.
