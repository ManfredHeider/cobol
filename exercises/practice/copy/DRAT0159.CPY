      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0159.cpy  $
      *    $Revision:   1.2  $
      *
      *    $Log:   L:\MF\CPY\drat0159.cpv  $
      *
      *   Rev 1.2   28 Sep 2000 10:42:28   T13EX25
      *APA00105, Neue Konstanten für EE iii
      *
      *   Rev 1.1   Jul 28 1999 13:27:02   T13EX16
      *APA00046 Neuerstellung für DAISY-PVW
      *
      *   Rev 1.0   18 Aug 1997 10:27:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0159  KONSTANTEN FÜR     KTY = 123       *
      *                 Konditions-Regel Ersatz/Erhöhung-Prüfung     *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! AUERBACH *
      *         !13.11.97!    ! KORREKTUREN WG. NEUDEFINI-! STORCH   *
      *         !        !    ! TION UND NAMENSäNDERUNGEN !          *
      * APA00105!25.09.00! 02 ! Neue Regeln für Sparplan  ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  REGEL                                                  *
      *  ----------------------------------                          *
      *    1  keine Prüfung mit Rückprämie Altvertrag                *
      *    2  Prüfung mit Rückprämie Altvertrag                      *
      *    3  keine Prüfung mit Rückprämie Neuvertrag                *
      *    4  Prüfung mit Rückprämie Neuvertrag                      *
      *                                                              *
      ****************************************************************

       01  FILLER.
           05 K-KNR-K-REGEL-EEP-KP-ALT       PIC S9(004) BINARY VALUE 1.
           05 K-KNR-K-REGEL-EEP-P-ALT        PIC S9(004) BINARY VALUE 2.
           05 K-KNR-K-REGEL-EEP-KP-NEU       PIC S9(004) BINARY VALUE 3.
           05 K-KNR-K-REGEL-EEP-P-NEU        PIC S9(004) BINARY VALUE 4.
250900     05 K-KNR-K-REGEL-EEP-KP-EH        PIC S9(004) BINARY VALUE 5.
250900     05 K-KNR-K-REGEL-EEP-P-SEH        PIC S9(004) BINARY VALUE 6.
