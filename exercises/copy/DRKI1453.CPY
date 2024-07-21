      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1453.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1453.CPv  $
      *
      *   Rev 2.3   29 Apr 2008 16:25:34   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten PERFORMANCE
      *
      *   Rev 2.2   29 Apr 2008 10:58:56   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten PERFORMANCE
      *
      *   Rev 2.1   Jul 10 2003 13:45:08   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR PERFORMANCE DATEI DEUTSCHE BANK             *
      *    ----------------------------------------------              *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 100 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00306!10.09.02!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00708!14.02.08!    ! zus. Attribute            !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1453.
      *
           10  DB-VORSATZ.
      *
               20  DB-V-SA            PIC X(001).
      *            SATZART = '1'
               20  DB-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DB AG'
               20  DB-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  DB-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  DB-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            PERFORMANCE  KOMPLETT 'PERFK   '
               20  DB-V-WKZ           PIC X(03).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(68).
      *            RESERVE
      *
           10  DB-SATZ     REDEFINES      DB-VORSATZ.
      *
               20  DB-SA                  PIC  X(001).
      *            SATZART        = '8'
               20  DB-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  DB-VNR.
                   25  DB-FILIAL-NR       PIC 9(03).
                   25  DB-KDNR            PIC 9(07).
                   25  DB-UNTER-DEP-NR    PIC 9(02).
                   25  DB-WKZ             PIC 9(03).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  DB-BRUTTO-PER-LDJ      PIC S9(13)V9(05).
      *            Bruttoperformance in % im lfd. Jahr
               20  DB-BRUTTO-PER-GES      PIC S9(13)V9(05).
      *            Bruttoperformance in % gesamt
               20  DB-DEL-KZ              PIC X(01).
      *            DELETE-Kennzeichen
               20  DB-BEG-BTR-PERFORM-LFD PIC X(08).
>>    *            Beginn Bruttoperformance in % im lfd. Jahr
               20  DB-END-BTR-PERFORM-LFD PIC X(08).
      *            Ende Bruttoperformance in % im lfd. Jahr
               20  DB-BEG-BTR-PERFORM-GES PIC X(08).
<<    *            Beginn Bruttoperformance in % gesamt
               20  DB-END-BTR-PERFORM-GES PIC X(08).
      *            Ende Bruttoperformance in % gesamt
      *        20  FILLER                 PIC  X(46).
               20  FILLER                 PIC  X(13).
      *            RESERVE

           10  DB-NACHSATZ    REDEFINES     DB-VORSATZ.
      *
               20  DB-N-SA            PIC X(001).
      *            SATZART             '9'
               20  DB-N-ANZ           PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(092).
      *            RESERVE

