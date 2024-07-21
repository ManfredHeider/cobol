      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1413.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/DRKI1413.CPv  $
      *
      *   Rev 2.16   Jun 04 2018 07:01:08   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.15   May 30 2018 09:09:48   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.14   May 29 2018 08:25:40   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.13   Apr 20 2018 10:26:44   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.12   Apr 20 2018 09:53:50   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.11   Mar 27 2017 09:28:00   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.10   Nov 08 2013 13:52:12   t13ex48
      *AKI01260: Übermittlung der Daten des § 34f GewO an die Deuts
      *che Bank
      *
      *   Rev 2.9   Sep 27 2011 16:40:52   t13ae49
      *AKI01001: Deutsche Bank: Anspracheschutz
      *
      *   Rev 2.8   Sep 23 2011 15:00:54   t13ae49
      *AKI01001: Deutsche Bank: Anspracheschutz
      *
      *   Rev 2.7   Sep 16 2011 12:01:24   t13ex48
      *AKI01001: Deutsche Bank: Anspracheschutz: Programme
      *
      *   Rev 2.6   Aug 29 2006 09:54:32   t13ex14
      *AKI00602: Deutsche Bank: Erweiterung der Rück-DTA
      *
      *   Rev 2.5   Aug 29 2006 09:46:58   t13ex14
      *AKI00602: Deutsche Bank: Erweiterung der Rück-DTA
      *
      *   Rev 2.4   Aug 29 2006 09:37:58   t13ex14
      *AKI00602: Deutsche Bank: Erweiterung der Rück-DTA
      *
      *   Rev 2.3   Aug 29 2006 08:43:24   t13ex14
      *AKI00602: Deutsche Bank: Erweiterung der Rück-DTA
      *
      *   Rev 2.2   Aug 29 2006 08:18:08   t13ex14
      *AKI00602: Deutsche Bank: Erweiterung der Rück-DTA
      *
      *   Rev 2.1   Aug 05 2002 12:44:46   t13ex48
      *AKI00274
      *RUECK-DTA fuer DB24
      *
      *   Rev 2.0   Apr 23 2002 16:01:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1413 DATEIAUFBAU RUECK-DTA DB24/DWS      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: H. Sebbach                                      *
      *    DATUM:    06.05.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!06.05.02! 01 ! NEUERSTELLUNG             !Sebbach
      * -------------------------------------------------------------*
      * AKI0602 !29.08.06! 1.1! Aufnahme Berechtigt-KZ    !Misgeld   *
      * -------------------------------------------------------------*
      * AKI01001!18.07.11! 1.2! Erweiterung DTA           !HSE       *
      *         !        !    ! Lrecl von 150 auf 200     !HSE       *
      * -------------------------------------------------------------*
      * AKI01001!22.09.11! 1.3! Erwiterung Sender-Konstan-!HSE       *
      *         !        !    ! ten (88er Stufe)          !          *
      * -------------------------------------------------------------*
      * AKI01260!08.11.13! 1.4! Neues Feld Berechtigung   !HSE       *
      *         !        !    ! gemaess Par. 34F Gew-Ord. !          *
      * -------------------------------------------------------------*
      * FKI01544!24.03.17!    ! FVD entfernt              !HHU       *
      * -------------------------------------------------------------*
      * AKI01573!07.03.18!    ! Integration Generali und  !tfl       *
      *         !        !    ! erweiterte Stammdaten an  !          *
      *         !        !    ! Deutsche Bank             !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      * SATZBESCHREIBUNG: RUECKDTA DB24/DWS                          *
      * DATENSATZLAENGE: 500 Byte                                    *
      ****************************************************************
      *
      *01  DRKI1413-REC                     PIC X(150).
      *01  DRKI1413-REC                     PIC X(200).
       01  DRKI1413-REC                     PIC X(500).
 
       01  DRKI1413-0-Rec REDEFINES DRKI1413-REC.
           10 DRKI1413-0-VB-NR                PIC 9(07).
            88 DRKI1413-0-VB-NR-ZERO           VALUE 0.
           10 DRKI1413-0-RecType              PIC 9.
            88 DRKI1413-RecType-0              VALUE 0.
           10 DRKI1413-0-Sender               PIC X(16).
            88 DRKI1413-Sender-DVAG            VALUE 'DVAG'.
            88 DRKI1413-Sender-ALLFINANZ       VALUE 'ALLFINANZ'.
            88 DRKI1413-Sender-ALAG            VALUE 'ALAG'.
           10 DRKI1413-0-Receiver             PIC X(4).
            88 DRKI1413-Receiver-DB24          VALUE 'DB24'.
           10 DRKI1413-0-Content              PIC X(21).
            88 DRKI1413-0-Content-AgentCust   VALUE 'VERMITTLER-KUNDE'.
           10 DRKI1413-O-Date                 PIC X(10).
      *    10 DRKI1413-0-FILLER               PIC X(141).
           10 DRKI1413-0-FILLER               PIC X(441).
 
       01  DRKI1413-1-REC REDEFINES DRKI1413-REC.
           10 DRKI1413-1-VB-NR                PIC 9(07).
           10 DRKI1413-1-RECTYPE              PIC 9.
            88 DRKI1413-RECTYPE-1              VALUE 1.
           10 DRKI1413-1-UPDATE-FLAG          PIC 99.
           10 DRKI1413-1-VB-NAME              PIC X(30).
           10 DRKI1413-1-VB-VORNAME           PIC X(30).
           10 DRKI1413-1-VB-GEDDAT            PIC X(10).
           10 DRKI1413-1-VB-AUSTRITTS-DAT     PIC X(10).
      *    Kennzeichen ob VB der Strukturstufen ab GS und
      *    Praxisstufen P5 - P7 auf Kreditkarte angegeben wird (J/N)
           10 DRKI1413-1-VB-BERECHTIGT        PIC X(01).
           10 DRKI1413-1-VB-Anrede            PIC X(01).
           10 DRKI1413-1-TEL-gesch            PIC X(24).
           10 DRKI1413-1-TEL-mobil            PIC X(24).
           10 DRKI1413-1-FAX                  PIC X(24).
           10 DRKI1413-34F-GEWO-FLAG          PIC X(01).
           10 DRKI1413-34C-GEWO-FLAG          PIC X(01).
           10 DRKI1413-34I-GEWO-FLAG          PIC X(01).
           10 DRKI1413-34D-GEWO-FLAG          PIC X(01).
           10 DRKI1413-34D-GEWO-REGNR         PIC X(15).
           10 DRKI1413-34F-GEWO-REGNR         PIC X(15).
           10 DRKI1413-34I-GEWO-REGNR         PIC X(15).
      *    10 DRKI1413-VB-NATIONALITAET       PIC X(03).
      *    10 DRKI1413-ADR-NAME1              PIC X(35).
      *    10 DRKI1413-ADR-NAME2              PIC X(35).
      *    10 DRKI1413-ADR-NAME3              PIC X(35).
           10 DRKI1413-ADR-STR                PIC X(35).
           10 DRKI1413-ADR-HNR                PIC X(12).
           10 DRKI1413-ADR-PLZ                PIC X(10).
           10 DRKI1413-ADR-ORT                PIC X(35).
           10 DRKI1413-ADR-ISOCODE            PIC 9(03).
           10 DRKI1413-ADR-ISOLAND            PIC X(35).
      *    10 DRKI1413-1-FILLER               PIC X(59).
      *    10 DRKI1413-1-FILLER               PIC X(60).
      *    Filler war ein Byte zu kurz definiert und muss deswegen
      *    nicht geaendert werden
      *    10 DRKI1413-1-FILLER               PIC X(35).
           10 DRKI1413-1-FILLER               PIC X(157).
 
       01  DRKI1413-2-REC REDEFINES DRKI1413-REC.
           10 DRKI1413-2-VB-NR                PIC 9(07).
           10 DRKI1413-2-RECTYPE              PIC 9.
            88 DRKI1413-RECTYPE-2              VALUE 2.
           10 DRKI1413-2-UPDATE-FLAG          PIC 99.
           10 DRKI1413-2-VERTRAG-NR           PIC 9(10).
           10 REDEFINES DRKI1413-2-VERTRAG-NR.
             15 DRKI1413-BRANCH-NO            PIC 9(03).
             15 DRKI1413-ACCOUNT-NO-ROOT      PIC 9(07).
      *    10 DRKI1413-2-FILLER               PIC X(180).
           10 DRKI1413-2-FILLER               PIC X(480).
 
       01  DRKI1413-9-REC REDEFINES DRKI1413-REC.
           10 DRKI1413-9-VB-NR                PIC 9(07).
            88 DRKI1413-9-AGENT-ID-ZERO        VALUE 9999999.
           10 DRKI1413-9-RECTYPE              PIC 9.
            88 DRKI1413-RecType-9              VALUE 9.
           10 DRKI1413-9-Rec-Count            PIC 9(9).
      *    10 DRKI1413-9-FILLER               PIC X(183).
           10 DRKI1413-9-FILLER               PIC X(483).
 
 
 
 
 
 

