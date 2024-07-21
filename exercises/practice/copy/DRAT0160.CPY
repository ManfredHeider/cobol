      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0160.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAT0160.CPv  $
      *
      *   Rev 2.9   Feb 03 2016 10:47:28   t13ex24
      *APA02169: Ges. 50 - Central neue PGVs zum Mein Gesundheitspl
      *an- KBP PEG
      *
      *   Rev 2.8   Jan 13 2016 11:50:26   t13ex24
      *APA02169: Ges. 50 - Central neue PGVs zum Mein Gesundheitspl
      *an- KBP PEG
      *
      *   Rev 2.7   Dec 21 2015 11:51:10   t13ex24
      *APA02169: Ges. 50 - Central neue PGVs zum Mein Gesundheitspl
      *an- KBP PEG
      *
      *   Rev 2.6   Jan 10 2014 12:19:22   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.5   Apr 20 2005 11:04:28   t13ae46
      *APA00346: Ges. 26 - Dynamikerhöhungen
      *
      *   Rev 2.4   04 Apr 2001 14:58:58   T13EX43
      *APA00119 : AP-Prodarten für Fondgesellschaften
      *
      *   Rev 2.3   31 Aug 2000 09:03:36   T13EX22
      *APA00105: PVW III
      *
      *   Rev 2.2   Apr 28 2000 08:49:50   T13EX22
      *APA00090: Ergänzungen für GEVO Gutschrift
      *
      *   Rev 2.1   May 29 1999 07:40:40   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   18 Aug 1997 10:27:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0160  KONSTANTEN FÜR     KTY = 124       *
      *                 Konditions-Regel AP/FP-Regelung              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! Auerbach *
      * APA00090!13.04.00! 02 ! NEUE KNR-REGEL FÜR BADENIA! MUND     *
      * APA00105!22.08.00! 03 ! NEUE KNR-REGEL FÜR III    ! MUND     *
      * APA00119!04.04.01!2.04! ABGRENZUNG AP HINZU       ! KLEEMEIER*
      * APA00346!12.04.01!2.05! NEUE KNR-REGEL            ! WANG     *
      * APA01526!24.05.13!2.06! NEUE KNR-REGEL            ! BÖTTCHER *
      * APA02168!30.11.15!2.07! NEUE KNR-REGEL            ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  REGEL                                                  *
      *  ----------------------------------                          *
      *    1  AP/FP ist vom Zeitraum abhängig                        *
      *    2  AP/FP ist von den Bewegungen abhängig                  *
      *       1.      Bewegung = AP                                  *
      *       2. - n. Bewegung = FP                                  *
      *                                                              *
      ****************************************************************
 
       01  FILLER.
           05 K-KNR-K-REGEL-AP-FP-ZEITRAUM   PIC S9(004) BINARY VALUE 1.
           05 K-KNR-K-REGEL-AP-FP-BEWEGUNG   PIC S9(004) BINARY VALUE 2.
130400     05 K-KNR-K-REGEL-AP-FP-IMMER-AP   PIC S9(004) BINARY VALUE 3.
220800     05 K-KNR-K-REGEL-AP-FP-ZEITPUNKT  PIC S9(004) BINARY VALUE 4.
040401     05 K-KNR-K-REGEL-AP-FP-ABGRENZ-AP PIC S9(004) BINARY VALUE 5.
120405     05 K-KNR-K-REGEL-AP-FP-DYNAMIK    PIC S9(004) BINARY VALUE 6.
250513*    Provision ist immer AP mit Strukturbereinigung bei Dynamik
250513     05 K-KNR-K-REGEL-AP-FP-STRUKT-DYN PIC S9(004) BINARY VALUE 7.
301115     05 K-KNR-K-REGEL-AP-FP-IMMER-FP   PIC S9(004) BINARY VALUE 8.
 

