      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0573.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0573.CPv  $
      *
      *   Rev 2.1   Apr 07 2016 10:12:10   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:40:10   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   22 Oct 1997 13:12:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0573  NESTED-COPY-STRECKE FÜR            *
      *                "KONDITION FÜR ERSATZ- UND ERHÖHUNGSPRÜFUNG"  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    24.10.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!24.10.97! 01 !NEU                        !BOHLINGER *
      * APA02220!10.03.16!2.1 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0573                                 *
      *                                                              *
      ****************************************************************

             30 KOND-DATEN-EEP.
                35 PRKO-KOND-NR         PIC S9(004)  BINARY.
                35 KNR-KOND-REGEL-EEP   PIC S9(004)  BINARY.
100316*         35 PRKO-PROZ-MPRAEMIE   PIC S9(003)V999 PACKED-DECIMAL.
100316          35 PRKO-PROZ-MPRAEMIE   PIC S9(004)V9(4) PACKED-DECIMAL.
                35 PRKO-EE-MIND-LZ      PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************

