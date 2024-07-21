      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0548.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0548.CPv  $
      *
      *   Rev 2.3   Sep 21 2018 12:17:24   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.2   Apr 07 2016 10:11:44   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   09 May 2001 09:19:48   T13EX25
      *APA00132; Wert-2 aus den Konditionen neu
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 10:33:36   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   01 Oct 1997 11:29:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0548                                      *
      *    --------------------------------------------------------   *
      *    DATEN FÜR KONDITION ZUR EINHEITENERMITTLUNG                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                    *
      *    DATUM:    13.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.10.97! 01 ! NEU                       ! JÄGER    *
      * APA00132!07.05.01! 01 ! NEUES ATTRIBUT EINFÜGEN   ! JÄGER    *
      * APA02220!10.03.16!2.2 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02548!26.07.18!2.3 ! PRKO-NFOE-STZ             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE KONDITION ZUR       *
      *    GESAMTPROVISION.                                          *
      *                                                              *
      ****************************************************************
      *
             30 KOND-DATEN-EH.
                35 PRKO-KOND-NR        PIC S9(004)       BINARY.
                35 KNR-KOND-REGEL-EH   PIC S9(004)       BINARY.
100316          35 PRKO-VWF-STZ        PIC S9(003)V9(03) PACKED-DECIMAL.
100316          35 PRKO-VTF-STZ        PIC S9(004)V9(04) PACKED-DECIMAL.
100316          35 PRKO-AVERM-STZ      PIC S9(004)V9(04) PACKED-DECIMAL.
100316          35 PRKO-VM-STZ         PIC S9(004)V9(04) PACKED-DECIMAL.
100316          35 PRKO-KD-BETR-STZ    PIC S9(004)V9(04) PACKED-DECIMAL.
100316          35 PRKO-REST-STRK-STZ  PIC S9(004)V9(04) PACKED-DECIMAL.
260718          35 PRKO-NFOE-STZ       PIC S9(004)V9(04) PACKED-DECIMAL.
                35 PRKO-EH-PROV-WERT   PIC S9(009)       BINARY.
                35 PRKO-EH-PRO-RATA-BERECHNUNG
                                       PIC S9(009)       BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
 

