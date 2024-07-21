      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0114.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0114.CPv  $
      *
      *   Rev 2.1   Sep 11 2003 16:10:26   t13ae34
      *AAN00369 - hier: Zugriff auf Konditionen GP über KTY=210
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   08 Aug 1998 12:58:44   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 07 1997 09:17:50   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:18:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0114  KONSTANTEN FÜR                     *
      *                           Konditionsart            KTY = 104 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    10.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AFPA0012!03.09.97! 02 ! Umstellung der KNR-Nummern! Auerbach *
      *         !        !    ! entsprechend Konditions-  !          *
      *         !        !    ! konzept in DAISY          !          *
      * AFPA00??!06.10.97!    ! Anpassungen an Namenskon- ! Storch   *
      *         !        !    ! ventionen                 !          *
      * AAN00369!11.09.03!2.1 ! K-KNR-KOND-ART-GP-EING    ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * Erklärung zum Aufbau der KNR:                                *
      * -----------------------------                                *
      * - im Bereich der Konditionen bilden wir mit Hilfe der KTY/KNR*
      *   die fachliche Hierarchie ab                                *
      * - Bei den Konditionen (KTY=104) werden unterschiedliche      *
      *   Konditionsarten (KNR-KOND-ART) unterschieden               *
      *   unterschieden.                                             *
      * - Zu jeder Konditionsart (KNR-KOND-ART) gibt es Konditions-  *
      *   regeln (z.B. KTY=119).                                     *
      * - Um diesen Zusammenhang abzubilden, werden die KNR der      *
      *   Konditionsarten so gewählt, daß sie zu den KTY der         *
      *   Konditionsregeln passen.                                   *
      *                                                              *
      ****************************************************************
       01  FILLER.
           05 K-KNR-KOND-ART-GRUND-PROV  PIC S9(004) BINARY SYNC
                                                     VALUE  119.
061097*    05 K-KNR-KOND-ART-GES-PROV    PIC S9(004) BINARY SYNC
061097     05 K-KNR-KOND-ART-GSMT-PROV   PIC S9(004) BINARY SYNC
                                                     VALUE  120.
061097*    05 K-KNR-KOND-ART-EINHEITEN   PIC S9(004) BINARY SYNC
061097     05 K-KNR-KOND-ART-EINH        PIC S9(004) BINARY SYNC
                                                     VALUE  121.
           05 K-KNR-KOND-ART-KD-BETR     PIC S9(004) BINARY SYNC
                                                     VALUE  122.
           05 K-KNR-KOND-ART-EE-PRUEFUNG PIC S9(004) BINARY SYNC
                                                     VALUE  123.
061097*    05 K-KNR-KOND-ART-AP-FP       PIC S9(004) BINARY SYNC
061097     05 K-KNR-KOND-ART-AFP         PIC S9(004) BINARY SYNC
                                                     VALUE  124.
           05 K-KNR-KOND-ART-HAFTUNG     PIC S9(004) BINARY SYNC
                                                     VALUE  125.
110903     05 K-KNR-KOND-ART-GP-EING     PIC S9(004) BINARY SYNC
110903                                               VALUE  210.
