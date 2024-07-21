      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0554.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0554.CPv  $
      *
      *   Rev 2.2   Apr 07 2016 10:11:52   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   Aug 12 2003 09:43:14   t13ae46
      *APA00245:Namensänderung STZ in STZ1,
      *         PRKO-KD-BETR-STZ2 hinzu
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   04 Aug 1998 14:15:08   T13EX25
      *APA00032 - Neuerstellung
      *
      *   Rev 1.0   01 Oct 1997 11:31:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0554                                      *
      *    --------------------------------------------------------   *
      *    DATEN FÜR KONDITION ZUM KD-BETR                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                    *
      *    DATUM:    20.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!20.10.97!01.01! NEU                       ! JÄGER   *
      * AFPA0012!30.10.97!01.01! NAMENSÄNDERUNG            ! DORSCH  *
      *         !        !     ! PRKO-KDB-STZ IN PRKO-KD-BETR_STZ    *
      * APA00245!23.07.03! 2.1 ! NAMENSÄNDERUNG STZ IN STZ1! WANG    *
      *         !        !     ! PRKO-KD-BETR-STZ2 HINZU   !         *
      * APA02220!10.03.16! 2.2 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH  *
      *         !        !     !                           !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE KONDITION ZUR       *
      *    KUNDENBETREUUNGS-PROVISION                                *
      *                                                              *
      ****************************************************************
      *
             30 KOND-DATEN-KDB.
                35 PRKO-KOND-NR                     PIC S9(004) BINARY.
                35 KNR-KOND-REGEL-KDB               PIC S9(004) BINARY.
100316*         35 PRKO-KD-BETR-STZ1                PIC S9(003)V999
100316          35 PRKO-KD-BETR-STZ1                PIC S9(004)V9(4)
                                                        PACKED-DECIMAL.
100316*         35 PRKO-KD-BETR-STZ2                PIC S9(003)V999
100316          35 PRKO-KD-BETR-STZ2                PIC S9(004)V9(4)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

