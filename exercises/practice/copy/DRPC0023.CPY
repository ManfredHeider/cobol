      * T13AE19   Stand: 12/06/96    13:06:41
      * T13AE19   Stand: 11/06/96    16:46:42
      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPC0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPC0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:48:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Sep 19 1996 16:19:56   T13AE19
      *erste uebergabe, satzaufbau der tabelle TEG
      *
      *   Rev 1.0   Jun 03 1996 13:42:46   T13AE19
      *Initial revision.
      *
      *****  VERSION
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER UMSETZEN EINGEREICHTES GESCHÄFT UM15 (UM4009)   *
      *              IN TABELLE (TEG) FÜR DIE REPLIKATION NACH UM16  *
      *                                                              *
      ****************************************************************
      * ERSTELLT AM : 10.06.1996                                     *
      *         VON : ANDREAS BORK                                   *
      *                                                              *
      *                                                              *
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AFPC....!10.06.96! NEU                       ! BORK          *
      *         !        !                           !               *
      *         !        !                           !               *
      ****************************************************************
       01  DRPC0023-TEG-SATZ.
           05 DRPC0023-MA-ST-NR              PIC S9(09) COMP.
           05 DRPC0023-TEG-STAT-DAT-VON      PIC  X(10).
           05 DRPC0023-KNR-PROD-GB-GES       PIC S9(04) COMP.
           05 DRPC0023-PRPS-PROD-SCHL        PIC S9(04) COMP.
           05 DRPC0023-MS-NR                 PIC S9(04) COMP.
           05 DRPC0023-TEG-BETR-NR           PIC S9(09) COMP.
           05 DRPC0023-TEG-TAGES-DAT         PIC  X(10).
           05 DRPC0023-KTY-PROD-GB-GES       PIC S9(04) COMP.
           05 DRPC0023-KNR-ERGEBNIS-TYP      PIC S9(04) COMP.
           05 DRPC0023-KTY-ERGEBNIS-TYP      PIC S9(04) COMP.
           05 DRPC0023-TEG-EH-NT-EG          PIC S9(10)V9 COMP-3.
           05 DRPC0023-TEG-ANZ-AN-EG         PIC S9(04) COMP.
           05 DRPC0023-TEG-VS-SUM-EG         PIC S9(11) COMP-3.
           05 DRPC0023-TEG-BEITRAG-EG        PIC S9(09)V99 COMP-3.
           05 DRPC0023-KNR-WAEHRUNG          PIC S9(04) COMP.
           05 DRPC0023-KTY-WAEHRUNG          PIC S9(04) COMP.
           05 DRPC0023-TEG-EH-NT-GR          PIC S9(10)V9 COMP-3.
           05 DRPC0023-TEG-ANZ-AN-GR         PIC S9(04) COMP.
           05 DRPC0023-TEG-VS-SUM-GR         PIC S9(11) COMP-3.
           05 DRPC0023-TEG-BEITRAG-GR        PIC S9(09)V99 COMP-3.
